function [SIARCToutput, runtime, FRT] = SIARCT(D, r, compactThresh,...
    cardinaThresh, regionType)

% Copyright 2012 Tom Collins

% Given a dataset D, this function returns compact subsets of maximal
% translatable patterns (MTP) that occur in D, and for which a conjugate
% generating vector lies on or within the first r superdiagonals of the
% similarity array for D. The runtime and fifth return time (FRT, time
% taken for SIARCToutput to reach size 5) are output also.

% INPUT
%  D is a point set, assumed to be in lexicographic order. Otherwise
%   D = sortrows(D);
%   can be used to achieve this. Alternatively,
%   D = unique(D, 'rows');
%   will also remove duplicate datapoints.
%  r is the number of superdiagonals used by SIAR.
%  compactThresh is the compactness threshold. Patterns with compactness
%   greater than this value (in [0, 1]) are returned.
%  cardinaThresh is the points threshold. Patterns with at least this many
%   points in one occurrence are returned.
%  regionType is a string ('lexicographic' or 'convex hull', indicating
%   which region type should be used by the function compactSubpatterns.

% EXAMPLE
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% r = 1;
% compactThresh = .75;
% cardinaThresh = 4;
% regionType = 'lexicographic';
% [SIARCToutput, runtime, FRT] = SIARCT(D, r, compactThresh,...
%     cardinaThresh, regionType);

tStart = tic;
n = size(D,1);
d = size(D,2);
if r >= n
    r = n-1;
end
N = n*(n-1)/2;

% Calculate the reduced similarity matrix, but leave it as a vector.
% tic
V = zeros(N,d+1);
L = 1;
for i = 1:r
    for j = i+1:n
        V(L,:) = [D(j,:)-D(j-i,:) j-i];
        L = L+1;
    end
end
V = V(1:L-1,:);
V = sortrows(V);
% toc

% Parse V and define a new set of datapoints each time a new difference
% vector is encountered. Store the results as a struct, W.
% tic
W = repmat(struct('vector',[],'datapoints',[]),1,L-1);
% W = repmat(struct('vector',[],'datapoints',[], 'indices',[]),1,L-1);
i = 1; % Increment over V.
j = 1; % Increment over W.
W(j).vector = V(i,1:d);
W(j).datapoints = D(V(i,d+1),:);
% W(j).indices = V(i,d+1);
for i = 2:L-1
    % Did difference between datapoints result in the same vector?
    if V(i,1:d) == V(i-1,1:d)
        % Yes: include datapoint in existing set.
        W(j).datapoints = [W(j).datapoints; D(V(i,d+1),:)];
        % W(j).indices = [W(j).indices; V(i,d+1)];
    else
        % No: define a new set of datapoints.
        W(j+1).vector = V(i,1:d);
        W(j+1).datapoints = D(V(i,d+1),:);
        % W(j+1).indices = V(i,d+1);
        j=j+1;
    end
end
W = W(1:j);
% toc

% Parse W and run SIA on an element of W if it contains more than one
% datapoint, recording the results in one list L.
% tic
L = zeros(N,d);
s = 1; % Increment over L.
t = 1; % Increment over W.
T = j; % Length of W.
while t <= T
    nWt = size(W(t).datapoints,1);
    if nWt > 1
        for i = 1:nWt
            for j = i+1:nWt
                L(s,:) = W(t).datapoints(j,:)-W(t).datapoints(i,:);
                s = s+1;
            end
        end
    end
    t=t+1;
end
L = L(1:s-1,:);
% toc

% tic
% The more numerous the vector in L, the more likely it is to correspond to
% a salient MTP. So we count the vectors in L and then sort them by number
% of occurrences. The number of occurrences is not retained.
M = count(L, 'rows');
M = [M(:, d+1) M(:, 1:d)];
M = unique(M, 'rows');
M = flipud(M(:, 2:d+1));
nM = size(M,1);
% toc

% For each element w of the list M, calculate the maximal translatable
% pattern.
% tic

% These three lines seemed to be generating an error, but I have no idea
% why, as the corresponding code from SIACT does not generate an error.
% SIARCToutput = repmat(struct('pattern', [], 'indices', [],...
%     'cardinality', [], 'compactness', [], 'region', [], 'span', [],...
%     'regionType', [], 'vector', []), 1, nM);
i = 1; % Increment over L.
j = 1; % Increment over SIARCToutput.
FRT = [];
fifthMeasure = 1; % Logical for measuring fifth return time (FRT).
while i <= nM
    if fifthMeasure && j >= 5
        % Measure the 5th return time.
        FRT = toc(tStart);
        fifthMeasure = 0;
    end
    v = M(i,:);
    [P, I] = maximalTranslatablePattern(v, D, d, n);
    % Check the MTP for compact subpatterns.
    S = compactSubpatterns(P, D, d, compactThresh, cardinaThresh,...
        regionType);
    if isfield(S,'pattern');
        % Include the generating vector of the MTP.
        [S(:).vector] = deal(v);
        % Indices default to empty at present. Could be improved.
        [S(:).indices] = deal([]);
        % Append compact subpatterns to SIACT output.
        k = size(S,2);
        SIARCToutput(j:j+k-1) = S;
        j=j+k;
    end
    i=i+1;
end
runtime = toc(tStart);
% toc

end
