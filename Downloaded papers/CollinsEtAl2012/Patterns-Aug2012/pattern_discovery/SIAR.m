function [SIARoutput, runtime, FRT] = SIAR(D, r)

% Copyright 2012 Tom Collins

% Given a dataset D, this function returns maximal translatable patterns
% (MTP) that occur in D, and for which a conjugate generating vector lies
% on or within the first r superdiagonals of the similarity array for D.
% As such, with r = 1 or small, my structure induction algorithm for r
% superdiagonals is more precise than the structural induction algorithm
% (SIA) as described by Meredith, Lemstrom, and Wiggins (2002). The runtime
% and fifth return time (FRT, time taken for SIARoutput to reach size 5)
% are output also.

% REFERENCE
%  David Meredith, Kjell Lemstrom, and Geraint A. Wiggins. Algorithms for
%   discovering repeated patterns in multidimensional representations of
%   polyphonic music. Journal of New Music Research, 31(4) (2002), 321-345.

% INPUT
%  D is a point set. It is assumed that D is in lexicographic order.
%   Otherwise
%   D = sortrows(D);
%   can be used to achieve this. Alternatively,
%   D = unique(D, 'rows');
%   will also remove duplicate datapoints.
%  r is the number of superdiagonals for which difference vectors are
%   calculated.

% EXAMPLE
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% r = 1;
% [SIARoutput, runtime, FRT] = SIAR(D, r);

tStart = tic;
n = size(D,1);
k = size(D,2);
if r >= n
    r = n-1;
end
N = n*(n-1)/2;

% Calculate the reduced similarity matrix, but leave it as a vector.
% tic
V = zeros(N,k+1);
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
W(j).vector = V(i,1:k);
W(j).datapoints = D(V(i,k+1),:);
% W(j).indices = V(i,k+1);
for i = 2:L-1
    % Did difference between datapoints result in the same vector?
    if V(i,1:k) == V(i-1,1:k)
        % Yes: include datapoint in existing set.
        W(j).datapoints = [W(j).datapoints; D(V(i,k+1),:)];
        % W(j).indices = [W(j).indices; V(i,k+1)];
    else
        % No: define a new set of datapoints.
        W(j+1).vector = V(i,1:k);
        W(j+1).datapoints = D(V(i,k+1),:);
        % W(j+1).indices = V(i,k+1);
        j=j+1;
    end
end
W = W(1:j);
% toc

% Parse W and run SIA on an element of W if it contains more than one
% datapoint, recording the results in one list L.
% tic
L = zeros(N,k);
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
M = [M(:, k+1) M(:, 1:k)];
M = unique(M, 'rows');
M = flipud(M(:, 2:k+1));
nM = size(M,1);
% toc

% For each element w of the list M, calculate the maximal translatable
% pattern.
% tic
SIARoutput = repmat(struct('vector',[],'pattern',[], 'indices',[]),1,nM);
FRT = [];
i = 1; % Increment over L.
while i <= nM
    if i == 5
        % Measure the 5th return time.
        FRT = toc(tStart);
    end
    v = M(i,:);
    SIARoutput(i).vector = v;
    [P, I] = maximalTranslatablePattern(v, D, k, n);
    SIARoutput(i).pattern = P;
    SIARoutput(i).indices = I;
    i=i+1;
end
runtime = toc(tStart);
% toc

end
