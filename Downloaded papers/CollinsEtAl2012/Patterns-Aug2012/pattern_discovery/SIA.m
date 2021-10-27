function [SIAoutput, runtime, FRT] = SIA(D)

% Copyright 2012 Tom Collins

% Given a dataset D, this function returns the maximal translatable
% patterns (MTP) that occur in D, along with their generating vectors, as a
% struct. It is an implementation of the structural induction algorithm
% (SIA) as described by Meredith, Lemstrom, and Wiggins (2002). The runtime
% and fifth return time (FRT, time taken for SIA output to reach size 5)
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

% EXAMPLE
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% [SIAoutput, runtime, FRT] = SIA(D);

tStart = tic;
n = size(D,1);
k = size(D,2);
N = n*(n-1)/2;

% Calculate the similarity matrix, but leave it as a vector.
% tic
V = zeros(N,k+1);
L = 1;
for i = 1:n-1
    for j = i+1:n
        V(L,:) = [D(j,:)-D(i,:) i];
        L = L+1;
    end
end
V = sortrows(V);
% toc

% Parse V and define a new MTP each time a new difference vector is
% encountered. Store the results as a struct.
% tic
SIAoutput = repmat(struct('vector',[],'pattern',[], 'indices',[]),1,...
    L-1);
i = 1; % Increment over V.
j = 1; % Increment over SIAoutput.
SIAoutput(j).vector = V(i,1:k);
SIAoutput(j).pattern = D(V(i,k+1),:);
SIAoutput(j).indices = V(i,k+1);
for i = 2:N
    % Did difference between datapoints result in the same vector?
    if V(i,1:k) == V(i-1,1:k)
        % Yes: include datapoint in existing maximal translatable pattern.
        SIAoutput(j).pattern = [SIAoutput(j).pattern; D(V(i,k+1),:)];
        SIAoutput(j).indices = [SIAoutput(j).indices; V(i,k+1)];
    else
        % No: define a new maximal translatable pattern.
        SIAoutput(j+1).vector = V(i,1:k);
        SIAoutput(j+1).pattern = D(V(i,k+1),:);
        SIAoutput(j+1).indices = V(i,k+1);
        j=j+1;
    end
end
SIAoutput = SIAoutput(1:j);
% toc
runtime = toc(tStart);
FRT = runtime;

end
