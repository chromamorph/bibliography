function logp = empiricalLogProbability(S, cardinality, dimension,...
    empiricalMass)

% Copyright 2012 Tom Collins

% This function takes a matrix S consisting of row vectors, which is the
% result of stripping some pattern P of its ontime values. The cardinality
% of S is usually known when calling this function, so it is a required
% argument. The third argument specifies a dimension of S over which
% probabilities are calculated. The fourth argument is an empirical mass
% function in which rows of S appear with relative frequencies p1, p2,...,
% pn. The sum of the natural logarithms of these probabilities is returned.

% INPUT
%  S is a matrix, the result of stripping some pattern P of its ontime
%   values.
%  cardinality is the number of rows in S.
%  dimension is the number of columns in S.
%  empiricalMass is a matrix, output by the function count, which contains
%   the relative frequencies for rows of S in the dataset.

% EXAMPLE
% S = [71 1/2; 70 1/2; 68 1/2];
% empiricalMass = [62 1 1 1/16; 65 1/2 1 1/16; 65 1 1 1/16;...
%     68 1/2 2 1/8; 70 1/2 2 1/8; 70 3/2 2 1/8; 71 1/2 3 3/16;...
%     73 1 2 1/16; 75 1/2 2 1/16];
% logp = empiricalLogProbability(S, size(S, 1), size(S, 2), empiricalMass);

m = dimension;
% Empirical mass without the count and relative frequency data.
X = empiricalMass(:,1:m);

logp = 0;
i = 1;
while i <= cardinality
    % Is the row of S in the empirical mass?
    [tf, loc] = ismember(S(i,:), X, 'rows');
    if tf == 0
        logp = -Inf;
        i = cardinality;
    else
        logp = logp + log(empiricalMass(loc,m+2));
    end
    i=i+1;
end

end
