function [P translators] = pattern_scatter(k, l, m, L1, L2, plotData)

% Copyright 2012 Tom Collins

% Returns a k-dimensional set of at most l points, a subset of integer
% k-tuples greater than or equal to 1 and less than or equal to L1. Also
% returned are at most m randomly translated copies of the set, satisfying
% a corresponding 1 <= integer <= L2 criterion.

% INPUT
%  k is a scalar specifying the dimension of the dataset.
%  l is the cardinality of the set to be returned.
%  m is the number of translated copies to be created (including the
%   original).
%  L1 is the size of the integer grid for the subset.
%  L2 is the size of the integer grid containing all translations.
%  plotData takes the value one when the pattern should be plotted, and
%   zero otherwise.

% EXAMPLE
% s = RandStream('mt19937ar','Seed',1);
% RandStream.setDefaultStream(s);
% k = 3;
% l = 20;
% m = 30;
% L1 = 20;
% L2 = 1000;
% plotData = 1;
% [P translators] = pattern_scatter(k, l, m, L1, L2, plotData);

% Generate random pattern. An l x k matrix of 1 <= integers <= L1 is
% returned. There is a very small probability that the integer L1 + 1
% occurs in this matrix, so we search for it and replace with L1 if any
% occurrences are found.
P = floor(1 + L1.*rand(l, k));
problem_patt_idxs = find(P >= L1+1);
if problem_patt_idxs
   P(problem_patt_idxs) = L1;
end
P = unique(P, 'rows');
% l = size(P, 1);

% Find its max in each dimension, so that translating it does not lead to
% points beyond the integer grid.
maxP = max(P);

% Generate random translators. Again, there is a very small probability
% that one of the translator dimensions contains an integer one too big,
% so we search for it and replace with L2 - maxP(col) if occurrences are
% found.
translators = zeros(m, k);
for col = 1:k
    translators(:, col) = floor((L2 - maxP(col) + 1).*rand(m, 1));
end
for col = 1:k
    problem_trans_idxs = find(translators(:, col) >= L2-maxP(col)+1);
    if problem_trans_idxs
        translators(problem_trans_idxs, col) = L2 - maxP(col);
    end
end
translators = unique(translators, 'rows');
m = size(translators, 1);

if plotData
    colours = {'b', 'g', 'r', 'c', 'm', 'y', 'k'};
    if k == 2 % 2D plot.
        hold on
        for transi = 1:m
            plot(P(:,1) + translators(transi, 1),...
                P(:,2) + translators(transi, 2),...
                ['.' colours{mod(transi - 1, 7) + 1}]);
        end
        grid
        hold off
    end
    if k == 3 % 3D plot.
        hold on
        for transi = 1:m
            plot3(P(:,1) + translators(transi, 1),...
                P(:,2) + translators(transi, 2),...
                P(:,3) + translators(transi, 3),...
                ['.' colours{mod(transi - 1, 7) + 1}]);
        end
        grid
        hold off
    end
end

end
