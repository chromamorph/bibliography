function [P translators D v] = pattern_shape(l, m, L1, L2, plotData)

% Copyright 2012 Tom Collins

% Returns a two-dimensional set of points representing a shape. Each
% coordinate is an integer greater than or equal to 1 and less than or
% equal to L1. Also returned are at most m randomly translated copies of
% the set, satisfying a corresponding 1 <= integer <= L2 criterion.

% INPUT
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
% l = 15;
% m = 4;
% L1 = 30;
% L2 = 500;
% plotData = 1;
% [P translators D v] = pattern_shape(l, m, L1, L2, plotData);

% The last radius here is linked to the maximum possible value of
% amplitude, so that the minimum integer of a shape point is 1.
radii = [L1/4 L1/3 L1/2-3];
radius = radii(randsample(3, 1));
freq = randsample(5, 2)';
amp = randsample(4, 2)'/2;
phi = 2*pi*rand(1, 2);
[x y] = fourier_shape(l, radius, freq, amp, phi);
P = unique([round(x + L1/2)' round(y + L1/2)'], 'rows');
% plot(P(:,1), P(:,2), '.k');
l = size(P, 1);

% Find its max in each dimension, so that translating it does not lead to
% points beyond the integer grid.
maxP = max(P);

% Generate random translators. Again, there is a very small probability
% that one of the translator dimensions contains an integer one too big,
% so we search for it and replace with L2 - maxP(col) if occurrences are
% found.
translators = zeros(m, 2);
for col = 1:2
    translators(:, col) = floor((L2 - maxP(col) + 1).*rand(m, 1));
end
for col = 1:2
    problem_trans_idxs = find(translators(:, col) >= L2-maxP(col)+1);
    if problem_trans_idxs
        translators(problem_trans_idxs, col) = L2 - maxP(col);
    end
end
translators = unique(translators, 'rows');
m = size(translators, 1);

D = nan(l*m, 2);
v = [];
for transi = 1:m
    D(l*(transi-1)+1:l*transi, :)...
        = P + repmat(translators(transi, :), l, 1);
    if max(max(D)) > L2 || min(min(D)) < 1
        v = transi;
    end
end

if plotData
    colours = {'b', 'g', 'r', 'c', 'm', 'y', 'k'};
    hold on
    for transi = 1:m
        plot(P(:,1) + translators(transi, 1),...
            P(:,2) + translators(transi, 2),...
            ['.' colours{mod(transi - 1, 7) + 1}]);
    end
    grid
    hold off
end

end
