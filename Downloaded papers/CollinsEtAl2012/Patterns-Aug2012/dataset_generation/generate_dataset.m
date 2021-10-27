function datasetStruct = generate_dataset(k, n, L, G, SNR, plotData, fName)

% Copyright 2012 Tom Collins

% Returns a k-dimensional set of n points, a subset of integer k-tuples
% greater than or equal to 1 and less than or equal to L. The set contains
% translated copies of smaller subsets, and is augmented to cardinality n
% by random selection.

% INPUT
%  k is a scalar specifying the dimension of the dataset.
%  n is the cardinality of the set to be returned.
%  L is the size of the integer grid.
%  G is a string specifying the generation method (either 'scatter' or
%   'shape').
%  SNR is a string specifiying whether the signal-to-noise ratio should
%   remain 'constant' or 'increasing' with n.
%  plotData takes the value one when the pattern should be plotted, and
%   zero otherwise.
%  fName is a string containing the path of where the datasetStruct should
%   be saved. A filename including date stamp is generated automatically.
%   If empty the datasetStruct is not saved.

% EXAMPLE
% s = RandStream('mt19937ar','Seed',1);
% RandStream.setDefaultStream(s);
% k = 2;
% n = 1000;
% L = 1000;
% G = 'scatter';
% SNR = 'decreasing';
% plotData = 1;
% fName = [];
% % Press a button to see the annotated scatter patterns.
% datasetStruct = generate_dataset(k, n, L, G, SNR, plotData, fName);

% Number of artificially created patterns.
if strcmp(SNR, 'constant')
    M = binornd(round(n/40), 0.5);
else
    M = binornd(25, 0.5);
end
points = zeros(n, k);
ptsi = 1; % Increment to fill points.
details = struct([]);
for patti = 1:M
    % Cardinality of pattern.
    l = max(binornd(20, 0.5), 10);
    % Number of translators.
    m = max(poissrnd(5), 2);
    % Size of the integer grid for the subset.
    L1 = 30;
    if strcmp(G, 'shape')
        if k ~= 2
            warning(['Warning in generate_dataset.m at line 58: The'...
                ' function pattern_shape is called with dimension'...
                ' parameter k ~= 2.']);
        end
        [P translators] = pattern_shape(l, m, L1, L, 0);
    else
        [P translators] = pattern_scatter(k, l, m, L1, L, 0);
    end
    l = size(P, 1);
    m = size(translators, 1);
    details(patti).pattern = P;
    details(patti).translators = translators;
    details(patti).cardinality = l;
    details(patti).occurrences = m;
    if details(patti).occurrences == 1
        error(['Error in generate_dataset.m at line 63: Only one'...
            ' translator was defined for this pattern.']);
    end
    for transi = 1:m
        points(ptsi:ptsi+l-1, :) =...
            P + repmat(translators(transi, :), l, 1);
        ptsi = ptsi + l;
    end
end
% Determine how many unique points there are in the variable points.
ptsn = ptsi - 1;
points = points(1:ptsn, :);
pointsOnly = unique(points, 'rows');
pointsAugm = pointsOnly;
if ptsn > n
    warning(['Error in generate_dataset.m at line 83: Dataset'...
        ' cardinality exceeds specified parameter. Trying again...']);
    
    %% Trying once more only. Otherwise should consider resetting params.
    
    % Number of artificially created patterns.
    if strcmp(SNR, 'constant')
        M = binornd(round(n/40), 0.5);
    else
        M = binornd(25, 0.5);
    end
    points = zeros(n, k);
    ptsi = 1; % Increment to fill points.
    details = struct([]);
    for patti = 1:M
        % Cardinality of pattern.
        l = max(binornd(20, 0.5), 10);
        % Number of translators.
        m = max(poissrnd(5), 2);
        % Size of the integer grid for the subset.
        L1 = 30;
        if strcmp(G, 'shape')
            if k ~= 2
                warning(['Warning in generate_dataset.m at line 111:'...
                    ' The function pattern_shape is called with'...
                    ' dimension parameter k ~= 2.']);
            end
            [P translators] = pattern_shape(l, m, L1, L, 0);
        else
            [P translators] = pattern_scatter(k, l, m, L1, L, 0);
        end
        l = size(P, 1);
        m = size(translators, 1);
        details(patti).pattern = P;
        details(patti).translators = translators;
        details(patti).cardinality = l;
        details(patti).occurrences = m;
        if details(patti).occurrences == 1
            error(['Error in generate_dataset.m at line 122: Only one'...
                ' translator was defined for this pattern.']);
        end
        for transi = 1:m
            points(ptsi:ptsi+l-1, :) =...
                P + repmat(translators(transi, :), l, 1);
            ptsi = ptsi + l;
        end
    end
    % Determine how many unique points there are in the variable points.
    ptsn = ptsi - 1;
    points = points(1:ptsn, :);
    pointsOnly = unique(points, 'rows');
    pointsAugm = pointsOnly;
    
    %%
    
    if ptsn > n
        error(['Error in generate_dataset.m at line 139: Dataset'...
            ' cardinality exceeds specified parameter.']);
    end
else % Augment the dataset with randomly-generated points.
    ptsi = size(pointsAugm, 1) + 1;
    while ptsi <= n
        % There is a very small probability that the integer L + 1 occurs
        % in this matrix, so we search for it and replace with L1 if any
        % occurrences are found.
        probe = floor(1 + L.*rand(1, k));
        problem_idxs = find(probe >= L+1);
        if problem_idxs
            P(problem_idxs) = L;
        end
        ptsj = 1;
        matched = 0;
        while ptsj < size(pointsAugm, 1) && matched == 0
            if sum(probe == pointsAugm(ptsj,:)) == k
                matched = 1;
            end
            ptsj = ptsj + 1;
        end
        if ~matched
            pointsAugm(ptsi, :) = probe;
            ptsi = ptsi + 1;
        end
    end
end

% Define, return, and possibly save the datasetStruct variable.
datasetStruct = struct();
datasetStruct.ID = [randsample('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 5, true) ...
    '-' datestr(clock, 30)];
datasetStruct.k = k;
datasetStruct.n = n;
datasetStruct.L = L;
datasetStruct.G = G;
datasetStruct.SNR = SNR;
datasetStruct.pointsOnly = pointsOnly;
datasetStruct.pointsAugm = unique(pointsAugm, 'rows');
datasetStruct.details = details;
datasetStruct.targetn = size(details, 2);
if fName
    save(fullfile(fName, [datasetStruct.ID '.mat']), 'datasetStruct');
    fprintf('Saved dataset %s to file.\n', datasetStruct.ID);
end

if plotData
    colours = {'b', 'g', 'r', 'c', 'm', 'y'};
    if k == 2 % 2D plot.
        for patti = 1:M
            hold on
            plot(pointsAugm(:, 1), pointsAugm(:, 2), '.k');
            grid on
            for transi = 1:details(patti).occurrences
                if patti == 1 && transi == 1
                    waitforbuttonpress
                end
                pause(0.5)
                plot(details(patti).pattern(:,1) +...
                    details(patti).translators(transi, 1),...
                    details(patti).pattern(:,2) +...
                    details(patti).translators(transi, 2),...
                    ['.' colours{mod(patti-1, 6) + 1}]);
                if transi == details(patti).occurrences
                    pause(1.3)
                else
                    pause(0.4)
                end
            end
            hold off
            if patti < M
                grid off
            end
        end
    end
    if k == 3 % 3D plot.
        for patti = 1:M
            hold on
            plot3(pointsAugm(:, 1), pointsAugm(:, 2), pointsAugm(:, 3),...
                '.k');
            grid on
            for transi = 1:details(patti).occurrences
                if patti == 1 && transi == 1
                    waitforbuttonpress
                end
                pause(0.5)
                plot3(details(patti).pattern(:,1) +...
                    details(patti).translators(transi, 1),...
                    details(patti).pattern(:,2) +...
                    details(patti).translators(transi, 2),...
                    details(patti).pattern(:,3) +...
                    details(patti).translators(transi, 3),...
                    ['.' colours{mod(patti-1, 6) + 1}]);
                if transi == details(patti).occurrences
                    pause(1.3)
                else
                    pause(0.4)
                end
            end
            hold off
            if patti < M
                grid off
            end
        end
    end
end

end
