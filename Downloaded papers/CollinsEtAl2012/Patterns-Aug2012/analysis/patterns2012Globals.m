function defs = patterns2012Globals(defs)

% Copyright 2012 Tom Collins

% Returns global parameters for the patterns2012 project.

projectRoot = fullfile('/home', 'tommyc', 'projects', 'patterns2012');
defs.projectRoot = projectRoot;
dataRoot = fullfile('/home', 'tommyc', 'projects', 'patterns2012');
defs.dataRoot = dataRoot;

datasetsRoot = fullfile(dataRoot, 'data');
defs.datasetsRoot = datasetsRoot;
abstractDatasetsRoot = fullfile(datasetsRoot, 'abstract');
defs.abstractDatasetsRoot = abstractDatasetsRoot;
musicDatasetsRoot = fullfile(datasetsRoot, 'music');
defs.musicDatasetsRoot = musicDatasetsRoot;
imagesDatasetsRoot = fullfile(datasetsRoot, 'images');
defs.imagesDatasetsRoot = imagesDatasetsRoot;
commoditiesDatasetsRoot = fullfile(datasetsRoot, 'commodities');
defs.commoditiesDatasetsRoot = commoditiesDatasetsRoot;

resultsRoot = fullfile(dataRoot, 'results');
defs.resultsRoot = resultsRoot;

% Take averages by the following categories.
defs.groups = {'algorithm', 'n', 'SNR', 'G'};

%% Parameter Choices
paramChoices = struct;
%% Dimension of dataset.
paramChoices.k = 2;
%% Cardinality of dataset (number of points in dataset).
paramChoices.n = 1000:200:2000;
%% Signal-to-noise ratio (constant or decreasing with n).
paramChoices.SNR = {'constant' 'decreasing'};
%% Number of artificially created patterns (hardcoded at present, depends
% on value of SNR).
% M = binornd(round(n/40), 0.5) or binornd(25, 0.5);
%% Number of occurrences of each pattern (hardcoded at present).
% m = max(poissrnd(5), 2);
%% Cardinality of each pattern (number of points in one occurrence of
% pattern, hardcoded at present).
% l = max(binornd(20, 0.5), 10);
%% Length of the integer grid.
paramChoices.L = 1000;
%% Length of the integer grid for a pattern (hardcoded at present).
% L1 = 30;
%% Generation method (shape outlines as in Zhang and Luck (2008) or
% scattered point sets)
paramChoices.G = {'shape' 'scatter'};
%% Number of superdiagonals to consider.
paramChoices.r = [1];
%% Compactness threshold.
paramChoices.a = 0.95;
%% Points threshold.
paramChoices.b = 5;
%% Region type.
paramChoices.regionType = {'lexicographic' 'convex hull'};
%% Number of simulations per dataset cardinality.
paramChoices.S = 20;
defs.paramChoices = paramChoices;

%% Algorithms to run.
algorithms = struct([]);
algoi = 0;
% SIA.
algoi = algoi + 1;
algorithms(algoi).name = 'SIA';
% SIAR.
algoi = algoi + 1;
rn = length(paramChoices.r);
for ri=1:rn
    algorithms(algoi + ri - 1).name = 'SIAR';
    algorithms(algoi + ri - 1).r = paramChoices.r(ri);
end
% SIARCT
algoi = algoi + 1;
rn = length(paramChoices.r);
for ri=1:rn
    algorithms(algoi + ri - 1).name = 'SIARCT';
    algorithms(algoi + ri - 1).r = paramChoices.r(ri);
    algorithms(algoi + ri - 1).a = paramChoices.a;
    algorithms(algoi + ri - 1).b = paramChoices.b;
    algorithms(algoi + ri - 1).regionType = paramChoices.regionType{2};
end
defs.algorithms = algorithms;

end
