% Copyright 2012 Tom Collins

% This script runs the simulation for music data.

s = RandStream('mt19937ar','Seed',1);
RandStream.setDefaultStream(s);
% A = randn(3); % Test seed.

% Load globals.
params = patterns2012Globals;
p = params.paramChoices;

% Generate datasets. The function generate_music_dataset_structs does not
% need running every time. This is deterministic, but seeded so that the
% datasetStruct labels are the same if run again.
location = params.musicDatasetsRoot;
patternTypes = {'Sectional_repetitions'};
projectionIdx = [1 2];
plotData = 1;
saveData = 1;
datasetStructs = generate_music_dataset_structs(location, patternTypes,...
    projectionIdx, plotData, saveData);
datasetIDs =...
    load(fullfile(params.musicDatasetsRoot,...
    'datasetIDs-20120402T114459'));
datasetIDs = datasetIDs.datasetIDs;

% Algorithm parameter alterations here...
params.algorithms(4) = params.algorithms(3);
params.algorithms(4).a = .999;
params.algorithms(4).b = 50;
params.algorithms(5) = params.algorithms(4);
params.algorithms(5).regionType = 'lexicographic';
params.groups = {'algorithm'};

fName = fullfile(params.resultsRoot, 'music-20120402T114459');
[table_all, table_av] = algorithmMetrics(datasetIDs,...
    params.musicDatasetsRoot, params.algorithms, params.groups, fName);
