% Copyright 2012 Tom Collins

% This script runs the simulation for commodity price data.

s = RandStream('mt19937ar','Seed',1);
RandStream.setDefaultStream(s);
% A = randn(3); % Test seed.

% What follows is deterministic, but seeded so that the datasetStruct
% labels are the same if run again.

% Load globals and create datasetStruct.
params = patterns2012Globals;
location = params.commoditiesDatasetsRoot;
fName = '7commodities';
rowStart = 1; % March 1982.
rowEnd = 240; % February 2002.
rowPredictEnd = 360; % February 2012.
cols = 2:3:20;
nlevel = 30;
plotData = 1;
fNameOut = fullfile(params.commoditiesDatasetsRoot, '7commodities',...
    'Dataset_structs');
% fNameOut = [];
datasetStruct = generate_price_dataset(location, fName, rowStart,...
    rowEnd, rowPredictEnd, cols, nlevel, plotData, fNameOut);
datasetStruct = load(fullfile(params.commoditiesDatasetsRoot,...
    '7commodities', 'Dataset_structs', 'KSAHD-20120510T062358.mat'));
datasetStruct = datasetStruct.datasetStruct;

% Run SIAR, or load previously saved results.
r = 1;
tic
S_SIAR = SIAR(datasetStruct.D, r);
toc
% Filter output by dimension value, cardinality, and prediction of unseen
% data.
lowerL = 10; % Lower cardinality threshold.
upperL = 20; % Upper cardinality threshold.
minL = 5; % Minimum cardinality when pruning for predictive translators.
tic
S_SIAR_2 = filterByDimensionValue(S_SIAR, 3, 0);
S_SIAR_3 = filterByCardinality(S_SIAR_2, lowerL, upperL);
toc
tic
S_SIAR_4 = filterByPrediction(S_SIAR_3, datasetStruct.D,...
    datasetStruct.E, minL, 'RMSE');
toc
save(fullfile(params.resultsRoot, ['commodities-' datestr(clock, 30)]),...
    'S_SIAR_4');
S_SIAR_4 = load(fullfile(params.resultsRoot,...
    'commodities-20120511T032213.mat'));
S_SIAR_4 = S_SIAR_4.S_SIAR_4;
