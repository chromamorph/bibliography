% Copyright 2012 Tom Collins

% This script runs the simulation for image data.

s = RandStream('mt19937ar','Seed',1);
RandStream.setDefaultStream(s);
% A = randn(3); % Test seed.

% Load globals.
params = patterns2012Globals;
p = params.paramChoices;

% Generate image stack. This does not need running every time. Can just
% reload the datasetIDs variable.
stackSize = 50; % Size of the image stack.
nosPts = 100; % Size of maximum dimension of each image.
plotData = 0;
datasetIDs = cell(1, p.S);
datai = 1; % Increment to create datasetIDs.
for Si = 1:p.S % Number of simulations (set in patterns2012Globals).
    datasetStruct = generate_image_stack(params.imagesDatasetsRoot,...
        stackSize, nosPts, plotData,...
        fullfile(params.imagesDatasetsRoot, 'stack'));
    datasetIDs{datai} = datasetStruct.ID;
    datai = datai + 1;
end

% save(fullfile(params.imagesDatasetsRoot, 'stack',...
%     'datasetIDs-20120327T160759'), 'datasetIDs');
datasetIDs =...
    load(fullfile(params.imagesDatasetsRoot, 'stack',...
    'datasetIDs-20120327T160759'));
datasetIDs = datasetIDs.datasetIDs;

algoi = size(params.algorithms, 2) + 1;
params.algorithms(algoi).name = 'SIARCT';
params.algorithms(algoi).r = p.r;
params.algorithms(algoi).a = p.a;
params.algorithms(algoi).b = p.b;
params.algorithms(algoi).regionType = p.regionType{1};
params.groups = {'algorithm'};

fName = fullfile(params.resultsRoot, 'images-20120328T025059');
[table_all, table_av] = algorithmMetrics(datasetIDs,...
    fullfile(params.imagesDatasetsRoot, 'stack'),...
    params.algorithms, params.groups, fName);
