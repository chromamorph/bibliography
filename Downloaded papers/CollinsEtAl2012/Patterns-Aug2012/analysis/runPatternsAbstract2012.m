% Copyright 2012 Tom Collins

% This script runs the simulation for abstract shape and scatter data.

s = RandStream('mt19937ar','Seed',1);
RandStream.setDefaultStream(s);
% A = randn(3); % Test seed.

% Load globals.
params = patterns2012Globals;
p = params.paramChoices;

% Generate datasets. The for loop does not need running every time. Can
% just reload the datasetIDs variable.
plotData = 0;
nn = size(p.n, 2);
SNRn = size(p.SNR, 2);
Gn = size(p.G, 2);
datasetIDs = cell(1, nn*SNRn*Gn*p.S);
datai = 1; % Increment to create datasetIDs.
for ni = 1:nn % Iterate over dataset cardinalities.
    for SNRi = 1:SNRn % Iterate over signal-to-noise ratios.
        for Gi = 1:Gn % Iterate over generation methods.
            for Si = 1:p.S
                datasetStruct = generate_dataset(p.k, p.n(ni), p.L,...
                    p.G(Gi), p.SNR(SNRi), plotData,...
                    params.abstractDatasetsRoot);
                datasetIDs{datai} = datasetStruct.ID;
                datai = datai + 1;
            end
        end
    end
end
% save(fullfile(params.abstractDatasetsRoot,...
%     'datasetIDs-20120221T102059'), 'datasetIDs');
datasetIDs =...
    load(fullfile(params.abstractDatasetsRoot,...
    'datasetIDs-20120221T102059'));
datasetIDs = datasetIDs.datasetIDs;

fName = fullfile(params.resultsRoot, 'abstract-20120221T102059');
[table_all, table_av] = algorithmMetrics(datasetIDs,...
    params.abstractDatasetsRoot, params.algorithms, params.groups, fName);
