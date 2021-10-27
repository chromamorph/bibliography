function outputStruct = filterByOccurrences(inputStruct, D, m)

% Copyright 2012 Tom Collins

% Returns discovered translational patterns from D that have at least m
% occurrences, and also sorts by occurrences.

% INPUT
%  inputStruct contains patterns discovered in D.
%  D is a dataset.
%  m is a threshold for the number of occurrences of a pattern.

% EXAMPLE
% params = patterns2012Globals;
% % Load point set and inputStruct. 
% load(fullfile(params.commoditiesDatasetsRoot, '7commodities',...
%     'Dataset_structs', 'KSAHD-20120510T062358.mat'));
% load(fullfile(params.resultsRoot, 'commodities-20120511T032213.mat'));
% % Reduce occurrences of a pattern to demonstrate filtering.
% S_SIAR_4(2).translators = [0 0 0];
% m = 2;
% outputStruct = filterByOccurrences(S_SIAR_4(1:3), datasetStruct.D, m);

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
    if ~isfield(S(patti), 'occurrences') || isempty(S(patti).occurrences)
        if ~isfield(S(patti), 'translators') ||...
                isempty(S(patti).translators)
            S(patti).translators =...
                translatorsOfPatternInDataset(S(patti).pattern, D);
        end
        S(patti).occurrences = size(S(patti).translators, 1);
    end
    if S(patti).occurrences >= m
        if pattj == 1
            outputStruct = S(patti);
        else
            outputStruct(pattj) = S(patti);
        end
        pattj = pattj + 1;
    end    
end % Increment over S.

% Sort outputStruct by occurrences field.
filtn = size(outputStruct, 2);
if filtn > 1
    a = [outputStruct.occurrences];
    [~, idx] = sort(a, 'descend');
    outputStruct = outputStruct(idx);
end

end
