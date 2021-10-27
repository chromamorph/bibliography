function outputStruct = filterByCardinality(inputStruct, lowerL, upperL)

% Copyright 2012 Tom Collins

% Returns discovered translational patterns from D that have at least L
% points (cardinality L), and also sorts by cardinality.

% INPUT
%  inputStruct contains patterns discovered in D.
%  D is a dataset.
%  L is a threshold for the cardinality of a pattern.

% EXAMPLE
% params = patterns2012Globals;
% % Load inputStruct. 
% load(fullfile(params.resultsRoot, 'commodities-20120511T032213.mat'));
% lowerL = 17;
% upperL = 18;
% outputStruct = filterByCardinality(S_SIAR_4, lowerL, upperL);

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
    if ~isfield(S(patti), 'cardinality') || isempty(S(patti).cardinality)
        S(patti).cardinality = size(S(patti).pattern, 1);
    end
    if S(patti).cardinality >= lowerL && S(patti).cardinality <= upperL 
        if pattj == 1
            outputStruct = S(patti);
        else
            outputStruct(pattj) = S(patti);
        end
        pattj = pattj + 1;
    end    
end % Increment over S.

% Sort outputStruct by cardinality field.
filtn = size(outputStruct, 2);
if filtn > 1
    a = [outputStruct.cardinality];
    [~, idx] = sort(a, 'descend');
    outputStruct = outputStruct(idx);
end

end
