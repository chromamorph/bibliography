function outputStruct = filterByDimensionValue(inputStruct, dimi, val)

% Copyright 2012 Tom Collins

% Returns discovered translational patterns from D that have a generating
% vector whose i_th dimension (dimi) is equal to val.

% INPUT
%  inputStruct contains patterns discovered in D.
%  dimi is a dimension of the dataset.
%  val is a scalar.

% EXAMPLE
% params = patterns2012Globals;
% % Load inputStruct. 
% load(fullfile(params.resultsRoot, 'commodities-20120511T032213.mat'));
% % Alter generating vector of a pattern to demonstrate filtering.
% S_SIAR_4(2).vector(3) = 1;
% dimi = 3;
% val = 0;
% outputStruct = filterByDimensionValue(S_SIAR_4(1:3), dimi, val);

outputStruct = struct([]);
pattj = 1; % Increment to create outputStruct;
S = inputStruct;
pattn = size(S, 2);
for patti = 1:pattn % Increment over S.
    v = S(patti).vector;
    if abs(v(dimi) - val) < 1e-10
        if pattj == 1
            outputStruct = S(patti);
        else
            outputStruct(pattj) = S(patti);
        end
        pattj = pattj + 1;
    end    
end % Increment over S.

end

