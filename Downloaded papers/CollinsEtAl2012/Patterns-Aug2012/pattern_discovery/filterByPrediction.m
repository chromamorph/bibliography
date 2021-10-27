function outputStruct = filterByPrediction(inputStruct, D, E, minL, metric)

% Copyright 2012 Tom Collins

% Returns discovered translational patterns from D that have some
% predictive power, in the sense that a partially complete version of the
% pattern exists whose completion would require (and hence predicts the
% values of) future data. These predictions are then compared with observed
% future data in E, and the errors and directions of the predictions are
% stored in the output struct.

% INPUT
%  inputStruct contains patterns discovered in D.
%  D is a restricted (and quantized) dataset.
%  E is an unrestricted (extended to future) and unquantized dataset.
%  minL is a cardinality threshold: the partially complete occurrence must
%   have at least minL points in order to be considered.
%  metric is a string giving the field name by which members of the
%   outputStruct are sorted, e.g., 'RMSE' or 'credibility'.

% EXAMPLE
% params = patterns2012Globals;
% % Load point set and inputStruct. 
% load(fullfile(params.commoditiesDatasetsRoot, '7commodities',...
%     'Dataset_structs', 'KSAHD-20120510T062358.mat'));
% load(fullfile(params.resultsRoot, 'commodities-20120511T032213.mat'));
% minL = 10;
% metric = 'RMSE';
% outputStruct = filterByPrediction(S_SIAR_4(1:3), datasetStruct.D,...
%     datasetStruct.E, minL, 'RMSE');

S = inputStruct;
pattn = size(S, 2);
% outputStruct = struct([]);
outputStruct = repmat(struct('vector', [], 'pattern', [], 'indices',...
    [], 'cardinality', [], 'translators', []), 1, pattn);
k = size(D, 2);
currentTime = max(D(:,1));
pattj = 1; % Increment to create outputStruct.
for patti = 1:pattn % Increment over S.
    P = S(patti).pattern;
    L = size(P, 1);
    if ~isfield(S(patti), 'translators') || isempty(S(patti).translators)
        S(patti).translators =...
            translatorsOfPatternInDataset(S(patti).pattern, D);
    end
    T = S(patti).translators;
    QL = L - 1; % Used to increment over smaller versions of P until a
                % partially complete version Q is matched.
    while QL >= minL
        Q = P(1:QL, :);
        % U contains extra translators of Q (compared with those of P).
        U = setdiff(translatorsOfPatternInDataset(Q, D), T, 'rows');
        Um = size(U, 1);
        predictiveTranslators = zeros(Um, k);
        Uj = 1; % Increment to populate predictiveTranslators.
        for Ui = 1:Um
            predictionTime = P(QL+1, 1) + U(Ui, 1);
            % Determine if the extra translation of P extends beyond the
            % current dataset. And if the third column is zero (these are
            % the only translators that make sense).
            if predictionTime > currentTime && U(Ui, 3) == 0
                predictiveTranslators(Uj, :) = U(Ui, :);
                Uj = Uj + 1;
            end
        end
        Un = Uj - 1;
        predictiveTranslators = predictiveTranslators(1:Un, :);
        if Un >= 1 % That is, there are some predicitve translators.
            outputStruct(pattj).vector = S(patti).vector;
            outputStruct(pattj).pattern = S(patti).pattern;
            outputStruct(pattj).indices = S(patti).indices;
            outputStruct(pattj).cardinality = S(patti).cardinality;
            outputStruct(pattj).translators = S(patti).translators;
            predictions = struct([]);
            % Predictive datapoints.
            for Uj = 1:Un
                predictions(Uj).translator = predictiveTranslators(Uj, :);
                V = P(QL+1:L, :) +...
                    repmat(predictions(Uj).translator, L-QL, 1);
                Vm = size(V, 1);
                logIdx = zeros(Vm, 1);
                relIdx = zeros(Vm, 1);
                idxi = 1; % Increment to populate logIdx and relIdx.
                for Vi = 1:Vm
                    [tf, loc] =...
                        ismember(V(Vi, [1 3]), E(:, [1 3]), 'rows');
                    logIdx(Vi) = tf;
                    if tf % Some predicted datapoints may extend beyond
                          % rowPredictEnd, in which case exclude them from
                          % comparisons.
                        relIdx(idxi) = loc;
                        idxi = idxi + 1;
                    end
                end
                Vm = idxi - 1;
                relIdx = relIdx(1:Vm);
                V = V(logical(logIdx),:);
                predictions(Uj).predictivePoints = V;
                predictions(Uj).credibility = L/(L - QL);
                % Observed datapoints.
                W = E(relIdx, :);
                predictions(Uj).observedPoints = W;
                % Errors.
                predictions(Uj).RMSE = sqrt(mean((V(:, 2) - W(:, 2)).^2));
                % Find values at current time in relevant (3rd) column, to
                % test correct changes in direction.
                relIdx = zeros(Vm, 1);
                for Wi = 1:Vm
                    [~, loc] = ismember([currentTime W(Wi,3)],...
                        E(:, [1 3]), 'rows');
                    relIdx(Wi) = loc;
                end
                % Current datapoints.
                X = E(relIdx, :);
                predictions(Uj).currentPoints = X;
                predictions(Uj).testDirection =...
                    sign(V(:, 2) - X(:, 2)) == sign(W(:, 2) - X(:, 2));
            end
            % Sort predictions by metric field (e.g., RMSE or credibility).
            a = [predictions.(metric)];
            [~, idx] = sort(a, 'ascend');
            predictions = predictions(idx);
            % Update outputStruct with sorted predictions.
            outputStruct(pattj).predictions = predictions;
            outputStruct(pattj).maxMetric = max(a);
            pattj = pattj + 1;
            QL = minL - 1; % Cause while loop to terminate.
        else
            QL = QL - 1;
        end
    end
end % Increment over S.
filtn = pattj - 1;
outputStruct = outputStruct(1:filtn);

% Sort outputStruct by maxMetric field.
if filtn > 1
    b = [outputStruct.maxMetric];
    [~, idx2] = sort(b, 'ascend');
    outputStruct = outputStruct(idx2);
end

end

