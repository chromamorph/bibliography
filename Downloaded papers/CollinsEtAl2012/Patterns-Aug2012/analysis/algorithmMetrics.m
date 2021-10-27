function [table_all, table_av] = algorithmMetrics(datasetIDs,...
    datasetsRoot, algorithms, groups, fName)

% Copyright 2012 Tom Collins

% This function loads datasets specified by the first variable, and runs
% algorithms specified by the second variable on each of these datasets.
% The output consists of two tables (arrays), the first containing a row
% for each dataset and algorithm, and the second containing a row for each
% group, where grouping is specified by the third input variable. The
% metrics returned are precision, recall, fifth return time (FRT), and
% runtime. The column called top 5 target contains a one if any of the
% first five returned patterns are targets, and zero otherwise.

% INPUT
%  datasetIDs is a cell of strings that refer to stored datasets.
%  datasetsRoot is a string specifying the location of the stored datasets.
%  algorithms is a vector of structs where each struct specifies an
%   algorithm to be run, including the parameters with which it should be
%   run. If the same algorithm is to be run with different parameters, then
%   each parameter set will have its own entry in the variable algorithms.
%  groups is a cell of strings, specifying how averages should be
%   calculated.
%  fName is a string containing the path of where the tables should be
%   saved. If empty the tables are printed only.

% EXAMPLE
% params = patterns2012Globals;
% datasetsRoot = fullfile(params.abstractDatasetsRoot, 'test');
% datasetIDs = {'GMQAC-20120217T092327' 'GVNCM-20120217T092339'...
%     'OYGRU-20120217T092333' 'RHQJI-20120217T092323'...
%     'RPTOS-20120219T143652'};
% algorithms = struct([]);
% algorithms(1).name = 'SIAR';
% algorithms(1).r = 1;
% algorithms(2).name = 'SIA';
% groups = {'algorithm', 'n'};
% fName = fullfile(params.resultsRoot, 'test');
% [table_all, table_av] = algorithmMetrics(datasetIDs,...
%     datasetsRoot, algorithms, groups, fName);

algon = size(algorithms, 2);
datan = size(datasetIDs, 2);
% Load all the datasets into one struct.
for datai = 1:datan
    fprintf('Loading dataset %d: %s.\n', datai, datasetIDs{datai});
    datasetStruct =...
            load(fullfile(datasetsRoot, datasetIDs{datai}));
    datasetsStruct(datai) = datasetStruct.datasetStruct;
end

% Define empty table.
rown = algon*datan;
table_all = cell(rown, 7);
rowi = 1; % Increment over table_all.
% Labels for each algorthim, using parameter values to distinguish.
algoLabels = cell(1, algon);

for algoi = 1:algon % Iterate over algorithms.
    fprintf('Running algorithm %d: %s\n', algoi, algorithms(algoi).name);
    % Labels for each algorthim, using parameter values to distinguish.
    flds = fieldnames(algorithms(algoi));
    fldsn = size(flds, 1);
    algoLabel = '';
    for fldsi = 1:fldsn
        valAsStr = algorithms(algoi).(flds{fldsi});
        if isnumeric(valAsStr)
            valAsStr = num2str(valAsStr);
        end
        if ~isempty(valAsStr)
            if strcmp(flds{fldsi}, 'name')
                algoLabel = [algoLabel char(valAsStr) ', '];
            else
                algoLabel =...
                    [algoLabel flds{fldsi} '=' char(valAsStr) ', '];
            end
        end
    end
    algoLabels{algoi} = algoLabel;
    for datai = 1:datan % Iterate over datasets.
        fprintf('...on dataset %d: %s.\n', datai, datasetIDs{datai});
        D = datasetsStruct(datai).pointsAugm;
        if strcmp(algorithms(algoi).name, 'SIA')
            [algoOutput, runtime, FRT] = SIA(D);
        elseif strcmp(algorithms(algoi).name, 'SIAR')
            r = algorithms(algoi).r;
            [algoOutput, runtime, FRT] = SIAR(D, r);
        elseif strcmp(algorithms(algoi).name, 'SIACT')
            a = algorithms(algoi).a;
            b = algorithms(algoi).b;
            regionType = algorithms(algoi).regionType;
            [algoOutput, runtime, FRT] = SIACT(D, a, b, regionType);
        elseif strcmp(algorithms(algoi).name, 'SIARCT')
            r = algorithms(algoi).r;
            a = algorithms(algoi).a;
            b = algorithms(algoi).b;
            regionType = algorithms(algoi).regionType;
            [algoOutput, runtime, FRT] = SIARCT(D, r, a, b, regionType);
            
            
        else
        end
        % Find precision, recall, and other metrics.
        fprintf('Calculating metrics for dataset %d: %s.\n', datai,...
            datasetIDs{datai});
        [prec rec tf] =...
            precisionRecallTrans(algoOutput, datasetsStruct(datai));
        % Write results to table.
        table_all{rowi, 1} = datasetIDs{datai};
        table_all{rowi, 2} = algoLabels{algoi};
        table_all{rowi, 3} = prec;
        table_all{rowi, 4} = rec;
        table_all{rowi, 5} = runtime;
        table_all{rowi, 6} = FRT;
        table_all{rowi, 7} = tf;
        rowi=rowi+1;
    end
end

% Obsolete, but worth keeping for working out existence of different
% levels.
% Determine how many different levels there are in each group, and the
% values of these levels. Put this in a cell array that corresponds to the
% variable groups.
grpsn = size(groups, 2);
levels = cell(1, grpsn);
for grpsi = 1:grpsn % Iterate over groups.
    if strcmp(groups(grpsi), 'algorithm')
        levels{grpsi} = algoLabels;
    else
        levels{grpsi} = cell(1, datan);
        for datai = 1:datan % Iterate over datasets.
            valAsStr = datasetsStruct(datai).(groups{grpsi});
            if isnumeric(valAsStr)
                valAsStr = num2str(valAsStr);
            end
            levels{grpsi}{datai} = char(valAsStr);
        end
        levels{grpsi} = unique(levels{grpsi});
    end
end

% Define empty table.
fprintf('Creating table of averages.\n');
table_av = cell(rown, 11);
rowi = 1; % Increment to create table_av.
all_idx = 1:rown;
for rowj = 1:rown
    if ismember(rowj, all_idx)
        all_idx = setdiff(all_idx, rowj);
        idxn = size(all_idx, 2);
        rel_idx = zeros(1, idxn);
        rel_idx(1) = rowj;
        idxi = 2; % Increment to create rel_idx.
        % Define the label for rowj.
        dataj = find(strcmp(table_all{rowj, 1}, {datasetsStruct.ID}), 1);
        rowLabelj = table_all{rowj, 2};
        for grpsi = 1:grpsn % Iterate over groups.
            if ~strcmp(groups{grpsi}, 'algorithm')
                valAsStr = datasetsStruct(dataj).(groups{grpsi});
                if isnumeric(valAsStr)
                    valAsStr = num2str(valAsStr);
                end
                rowLabelj =...
                    [rowLabelj groups{grpsi} '=' char(valAsStr) ', '];
            end
        end
        % Determine which other rows of table_all are in the same group as
        % rowj.
        for idxk = 1:idxn % Increment over all_idx.
            rowk = all_idx(idxk);
            % Define the label for rowk.
            datak =...
                find(strcmp(table_all{rowk, 1}, {datasetsStruct.ID}), 1);
            rowLabelk = table_all{rowk, 2};
            for grpsi = 1:grpsn % Iterate over groups.
                if ~strcmp(groups{grpsi}, 'algorithm')
                    valAsStr = datasetsStruct(datak).(groups{grpsi});
                    if isnumeric(valAsStr)
                        valAsStr = num2str(valAsStr);
                    end
                    rowLabelk =...
                        [rowLabelk groups{grpsi} '=' char(valAsStr) ', '];
                end
            end
            if strcmp(rowLabelk, rowLabelj) % Test of same group.
                % Include rowk in rel_idx.
                rel_idx(idxi) = rowk;
                idxi = idxi + 1;
                % Update all_idx.
                
            end
        end
        % Update indices.
        rel_idx = rel_idx(1:idxi-1);
        all_idx = setdiff(all_idx, rel_idx);
        % Create an entry in table_av.
        table_av{rowi, 1} = rowLabelj;
        % Precision group mean and std. 
        table_av{rowi, 2} = mean([table_all{rel_idx, 3}]);
        table_av{rowi, 3} = std([table_all{rel_idx, 3}]);
        % Recall group mean and std. 
        table_av{rowi, 4} = mean([table_all{rel_idx, 4}]);
        table_av{rowi, 5} = std([table_all{rel_idx, 4}]);
        % runtime group mean and std.
        table_av{rowi, 6} = mean([table_all{rel_idx, 5}]);
        table_av{rowi, 7} = std([table_all{rel_idx, 5}]);
        % FRT group mean and std.
        table_av{rowi, 8} = mean([table_all{rel_idx, 6}]);
        table_av{rowi, 9} = std([table_all{rel_idx, 6}]);
        % Proportion of top 5 returns that contain a target.
        table_av{rowi, 10} = mean([table_all{rel_idx, 7}]);
        % Datasets in group (just for testing).
        table_av{rowi, 11} = {table_all{rel_idx, 1}};
        rowi = rowi + 1;
    end
end
table_av = table_av(1:rowi-1, :);
fprintf('Created table of averages.\n');

% Define titles for table_all and table_av.
titles_all = cell(1, 7);
titles_all{1} = 'Dataset ID';
titles_all{2} = 'Algorithm';
titles_all{3} = 'Precision';
titles_all{4} = 'Recall';
titles_all{5} = 'Runtime';
titles_all{6} = 'FRT';
titles_all{7} = 'Top 5 target';
table_all = [titles_all; table_all];

titles_av = cell(1, 11);
titles_av{1} = 'Group label';
titles_av{2} = 'Precision mean';
titles_av{3} = 'Precision std';
titles_av{4} = 'Recall mean';
titles_av{5} = 'Recall std';
titles_av{6} = 'Runtime mean';
titles_av{7} = 'Runtime std';
titles_av{8} = 'FRT mean';
titles_av{9} = 'FRT std';
titles_av{10} = 'Top 5 proportion';
titles_av{11} = 'Datasets';
table_av = [titles_av; table_av];

% Saving option.
if fName
    save(fName, 'table_all', 'table_av');
    fprintf('Saved results tables to %s.\n', fName);
end

end
