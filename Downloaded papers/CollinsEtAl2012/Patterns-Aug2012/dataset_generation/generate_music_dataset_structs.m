function datasetStructs = generate_music_dataset_structs(location,...
    patternTypes, projectionIdx, plotData, saveData)

% Copyright 2012 Tom Collins

% Returns a cell array of structures, each structure is a datasetStruct
% containing the point set representation of a piece of music, as well as
% details of the annotated translational patterns occurring within that
% piece.

% INPUT
%  location is a string specifying the location of csv files for arranging
%   into datasetStructs.
%  patternTypes is a cell array of strings that restrict the type of
%   pattern considered to those in folders with matching names (e.g.,
%   'Sectional_repetitions').
%  projectionIdx is a projection index, specifying which columns of the
%   csv files to use (which musical dimensions to focus on).
%  plotData takes the value one when the pieces and annotated patterns
%   should be plotted, and zero otherwise.
%  saveData takes the value one if each datasetStruct should be saved to
%   the Dataset_structs location of the piece's host folder, and if the
%   datasetIDs variable should be saved, and zero otherwise.

% EXAMPLE
% s = RandStream('mt19937ar','Seed',1);
% RandStream.setDefaultStream(s); % Seed for purpose of creating same
%                                 % labels. This code is deterministic.
% params = patterns2012Globals;
% location = params.musicDatasetsRoot;
% patternTypes = {'Sectional_repetitions'};
% projectionIdx = [1 3];
% plotData = 1;
% saveData = 0;
% datasetStructs = generate_music_dataset_structs(location,...
%     patternTypes, projectionIdx, plotData, saveData);

if plotData
    h = figure;
end
datasetStructs = {};
datasetIDs = {};
piecei = 1; % Increment to create datasetStructs.
contents = dir(location); % Contents of location storing music data.
ndir = size(contents, 1);
for idir = 1:ndir % Iterate over the pieces in the folder specified by
                  % location.
    % If the name folder is one of the following, do nothing.
    if strcmp(contents(idir).name, '.') || ...
            strcmp(contents(idir).name, '..') || ...
            strcmp(contents(idir).name, '.DS_Store') || ...
            strcmp(contents(idir).name,...
            'datasetIDs-20120402T114459.mat') || ...
            strcmp(contents(idir).name, '1example')
        continue
    end
    pathName = fullfile(location, contents(idir).name);
    
    % Locate and load the first csv file for this pathName.
    pieceContents = dir(fullfile(pathName, 'CSV_datasets'));
    ncsv = size(pieceContents, 1);
    icsv = 1;
    while icsv <= ncsv
        % If the name folder is one of the following, do nothing.
        if strcmp(pieceContents(icsv).name, '.') || ...
                strcmp(pieceContents(icsv).name, '..') || ...
                strcmp(pieceContents(icsv).name, '.DS_Store')
            icsv = icsv + 1;
        else
            csvName = fullfile(pathName, 'CSV_datasets',...
                pieceContents(icsv).name);
            icsv = ncsv + 1;
        end
    end
    % Round to three decimal places, to allow very-near translations.
    D = round(1000*csvread(csvName))/1000;
    pointsOnly = unique(D(:, projectionIdx), 'rows');
    
    % Patterns directory.
    pattDirContents = dir(fullfile(pathName, 'Repeated_patterns'));
    nPattDir = size(pattDirContents, 1);
    pattDirPaths = {};
    pattDirPathi = 1; % Increment to create pattDirPaths.
    for iPattDir = 1:nPattDir
        if strcmp(pattDirContents(iPattDir).name, '.') || ...
                strcmp(pattDirContents(iPattDir).name, '..') || ...
                strcmp(pattDirContents(iPattDir).name, '.DS_Store') || ...
                ~ismember(pattDirContents(iPattDir).name, patternTypes)
            continue
        end
        pattDirPaths{pattDirPathi} = pattDirContents(iPattDir).name;
        pattDirPathi = pattDirPathi + 1;
    end
    pattDirPathn = size(pattDirPaths, 2);
    
    % Create the details field.
    details = struct([]);
    deti = 1; % Increment to create details.
    for pattDirPathi = 1:pattDirPathn % Increment over the pattern
                                      % directories.
        pattPath = fullfile(pathName, 'Repeated_patterns',...
            pattDirPaths{pattDirPathi});
        pattPaths = dir(pattPath);
        pattPathn = size(pattPaths, 1);
        for pattPathi = 1:pattPathn % Increment over the patterns within
                                    % each directory.
            if strcmp(pattPaths(pattPathi).name, '.') || ...
                    strcmp(pattPaths(pattPathi).name, '..') || ...
                    strcmp(pattPaths(pattPathi).name, '.DS_Store')
                continue
            end
            pattContents = dir(fullfile(pathName, 'Repeated_patterns',...
                pattDirPaths{pattDirPathi}, pattPaths(pattPathi).name,...
                'CSV_datasets'));
            % Locate and load the first csv file for this pattContents.
            ncsv = size(pattContents, 1);
            icsv = 1;
            while icsv <= ncsv
                % If the name folder is one of the following, do nothing.
                if strcmp(pattContents(icsv).name, '.') || ...
                        strcmp(pattContents(icsv).name, '..') || ...
                        strcmp(pattContents(icsv).name, '.DS_Store')
                    icsv = icsv + 1;
                else
                    csvName = fullfile(pathName, 'Repeated_patterns',...
                        pattDirPaths{pattDirPathi},...
                        pattPaths(pattPathi).name, 'CSV_datasets',...
                        pattContents(icsv).name);
                    icsv = ncsv + 1;
                end
            end
            % Round to three decimal places, to allow very-near
            % translations.
            Q = round(1000*csvread(csvName))/1000;
            P = unique(Q(:, projectionIdx), 'rows');
            T = translatorsOfPatternInDataset(P, pointsOnly);
            L = size(P, 1);
            m = size(T, 1);
            if m < 2
                fprintf(['Annotated pattern %s in %s has fewer than two'...
                    ' occurrences.\n'], pattPaths(pattPathi).name,...
                    contents(idir).name);
            end
            details(deti).name = pattPaths(pattPathi).name;
            details(deti).type = pattDirPaths{pattDirPathi};
            details(deti).pattern = P;
            details(deti).translators = T;
            details(deti).cardinality = L;
            details(deti).occurrences = m;
            deti = deti + 1;
        end
    end
    
    % Only populate, plot, and save if there are some patterns in this
    % directory.
    if deti >= 2
        % Populate other fields of datasetStructs.
        datasetStruct = struct;
        datasetStruct.ID =...
            [randsample('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 5, true) '-' ...
            datestr(clock, 30)];
        datasetStruct.k = 2;
        datasetStruct.n = size(pointsOnly, 1);
        datasetStruct.G = 'music';
        datasetStruct.SNR = 'no noise';
        datasetStruct.details = details;
        datasetStruct.pointsOnly = pointsOnly;
        datasetStruct.pointsAugm = pointsOnly; % There are no random
        % augmentations.
        datasetStruct.targetn = size(details, 2);
        % Extra fields specific to a piece of music.
        datasetStruct.name = contents(idir).name;
        datasetStruct.projectionIdx = projectionIdx;
        
        % Plot the annotated patterns.
        if plotData
            detn = deti - 1;
            for deti = 1:detn
                clf(h)
                hold on
                plot(pointsOnly(:,1), pointsOnly(:,2), '.k');
                pause(0.5);
                tn = details(deti).occurrences;
                for ti = 1:tn;
                    Q = details(deti).pattern +...
                        repmat(details(deti).translators(ti,:),...
                        details(deti).cardinality, 1);
                    plot(Q(:,1), Q(:,2), '*b');
                    pause(0.5)
                end
                hold off
            end
        end
        
        % Save datasetStruct.
        if saveData
            save(fullfile(pathName, 'Dataset_structs',...
                [datasetStruct.ID '.mat']), 'datasetStruct');
            fprintf('Saved dataset %s to file.\n', datasetStruct.ID);
        end
        % datasetIDs{piecei} = fullfile(pathName, 'Dataset_structs',...
        %         [datasetStruct.ID '.mat']);
        datasetIDs{piecei} = fullfile(contents(idir).name,...
            'Dataset_structs', [datasetStruct.ID '.mat']);
        
        % Finally, update the datasetStructs variable.
        datasetStructs{piecei} = datasetStruct;
        piecei = piecei + 1;
    end
end

% Save datasetIDs.
if saveData
    save(fullfile(location, 'datasetIDs-20120402T114459'), 'datasetIDs');
end

end