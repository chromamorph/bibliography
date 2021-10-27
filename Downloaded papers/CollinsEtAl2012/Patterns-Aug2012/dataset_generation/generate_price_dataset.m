function datasetStruct = generate_price_dataset(location, fName,...
    rowStart, rowEnd, rowPredictEnd, cols, nlevel, plotData, fNameOut)

% Copyright 2012 Tom Collins

% Returns a dataset D of discretized price data, and an extended dataset E
% of non-discretized price data, which extends beyond D for prediction
% purposes.

% INPUT
%  fNameCSV is a string specifying the location of the csv file containing
%   price data.
%  rowStart is a string of the form 'mmm-yy' specifying the month and year
%   to begin D.
%  rowEnd is a string of the form 'mmm-yy' specifying the month and year
%   to end D.
%  rowPredictEnd is a string of the form 'mmm-yy' specifying the month and
%   year to end E (it begins at the same time as D).
%  cols is a vector of integers indicating the relevant columns of the csv
%   file to read.
%  nlevel is a positive integer determining the number of discrete values
%   to which the price data can be mapped. For example, if the maximum
%   datum is 10 and nlevel = 20, then price data can be mapped to -10,
%   -9.5, -9,..., 10.
%  plotData takes the value one when the price data should be plotted, and
%   zero otherwise.
%  fNameOut is a string containing the path of where the datasetStruct
%   should be saved. A filename including date stamp is generated
%   automatically. If empty the datasetStruct is not saved.

% EXAMPLE
% params = patterns2012Globals;
% location = params.commoditiesDatasetsRoot;
% fName = '7commodities';
% rowStart = 1;
% rowEnd = 240;
% rowPredictEnd = 360;
% cols = 2:3:20;
% nlevel = 30;
% plotData = 1;
% fNameOut = [];
% datasetStruct = generate_price_dataset(location, fName,...
%     rowStart, rowEnd, rowPredictEnd, cols, nlevel, plotData, fNameOut);

% Read in csv file and transform to z-scores.
A = csvread(fullfile(location, fName, 'CSV', [fName '.csv']));
B = A(rowStart:rowPredictEnd, cols);
Bz = zscore(B);

% Discretize the input data to the nearest values as determined by nlevel.
% New version where quantisation is done independently.
% M = max(abs(Bz));
% Old version where quantisation was not done independently.
M = max(abs(Bz(:)));
mcol = size(Bz, 2);
% New version where quantisation is done independently.
% vals = zeros(mcol, 2*nlevel+1);
% for icol = 1:mcol
%     vals(icol, :) = -M(icol):M(icol)/nlevel:M(icol);
% end
% Old version where quantisation was not done independently.
vals = -M:M/nlevel:M;
% For each price datum, map it to the nearest value in vals. Now we are
% focusing on dateStart to dateEnd, not datePredictEnd.
mrow = rowEnd - rowStart + 1;
C = zeros(mrow, mcol);
for irow = 1:mrow
    for icol = 1:mcol
        % New version where quantisation is done independently.
        % [~, idx] = min(abs(Bz(irow, icol) - vals(icol, :)));
        % Old version where quantisation was not done independently.
        [~, idx] = min(abs(Bz(irow, icol) - vals));
        % New version where quantisation is done independently.
        % C(irow, icol) = vals(icol, idx);
        C(irow, icol) = vals(idx);
    end
end

% Populate the dataset D.
nrow = mrow*mcol;
D = zeros(nrow, 3);
jrow = 1; % Increment to populate D.
D(:,1) = repmat((1:mrow)', mcol, 1);
for icol = 1:mcol
    for irow = 1:mrow
        D(jrow, 2:3) = [C(irow, icol) icol];
        jrow = jrow + 1;
    end
end
D = unique(D, 'rows');

% Create the extended dataset E that contains price data beyond dateEnd
% (for prediction purposes).
mrow = size(Bz, 1);
nrow = mrow*mcol;
E = zeros(nrow, 3);
jrow = 1; % Increment to populate E.
E(:,1) = repmat((1:mrow)', mcol, 1);
for icol = 1:mcol
    for irow = 1:mrow
        E(jrow, 2:3) = [Bz(irow, icol) icol];
        jrow = jrow + 1;
    end
end
E = unique(E, 'rows');

% Define the output variable datasetStruct.
datasetStruct = struct();
datasetStruct.ID = [randsample('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 5, true) ...
    '-' datestr(clock, 30)];
datasetStruct.k = 3;
datasetStruct.n = size(D, 1);
datasetStruct.details = {};
datasetStruct.targetn = 0;
% Fields specific to a price dataset.
datasetStruct.D = D;
datasetStruct.E = E;
datasetStruct.rowStart = rowStart;
datasetStruct.rowEnd = rowEnd;
datasetStruct.rowPredictEnd = rowPredictEnd;
datasetStruct.cols = cols;
datasetStruct.nlevel = nlevel;
% Commodity names.
fid = fopen(fullfile(location, fName, 'Commodity_names', [fName '.txt']));
commNames = textscan(fid, '%s', size(cols, 2), 'delimiter', '');
fclose(fid);
commNames = commNames{1};
for namei = 1:size(cols, 2)
    commNames{namei} = char(commNames{namei});
end
datasetStruct.commodityNames = commNames;
% Commodity dates.
mA = size(A, 1);
fid = fopen(fullfile(location, fName, 'Commodity_dates', [fName '.txt']));
commDates = textscan(fid, '%s %s', mA, 'delimiter', '-');
fclose(fid);
commDates = [commDates{1, 1} commDates{1, 2}];
for datei = 1:mA
    commDates{datei, 1} = char(commDates{datei, 1});
    commDates{datei, 2} = char(commDates{datei, 2});
end
datasetStruct.commodityDates = commDates;
if fNameOut
    save(fullfile(fNameOut, [datasetStruct.ID '.mat']), 'datasetStruct');
    fprintf('Saved dataset %s to file.\n', datasetStruct.ID);
end

if plotData
    FontSize = 16;
    FontName = 'Helvetica';
    % lineSpecs = {'.-c', '.-b', '.-g', '.-r', '.-y', '.-m', '.-k'};
    lineSpecs = {'c', 'b', 'g', 'r', 'y', 'm', 'k'};
    nlineSpec = size(lineSpecs, 2);
    figure
    orient landscape
    set(gcf, 'PaperPosition', [.25 .25 10 4.7]);
    hold on
    for icol = 1:mcol
        plot(Bz(:, icol), lineSpecs{1, mod(icol, nlineSpec)+1},...
            'LineWidth', 1.3);
    end
    % Put in a vertical line for the current point in time.
    line([rowEnd rowEnd], [min(min(Bz))-0.2 max(max(Bz))+0.2], 'Color',...
        [0.5 0.5 0.5], 'LineWidth', 2)
    hold off
    grid
    set(gca, 'FontSize', FontSize);
    set(gca, 'FontName', FontName);
    xlim([1 rowPredictEnd])
    xtickidx = round(linspace(1, rowPredictEnd, 5));
    set(gca, 'XTick', xtickidx);
    nticks = size(xtickidx, 2);
    xticklab = cell(1, nticks);
    for itick = 1:nticks
        xticklab{itick} =...
            [char(commDates(xtickidx(itick),1)) ' '...
            char(commDates(xtickidx(itick),2))];
    end
    set(gca, 'XTickLabel', xticklab);
    ylim([min(min(Bz))-0.2 max(max(Bz))+0.2])
    xlabel('Month and year')
    ylabel('Price (z-score)')
    legend(commNames, 'Location', 'NorthWest', 'FontSize', FontSize - 1);
    lh = findobj(gcf, 'type', 'axes', 'tag', 'legend');
    Pos = get(lh, 'position');
    Pos(1) = 0.058;
    Pos(2) = 0.41;
    Pos(3) = 1.15*Pos(3);
    set(lh, 'position', Pos)
    if fNameOut
        print('-dpsc', '-append',...
            fullfile(fNameOut, [datasetStruct.ID '.ps']));
    end
end

end
