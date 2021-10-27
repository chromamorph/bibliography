function [] = plotPatternOccurrence(S, D, i, j, allOcc)

% Copyright 2012 Tom Collins

% This function takes a vector of structs with information about a musical
% pattern as its first argument, a point set representation (called a
% dataset) of a piece of music (or excerpt) as its second argument, an
% index i referring to an element of the vector of structs as its third
% argument, and an index j of occurrence as its fourth argument. It plots
% the dataset as points in the plane, and the jth occurrence of the pattern
% is highlighted. The variable allOcc takes value one if all occurrences
% are to be displayed, and zero otherwise.

% INPUT
%  S is a vector of structs, with each struct containing information about
%   a repeated pattern.
%  D is a point set.
%  i is an index referring to an element of S.
%  j is an index referring to an occurence of the ith element of S.

% EXAMPLE
% params = patterns2012Globals;
% fileName = fullfile(params.musicDatasetsRoot, 'chopinOp59No1',...
%     'CSV_datasets', 'Chopin-op59-no1.txt');
% compactThresh = 2/3;
% cardinaThresh = 5;
% regionType = 'lexicographic';
% topN = 10;
% tic
% [S, D] = discoverRepeatedPatterns(fileName, compactThresh,...
%     cardinaThresh, regionType, topN);
% toc
% plotPatternOccurrence(S, D, 1, 2, 0)

s = S(i);
% Determine the projection of the dataset to which the pattern belongs.
if isfield(s, 'projection')
    if strcmp(s.projection, 'Ontime and MIDI note number')
        d = unique(D(:,1:2),'rows');
        yAxisLabel = 'Pitch (MIDI note number)';
    elseif strcmp(s.projection, 'Ontime and morphetic pitch number')
        d = unique([D(:,1) D(:,3)],'rows');
        yAxisLabel = 'Pitch (morphetic pitch number)';
    else
        d = unique([D(:,1) D(:,4)],'rows');
        yAxisLabel = 'Duration (crotchet beats)';
    end
else
    d = D;
    yAxisLabel = 'Staff height (middle C = 60)';
end

x = (s.pattern(:,1) + s.translators(j,1))/4 + 1;
y = s.pattern(:,2) + s.translators(j,2);
    
% Define and name parts of the figure.
FontSize = 16;
FontName = 'Helvetica';
figure
hold on
xlim([min(x) - 1 max(x) + 1])
ylim([min(y) - 4 max(y) + 4])
plot(d(:,1)/4 + 1,d(:,2),'kx')
xlabel('Bar', 'FontName', FontName, 'FontSize', FontSize)
ylabel(yAxisLabel, 'FontName', FontName, 'FontSize', FontSize)
m = s.occurrences;
if allOcc
    title(sprintf('Pattern %d, occurrence %d', i, j), 'FontSize',...
        FontSize, 'FontName', FontName)
else
    title(sprintf('Pattern %d has %d occurrences', i, m), 'FontSize',...
        FontSize, 'FontName', FontName)
end
grid
set(gca, 'FontSize', FontSize);
set(gca, 'FontName', FontName);    
plot(x,y,'bo');
hold off

end
