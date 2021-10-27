function [] = plotPattern(S, D, i)

% Copyright 2012 Tom Collins

% This function takes a vector of structs with information about a musical
% pattern as its first argument, a point set representation (called a
% dataset) of a piece of music (or excerpt) as its second argument, and an
% index i referring to an element of the vector of structs as its third
% argument. It plots the dataset as points in the plane, and pressing a
% keyboard button will cause the first occurrence of the pattern to be
% highlighted. Subsequent presses of the keyboard will highlight subsequent
% occurrences of the pattern, until all have been highlighted.

% INPUT
%  S is a vector of structs, with each struct containing information about
%   a repeated pattern.
%  D is a point set.
%  i is an index referring to an element of S.

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
% plotPattern(S, D, 1)

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

% Define and name parts of the figure.
figure
hold on
plot(d(:,1),d(:,2),'kx')
axis([(min(d(:,1)) - 1) (max(d(:,1)) + 1) (min(d(:,2)) - 1) ...
    (max(d(:,2)) + 1)]);
xlabel('Ontime (crotchet beats)')
ylabel(yAxisLabel)
title(strcat(s.projection, ' pattern'))
grid
% Add occurrences of patterns to plot.
c = ['c' 'b' 'm' 'g' 'r' 'y'];
j = 1;
while j <= s.occurrences
    waitforbuttonpress
    x = s.pattern(:,1) + s.translators(j,1);
    y = s.pattern(:,2) + s.translators(j,2);
    plot(x,y,strcat(c(mod(j,6)+1), '>'));
    j=j+1;
end
hold off

end
