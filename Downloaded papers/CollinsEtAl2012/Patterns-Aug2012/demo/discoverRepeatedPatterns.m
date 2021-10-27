function [S, D] = discoverRepeatedPatterns(fileName, compactThresh,...
    cardinaThresh, regionType, topN)

% Copyright 2012 Tom Collins

% This function takes a file name (string) as its first argument, referring
% to a point-set representation (called a dataset) of a piece of music (or
% excerpt). The function runs a pattern discovery algorithm, a
% compactness-trawling algorithm, and a rating formula on each of three
% projections of the dataset. These projections are two-dimensional, but
% the code does work with dimensions > 2 as well.

% INPUT
%  fieldName is a string specifying the location of the point set.
%  compactThresh is a real 0 < a <= 1. Only patterns with compactness
%   (number of points divided by number of points in the region spanned by
%   the pattern) greater than or equal to compactThresh will be output.
%  cardinaThresh is a positive integer: only patterns with cardinality
%   (number of points) greater than or equal to the variable cardinaThresh
%   will be output.
%  regionType is a string, equal to 'lexicographic' or 'convex hull'.
%  topN is a positive integer, specifying how many patterns to return from
%   each projection. For each projection, the topN-rated patterns are
%   combined into a vector of structs and returned as S.

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

% Load dataset representation of the piece of music. The first dimension of
% this file is ontime, counted in crotchet beats from zero. The second
% dimension is MIDI note number (MNN). The third dimension is morphetic
% pitch number (MPN) as defined by Meredith et al. (2002). The fourth
% dimension is duration in crotchets. The fifth dimension is an indication
% of staff (zero for right hand and one for left hand).
D = csvread(fileName,1);
% Create different projections of this dataset (ontime and MNN, ontime and
% MPN, and ontime and duration). Although it is not done here, it is
% possible to run the discovery algorithm on datasets of dimension n > 2.
D1 = unique(D(:,1:2), 'rows');
D2 = unique([D(:,1) D(:,3)], 'rows');
D3 = unique([D(:,1) D(:,4)], 'rows');

% Pattern discovery for ontime and MIDI note number.
[S1, ~, ~] = SIACT(D1, compactThresh, cardinaThresh, regionType);
warning off
S1 = rateOutput(S1, D1, 1);
warning on
N1 = size(S1,2);
S1 = S1(1:min(topN,N1));
[S1(:).projection] = deal('Ontime and MIDI note number');

% Pattern discovery for ontime and morphetic pitch number.
S2 = SIACT(D2, compactThresh, cardinaThresh, regionType);
warning off
S2 = rateOutput(S2, D2, 1);
warning on
N2 = size(S2,2);
S2 = S2(1:min(topN,N2));
[S2(:).projection] = deal('Ontime and morphetic pitch number');

% Pattern discovery for ontime and duration.
S3 = SIACT(D3, compactThresh, cardinaThresh, regionType);
warning off
S3 = rateOutput(S3, D3, 0);
warning on
N3 = size(S3,2);
S3 = S3(1:min(topN,N3));
[S3(:).projection] = deal('Ontime and duration');

% Order by rating overall.
combinedDiscoveries = [S1 S2 S3];
a = [combinedDiscoveries.rating];
[b, m] = sort(a,'descend');
S = combinedDiscoveries(m);

end
