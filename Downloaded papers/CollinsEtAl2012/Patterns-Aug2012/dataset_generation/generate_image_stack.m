function datasetStruct = generate_image_stack(location, stackSize,...
    nosPts, plotData, fName)

% Copyright 2012 Tom Collins

% Returns a 3-dimensional set of points that represent a stack of
% 2-dimensional images, which have been converted to black-white (zero-one)
% and placed one behind another. The so-called image stack thus created
% will typically contain duplicates, and the aim of subsequent functions is
% to find the identities of these duplicates. In the details field of the
% dataset structure output by this function, there are translators
% variables that encode the duplicate positions.

% INPUT
%  location is a string specifying the location of jpg images for inclusion
%   in the stack.
%  stackSize is the size of the image stack to be created.
%  nosPts is the number of points to be sampled from the black-white image.
%  plotData takes the value one when the image stack should be plotted, and
%   zero otherwise.
%  fName is a string containing the path of where the datasetStruct should
%   be saved. A filename including date stamp is generated automatically.
%   If empty the datasetStruct is not saved.

% EXAMPLE
% s = RandStream('mt19937ar','Seed',1);
% RandStream.setDefaultStream(s);
% params = patterns2012Globals;
% location = params.imagesDatasetsRoot;
% stackSize = 11;
% nosPts = 100;
% fName = [];
% plotData = 1;
% datasetStruct = generate_image_stack(location, stackSize,...
%     nosPts, plotData, fName);

% Load and process images that will be sampled for the stack.
nstep = 10; % Number of times to use interval bisection.
pathAndNames = {};
stackUniqueSet = {};
imagei = 1; % Increment to create pathAndNames and stack.
contents = dir(location); % Image files to sample for stack.
ndir = size(contents, 1);
for idir = 1:ndir
    % If the name folder is one of the following, do nothing.
    if strcmp(contents(idir).name, '.') || ...
            strcmp(contents(idir).name, '..') || ...
            strcmp(contents(idir).name, '.DS_Store') || ...
            strcmp(contents(idir).name, 'stack')
        continue
    end
    % Get parts of path and filename.
    pathAndName = fullfile(location, contents(idir).name, 'jpg',...
        [contents(idir).name '.jpg']);
    pathAndNames{imagei} = pathAndName;
    A = imread(pathAndName);
    level = level_for_im2bw(A, nstep);
    BW = im2bw(A, level);
    % imagesc(BW)
    % colormap('gray')
    BWs = bw2pointset(BW);
    dims = size(BWs);
    ptsIdx = randsample(dims(1), nosPts, false);
    BWr = BWs(ptsIdx, :);
    % plot(BWr(:,2), -BWr(:,1), '.k', 'MarkerSize', 18);
    % axis image
    stackUniqueSet{imagei} = BWr;
    imagei = imagei + 1;
end

% Create the point set.
imagen = imagei - 1;
% repeatIdx = randsample(imagen, nImRep);
sampIdx = randsample(imagen, stackSize, 'true');
pointsOnly = [];
rowi = 1; % Increment to create point set D.
for stacki = 1:stackSize
    Set = stackUniqueSet{sampIdx(stacki)};
    rowj = rowi + size(Set, 1) - 1;
    pointsOnly(rowi:rowj, :) = [repmat(stacki, size(Set, 1), 1) Set];
    rowi = rowj + 1;
end
pointsOnly = unique(pointsOnly, 'rows');

% Determine locations of image duplicates and populate details field of
% datasetStruct with this information.
details = struct([]);
deti = 1; % Increment to create details.
for imagei = 1:imagen
    relIdx = find(imagei == sampIdx);
    m = size(relIdx, 1); % Occurrences.
    if m > 1 % Only record as pattern if image is duplicated.
        Set = stackUniqueSet{imagei};
        L = size(Set, 1); % Cardinality.
        details(deti).pattern = [repmat(relIdx(1), L, 1) Set];
        details(deti).translators = [relIdx - relIdx(1) ...
            zeros(m, 2)];
        details(deti).cardinality = L;
        details(deti).occurrences = m;
        details(deti).imagei = imagei;
        details(deti).name = pathAndNames{imagei};
        deti = deti + 1;
    end
end

% Populate other fields of datasetStruct.
datasetStruct.ID = [randsample('ABCDEFGHIJKLMNOPQRSTUVWXYZ', 5, true) ...
    '-' datestr(clock, 30)];
datasetStruct.k = 3;
datasetStruct.n = rowi - 1;
datasetStruct.G = 'stack';
datasetStruct.SNR = 'no noise';
datasetStruct.details = details;
datasetStruct.pointsOnly = pointsOnly;
datasetStruct.pointsAugm = pointsOnly; % There are no random augmentations.
datasetStruct.targetn = size(details, 2);
% Extra fields specific to an image stack.
datasetStruct.sampIdx = sampIdx;
datasetStruct.pathAndNames = pathAndNames;
datasetStruct.stackUniqueSet = stackUniqueSet;
if fName
    save(fullfile(fName, [datasetStruct.ID '.mat']), 'datasetStruct');
    fprintf('Saved dataset %s to file.\n', datasetStruct.ID);
end

if plotData
    figure
    colormap(gray);
    for stacki = 1:stackSize
        x = stackUniqueSet{sampIdx(stacki)}(:,2);
        y = -stackUniqueSet{sampIdx(stacki)}(:,1);
        plot(x, y, '.k', 'MarkerSize', 18);
        pause(0.5)
    end
end
    
end
