function [SIACToutput, runtime, FRT] = SIACT(D, compactThresh,...
    cardinaThresh, regionType)

% Copyright 2012 Tom Collins

% This function takes a point set representation as its first argument. The
% rest of the arguments, compactThresh, cardinaThresh, and regionType, are
% parameters. The first step of SIACT is to run SIA. This produces a vector
% of structs called S1. Each struct contains a maximal translatable pattern
% (MTP). Each MTP is checked, by calling the function comapctSubpatterns,
% for consecutive subpatterns that have a cardinality (number of points)
% greater than or equal to the variable cardinaThresh, and a compactness
% (number of points divided by number of points in the region spanned by
% the pattern) greater than or equal to the variable compactnessThresh.
% Such subpatterns are included in the returned variable SIACToutput,
% another vector of structs. The runtime and fifth return time (FRT, time
% taken for SIACToutput to reach size 5) are output also.

% REFERENCE
%  Tom Collins, Jeremy Thurlow, Robin Laney, Alistair Willis, and Paul H.
%   Garthwaite. A comparative evaluation of algorithms for discovering
%   translational patterns in Baroque keyboard works. In J. Stephen Downie
%   and Remco Veltkamp (Eds.), Proceedings of the International Symposium
%   on Music Information Retrieval (Utrecht: International Society for
%   Music Information Retrieval, 2010), 3-8.

% INPUT
%  D is a point set, assumed to be in lexicographic order. Otherwise
%   D = sortrows(D);
%   can be used to achieve this. Alternatively,
%   D = unique(D, 'rows');
%   will also remove duplicate datapoints.
%  compactThresh is the compactness threshold. Patterns with compactness
%   greater than this value (in [0, 1]) are returned.
%  cardinaThresh is the points threshold. Patterns with at least this many
%   points in one occurrence are returned.
%  regionType is a string ('lexicographic' or 'convex hull'), indicating
%   which region type should be used by the function compactSubpatterns.

% EXAMPLE
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% compactThresh = .75;
% cardinaThresh = 4;
% regionType = 'lexicographic';
% [SIACToutput, runtime, FRT] = SIACT(D, compactThresh, cardinaThresh,...
%     regionType);

% Run SIA.
tStart = tic;
[S1, ~, ~] = SIA(D);
dimension = size(D,2);
N = size(S1,2);
SIACToutput = repmat(struct('pattern',[],'cardinality',[],...
    'compactness',[],'region',[],'span',[],'regionType',[],...
    'vector',[]),1,N);
i = 1; % Increment over S1.
j = 1; % Increment over SIACToutput.
while i <= N
    % Check each MTP for compact subpatterns.
    S = compactSubpatterns(S1(i).pattern, D, dimension, compactThresh,...
        cardinaThresh, regionType);
    if isfield(S,'pattern');
        % Include the generating vector of the MTP.
        [S(:).vector] = deal(S1(i).vector);
        % Append compact subpatterns to SIACT output.
        k = size(S,2);
        SIACToutput(j:j+k-1) = S;
        j=j+k;
    end
    i=i+1;
end
SIACToutput = SIACToutput(1:j-1);
runtime = toc(tStart);
FRT = runtime;

end
