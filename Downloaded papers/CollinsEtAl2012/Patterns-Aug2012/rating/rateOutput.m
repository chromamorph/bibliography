function ratedOutput = rateOutput(S, D, tfPitch)

% 11/5/2011 Copyright Tom Collins

% This function takes a struct S as input, a dataset D, and a logical
% variable tfPitch, indicating if the struct contains information about a
% pattern that consists of pitch material. The pattern is rated (from about
% 1, meaning of low musical importance, to 10, meaning of high musical
% importance) using a perceptually validated model (Collins et al., 2011).

% REFERENCE
% Tom Collins, Robin Laney, Alistair Willis, and Paul H. Garthwaite.
%  (2011). Modeling pattern importance in Chopin's mazurkas. Music
%  Perception, 28(4), 387-414.

% INPUT
% 

% EXAMPLE
% D = [1 1 4; 1 3 5; 2 1 1; 2 2 6; 2 3 2; 3 2 3;
%      6 1 4; 6 3 5; 7 1 1; 7 2 6; 7 3 2; 8 2 3;
%      11 -1 4; 11 0 1; 11 0 9; 11 1 5];
% [S, ~, ~] = SIA(D);
% tfPitch = 1;
% ratedOutput = rateOutput(S, D, tfPitch);

% The parameters (Collins et al., 2011) are as follows:
alpha = 4.277867; % Constant term.
beta1 = 3.422478734; % Coefficient for compactness.
beta2 = -0.038536808; % Coefficient for normExpectedOccurrences.
beta3 = 0.651073171; % Coefficient for compressionRatio.
a = 73.5383283152; % Coefficient for normalising expectedOccurrences.
b = 0.02114878519; % Exponent for normalising expectedOccurrences.

cardD = size(D,1);
dimension = size(D,2);
empiricalMass = count(D(:,2:dimension), 'rows');
% Preallocation.
[S(:).translators] = deal([]);
[S(:).occurrences] = deal([]);
[S(:).coverage] = deal([]);
[S(:).compressionRatio] = deal([]);
[S(:).expectedOccurrences] = deal([]);
[S(:).normExpectedOccurrences] = deal([]);
[S(:).rating] = deal([]);
n = size(S,2);
i = 1; % Increment over the structure that contains the patterns.
while i <= n
    S(i).translators = translatorsOfPatternInDataset(S(i).pattern, D);
    S(i).occurrences = size(S(i).translators,1);
    if ~isfield(S(i), 'cardinality') || isempty(S(i).cardinality)
        S(i).cardinality = size(S(i).pattern, 1);
    end
    L = S(i).cardinality;
    unionOfPoints = zeros(L*S(i).occurrences,dimension);
    j = 1; % Increment over the unionOfPoints.
    while j <= S(i).occurrences
        unionOfPoints((L*(j-1)+1):(L*j),:) = S(i).pattern +...
            repmat(S(i).translators(j,:),L,1);
        j=j+1;
    end
    unionOfPoints = unique(unionOfPoints, 'rows');
    S(i).coverage = size(unionOfPoints,1);
    S(i).compressionRatio = S(i).coverage/...
        (S(i).cardinality + S(i).occurrences - 1);
    if ~isfield(S(i), 'span') || isempty(S(i).span)
        [~, idxStart] = ismember(S(i).pattern(1, :), D, 'rows');
        [~, idxEnd] = ismember(S(i).pattern(L, :), D, 'rows');
        S(i).span = idxEnd - idxStart + 1;
    end
    S(i).expectedOccurrences = expectedOccurrences(S(i).pattern,...
        S(i).cardinality, cardD, dimension, S(i).span, empiricalMass,...
        tfPitch);
    S(i).normExpectedOccurrences = a*S(i).expectedOccurrences^b;
    if ~isfield(S(i), 'compactness') || isempty(S(i).compactness)
        S(i).compactness = L/S(i).span;
    end
    S(i).rating = alpha + beta1*S(i).compactness...
        + beta2*S(i).normExpectedOccurrences + beta3*S(i).compressionRatio;
    i=i+1;
end

% Removing supposedly repeated discoveries.
A = [[S.compactness]' [S.expectedOccurrences]' [S.compressionRatio]'];
[b, m] = unique(A, 'rows');
S = S(m);

% Sort by rating.
a = [S.rating];
[b, m] = sort(a,'descend');
ratedOutput = S(m);

end
