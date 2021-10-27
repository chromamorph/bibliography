% Copyright 2012 Tom Collins

% Regression analyses for precision, recall, runtime, FRT.
% We focus on G shape (generating method: shape) and allow SNR to vary.
% Have to import and manipulate raw results because of taking log
% precision.

% ?? Obsolete. Style.
FontName = 'Helvetica';
FontSize = 14;
colors = {'r' 'b' 'g'};

% Load the analysis results.
projectRoot = fullfile('/home', 'tommyc', 'projects', 'patterns2012');
dataRoot = fullfile(projectRoot, 'data');
dataRootAbstract = fullfile(dataRoot, 'abstract');
% dataRootImages =  fullfile(dataRoot, 'images', 'stack');
resultsRoot = fullfile(projectRoot, 'results');

%% Abstract shape data.
% For log precision and runtime the regression equation is
% y = a + b1.SIAR + b2.SIARCT + b3.size + b4.SNR + b5.SIAR.size ...
%       + b6.SIARCT.size + b7.SIAR.SNR + b8.SIARCT.SNR.
% For log runtime the regression equation is
% y = a + b1.SIAR + b2.SIARCT + b3.SIAR_FRT + b4.SIARCT_FRT + b5.size ...
%       + b6.SNR.
results = load(fullfile(resultsRoot, 'abstract-20120221T102059.mat'));
% These are the rows of the averages table that contain results for G =
% shape, and SNR = decreasing.
relAvIdx = 3:4:71;
% Old version where data for trying to keep ratio of pattern points to set
% size constant is included. Simpler (for explanation purposes) to leave
% out, because it's not included in the plots and the regression results
% are similar:
% relAvIdx = 1:2:71;
relAvIdxn = size(relAvIdx, 2);
relIdx = zeros(20*relAvIdxn, 1);
for relAvIdxi = 1:relAvIdxn
    rowStart = 20*(relAvIdx(relAvIdxi) - 1) + 2;
    rowEnd = 20*relAvIdx(relAvIdxi) + 1;
    relIdx(20*(relAvIdxi-1)+1:20*relAvIdxi) = rowStart:rowEnd;
end
nData = size(relIdx, 1);
% Include FRT data from SIAR and SIARCT in the runtime ANOVA.
nData_rt = 5/3*nData; 
% Response vectors.
y_prec = zeros(nData, 1);
y_rec = zeros(nData, 1);
y_rt = zeros(nData_rt, 1);
expvars = {'SIAR' 'SIARCT' 'size' 'SNR' 'SIAR.size' 'SIARCT.size' ...
    'SIAR.SNR' 'SIARCT.SNR'};
expvars_rt =...
    {'SIAR' 'SIARCT' 'SIAR_FRT' 'SIARCT_FRT' 'size' 'SNR' 'SIAR.size' ...
    'SIARCT.size' 'SIAR_FRT.size' 'SIARCT_FRT.size' 'SIAR.SNR' ...
    'SIARCT.SNR' 'SIAR_FRT.SNR' 'SIARCT_FRT.SNR'};
ncol = size(expvars, 2);
ncol_rt = size(expvars_rt, 2);
% Design matrix (no column of ones necessary for regstats).
X = zeros(nData, ncol);
X_rt = zeros(nData_rt, ncol_rt);
% Index x corresponds to raw results rows (20*(x - 1) + 1):20*x.
% nLabels = 1:.2:2;
% nn = size(nLabels, 2);
% algLabels = {'SIA', 'SIAR', 'SIARCT'};
% na = size(algLabels, 2);

jData = 1; % Increment over runtime variables.
for iData = 1:nData
    % Logicals to indicate that we are handling SIAR or SIARCT stats.
    SIAR_tf = ~isempty(regexp(...
        char(results.table_all{relIdx(iData), 2}), 'SIAR,', 'once'));
    SIARCT_tf = ~isempty(regexp(...
        char(results.table_all{relIdx(iData), 2}), 'SIARCT,', 'once'));
    % Log precision (response variable).
    y_prec(iData) = log(results.table_all{relIdx(iData), 3});
    % Recall (response variable).
    y_rec(iData) = results.table_all{relIdx(iData), 4};
    % Log runtime (response variable).
    y_rt(jData) = log(results.table_all{relIdx(iData), 5});
    if SIAR_tf || SIARCT_tf
        y_rt(jData+1) = log(results.table_all{relIdx(iData), 6});
    end
    % Indicator variable for SIAR.
    icol = 1;
    if SIAR_tf
        X(iData, icol) = 1;
        X_rt(jData, icol) = 1;
        X_rt(jData+1, 3) = 1; % Column 3 for SIAR_FRT.
    end
    % Indicator variable for SIARCT.
    icol = 2;
    if SIARCT_tf
        X(iData, icol) = 1;
        X_rt(jData, icol) = 1;
        X_rt(jData+1, 4) = 1; % Column 4 for SIARCT_FRT.
    end
    % Now load datasetStruct to obtain point set size and SNR.
    ID = results.table_all{relIdx(iData), 1};
    datasetStruct = load(fullfile(dataRootAbstract, [ID '.mat']));
    datasetStruct = datasetStruct.datasetStruct;
    signal = size(datasetStruct.pointsOnly, 1);
    signal_and_noise = datasetStruct.n;
    SNR = signal/(signal_and_noise - signal);
    logSNR = log(SNR);
    % Size of point set.
    icol = 3;
    X(iData, icol) = signal_and_noise;
    X_rt(jData, 5) = signal_and_noise; % Column 5 for size.
    if SIAR_tf || SIARCT_tf
        X_rt(jData+1, 5) = signal_and_noise; % Column 5 for size.
    end
    % Signal-to-noise ratio.
    icol = 4;
    X(iData, icol) = logSNR;
    X_rt(jData, 6) = logSNR; % Col 6 for SNR.
    if SIAR_tf || SIARCT_tf
        X_rt(jData+1, 6) = logSNR; % Col 6 for SNR.
    end
    % Interaction of SIAR and size.
    icol = 5;
    X(iData, icol) = SIAR_tf*signal_and_noise;
    X_rt(jData, 7) = SIAR_tf*signal_and_noise;
    if SIAR_tf
        X_rt(jData+1, 9) = SIAR_tf*signal_and_noise;
    end
    % Interaction of SIARCT and size.
    icol = 6;
    X(iData, icol) = SIARCT_tf*signal_and_noise;
    X_rt(jData, 8) = SIARCT_tf*signal_and_noise;
    if SIARCT_tf
        X_rt(jData+1, 10) = SIARCT_tf*signal_and_noise;
    end
    % Interaction of SIAR and logSNR.
    icol = 7;
    X(iData, icol) = SIAR_tf*logSNR;
    X_rt(jData, 11) = SIAR_tf*logSNR;
    if SIAR_tf
        X_rt(jData+1, 13) = SIAR_tf*logSNR;
    end
    % Interaction of SIARCT and logSNR.
    icol = 8;
    X(iData, icol) = SIARCT_tf*logSNR;
    X_rt(jData, 12) = SIARCT_tf*logSNR;
    if SIARCT_tf
        X_rt(jData+1, 14) = SIARCT_tf*logSNR;
    end
    % Increment jData appropriately.
    if SIAR_tf || SIARCT_tf
        jData = jData + 2;
    else
        jData = jData + 1;
    end
end

% For precision results, I want to regress log precision on algorithm
% (indicator variables for SIAR and SIACT, with SIA in the baseline), point
% set size (1000, 1200,..., 2000), SNR of point set, and first-order
% interactions.
model_prec = regstats(y_prec, X, 'linear',...
    {'rsquare','beta','r','mse','tstat','fstat'});
% Test the SIARCT - SIAR contrast.
CV = inv([ones(nData, 1) X]'*[ones(nData, 1) X]);
s = sqrt(model_prec.mse);
dfe = model_prec.tstat.dfe;
b_SIAR = model_prec.tstat.beta(2);
b_SIARCT = model_prec.tstat.beta(3);
t = (b_SIARCT - b_SIAR)/(s*sqrt(CV(2,2) + CV(3,3) - 2*CV(2,3)));
p = 2*(1 - tcdf(t, dfe));
% plot(X(:, 4), y_prec, '.');

% For recall results, I want to regress recall on algorithm (indicator
% variables for SIAR and SIACT, with SIA in the baseline), point set size
% (1000, 1200,..., 2000), SNR of point set, and first-order interactions.
model_rec = regstats(y_rec, X, 'linear',...
    {'rsquare','beta','r','mse','tstat','fstat'});
% Test the SIARCT - SIAR contrast.
s = sqrt(model_rec.mse);
dfe = model_rec.tstat.dfe;
b_SIAR = model_rec.tstat.beta(2);
b_SIARCT = model_rec.tstat.beta(3);
t = (b_SIARCT - b_SIAR)/(s*sqrt(CV(2,2) + CV(3,3) - 2*CV(2,3)));
p = 2*(1 - tcdf(abs(t), dfe));

% For runtime results, I want to regress log runtime on algorithm
% (indicator variables for SIAR, SIACT, SIAR_FRT, and SIARCT_FRT with SIA
% in the baseline), point set size (1000, 1200,..., 2000), and SNR of point
% set. Note, first-order interactions have been removed.
model_rt = regstats(y_rt, X_rt(:,1:6), 'linear',...
    {'rsquare','beta','r','mse','tstat','fstat'});
% plot(X_rt(:, 3), y_rt, '.'); xlim([-0.1 1.1])


%% Image data.
% For log precision and runtime the regression equation is
% y = a + b1.SIAR + b2.SIARCT.
% For log runtime the regression equation is
% y = a + b1.SIAR + b2.SIARCT + b3.SIAR_FRT + b4.SIARCT_FRT.
results = load(fullfile(resultsRoot, 'images-20120328T025059.mat'));
% These are the rows of the averages table that contain results for G =
% shape, and SNR = decreasing.
relAvIdx = [1 2 4];
relAvIdxn = size(relAvIdx, 2);
relIdx = zeros(20*relAvIdxn, 1);
for relAvIdxi = 1:relAvIdxn
    rowStart = 20*(relAvIdx(relAvIdxi) - 1) + 2;
    rowEnd = 20*relAvIdx(relAvIdxi) + 1;
    relIdx(20*(relAvIdxi-1)+1:20*relAvIdxi) = rowStart:rowEnd;
end
nData = size(relIdx, 1);
% Include FRT data from SIAR and SIARCT in the runtime ANOVA.
nData_rt = 5/3*nData; 
% Response vectors.
y_prec = zeros(nData, 1);
y_rec = zeros(nData, 1);
y_rt = zeros(nData_rt, 1);
expvars = {'SIAR' 'SIARCT'};
expvars_rt =...
    {'SIAR' 'SIARCT' 'SIAR_FRT' 'SIARCT_FRT'};
ncol = size(expvars, 2);
ncol_rt = size(expvars_rt, 2);
% Design matrix (no column of ones necessary for regstats).
X = zeros(nData, ncol);
X_rt = zeros(nData_rt, ncol_rt);
% Index x corresponds to raw results rows (20*(x - 1) + 1):20*x.
% nLabels = 1:.2:2;
% nn = size(nLabels, 2);
% algLabels = {'SIA', 'SIAR', 'SIARCT'};
% na = size(algLabels, 2);

jData = 1; % Increment over runtime variables.
for iData = 1:nData
    % Logicals to indicate that we are handling SIAR or SIARCT stats.
    SIAR_tf = ~isempty(regexp(...
        char(results.table_all{relIdx(iData), 2}), 'SIAR,', 'once'));
    SIARCT_tf = ~isempty(regexp(...
        char(results.table_all{relIdx(iData), 2}), 'SIARCT,', 'once'));
    % Log precision (response variable).
    y_prec(iData) = log(results.table_all{relIdx(iData), 3});
    % Recall (response variable).
    y_rec(iData) = results.table_all{relIdx(iData), 4};
    % Log runtime (response variable).
    y_rt(jData) = log(results.table_all{relIdx(iData), 5});
    if SIAR_tf || SIARCT_tf
        y_rt(jData+1) = log(results.table_all{relIdx(iData), 6});
    end
    % Indicator variable for SIAR.
    icol = 1;
    if SIAR_tf
        X(iData, icol) = 1;
        X_rt(jData, icol) = 1;
        X_rt(jData+1, 3) = 1; % Column 3 for SIAR_FRT.
    end
    % Indicator variable for SIARCT.
    icol = 2;
    if SIARCT_tf
        X(iData, icol) = 1;
        X_rt(jData, icol) = 1;
        X_rt(jData+1, 4) = 1; % Column 4 for SIARCT_FRT.
    end
    % Increment jData appropriately.
    if SIAR_tf || SIARCT_tf
        jData = jData + 2;
    else
        jData = jData + 1;
    end
end

% For precision results, I want to regress log precision on algorithm
% (indicator variables for SIAR and SIACT, with SIA in the baseline).
model_prec = regstats(y_prec, X, 'linear',...
    {'rsquare','beta','r','mse','tstat','fstat'});
% Test the SIARCT - SIAR contrast.
CV = inv([ones(nData, 1) X]'*[ones(nData, 1) X]);
s = sqrt(model_prec.mse);
dfe = model_prec.tstat.dfe;
b_SIAR = model_prec.tstat.beta(2);
b_SIARCT = model_prec.tstat.beta(3);
t = (b_SIARCT - b_SIAR)/(s*sqrt(CV(2,2) + CV(3,3) - 2*CV(2,3)));
p = 2*(1 - tcdf(t, dfe));

% For recall results, I want to regress recall on algorithm (indicator
% variables for SIAR and SIACT, with SIA in the baseline).
model_rec = regstats(y_rec, X, 'linear',...
    {'rsquare','beta','r','mse','tstat','fstat'});
% Test the SIARCT - SIAR contrast.
s = sqrt(model_rec.mse);
dfe = model_rec.tstat.dfe;
b_SIAR = model_rec.tstat.beta(2);
b_SIARCT = model_rec.tstat.beta(3);
t = (b_SIARCT - b_SIAR)/(s*sqrt(CV(2,2) + CV(3,3) - 2*CV(2,3)));
p = 2*(1 - tcdf(abs(t), dfe));

% For runtime results, I want to regress log runtime on algorithm
% (indicator variables for SIAR, SIACT, SIAR_FRT, and SIARCT_FRT with SIA
% in the baseline).
model_rt = regstats(y_rt, X_rt, 'linear',...
    {'rsquare','beta','r','mse','tstat','fstat'});
