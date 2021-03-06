function [SIARCT_CFPoutput, runtime, FRT] = SIARCT_CFP(D, r,...
  compactThresh, cardinaThresh, regionType, similarThresh, similarFunc,...
  similarParam, ratingField, SIARCToutput, SIARCTruntime, SIARCT_FRT)

% Copyright Tom Collins 22/7/2013

% Given a dataset D, this function:
% (1) Runs the SIARCT algorithm, returning compact subsets of maximal
%  translatable patterns (MTP) that occur in D, and for which a conjugate
%  generating vector lies on or within the first r superdiagonals of the
%  similarity array for D;
% (2) Categorises the patterns into exemplars and other patterns that have
%  higher-than-threshold similarity to an exemplar;
% (3) Finds other inexact occurrences of each exemplar in the point set.

% It is assumed that D is in lexicographic order. Otherwise
% D = sortrows(D);
% can be used to achieve this. Alternatively,
% D = unique(D, 'rows');
% will also remove duplicate datapoints.

% INPUT
%  D is an n x k matrix representing a k-dimensional set of n points.
%  r is a positive integer between 1 and n - 1, giving the number of
%   superdiagonals of the similarity array for D that will be used.
%  compactThresh is a parameter in (0, 1], giving the minimum compactness a
%   pattern occurrence must have in order to be included in the output.
%  cardinaThresh is a positive integer parameter, giving the minimum number
%   points that compactness a pattern occurrences must have in order to be
%   included in the output.
%  regionType is a string equal to 'lexicographic' or 'convex hull',
%   indicating which definition of region should be used for calculating
%   the compactness of patterns.
%  similarThresh is a value in [0, 1). If the similarity of the current
%   highest-rated pattern S_in(i) and some other pattern S_in(j) is greater
%   than this threshold, then S_in(j) will be categorised as an instance of
%   the exemplar S_in(i). Otherwise S_in(j) may become an exemplar in a
%   subsequent step.
%  similarFunc is a string indicating which function should be used for
%   calculating the symbolic music similarity, either 'cardinality score'
%   or 'normalised matching score'.
%  similarParam is an optional argument. If similarFunc = 'cardinality
%   score', then similarParam takes one of two values (one if calculation
%   of cardinality score allows for translations, and zero otherwise). If
%   similarFunc = 'normalised matching score', then similarParam takes a
%   string value ('normal', 'pitchindependent',
%   'tempoindependent', or 'tempoandpitchindependent', see fpgethistogram2
%   for details).
%  ratingField is an optional string indicating which field of each struct
%   in S should be used to order the repeated patterns.
%  SIARCToutput is an optional argument, containing the output of the
%   SIARCT algorithm (vector of structs).
%  SIARCTruntime is an optional argument, giving the runtime in seconds of
%   the SIARCT algorithm.
%  SIARCT_FRT is an optional argument, giving the fifth return time in
%   seconds of the SIARCT algorithm.

% Handle assignment of optional arguments.
tStart = tic;
if nargin < 10
  [SIARCToutput, SIARCTruntime, SIARCT_FRT] = SIARCT(D, r,...
    compactThresh, cardinaThresh, regionType);
end
if nargin < 9
  ratingField = 'rating';
end
if nargin < 8
  if strcmp(similarFunc, 'cardinality score')
    similarParam = 0;
  else
    similarParam = 'normal';
  end
end

% Catgeorise the score matrix for the SIARCT output, in an attempt to
% reduce output. NB, scoreMatrix.m applies a sqrt to the match score
% histogram, so the similarity threshold is really .25.
S_mid = categoriseRatedPatternsBySimilarity(SIARCToutput, ratingField,...
  [], similarThresh, similarFunc, similarParam);

% Now include the inexact occurrences as well.
SIARCT_CFPoutput = includeInexactOccurrences(S_mid, D, similarThresh,...
  similarFunc, similarParam);

runtime = SIARCTruntime + toc(tStart);
FRT = SIARCT_FRT + toc(tStart);

end
