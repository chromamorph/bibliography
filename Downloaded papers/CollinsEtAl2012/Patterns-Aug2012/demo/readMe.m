% Copyright 2012 Tom Collins

% Warning! Please execute this code one chunck at a time (beginning lines
% 52-57), as variables are overwritten later on, and the function
% plotPattern (first called line 61) requires pressing a button on the
% keyboard to browse results.

% This collection of functions is designed to take a piece of music
% represented as a point set as input, and return translational patterns
% that occur within the point set. The output patterns are rated according
% to a perceptually validated model (Collins, Laney, Willis, and
% Garthwatie, 2011), and can be viewed by the user as plots. They should
% correspond to perceptually salient musical patterns, including exact
% repetition, repeitition with interpolation, transposed repeititon (for
% both real and tonal sequences), and durational repetition.

% The first algorithm run is called the Structural Induction Algorithm
% (SIA), described by Meredith, Lemstrom, and Wiggins (2002). It has
% computational complexity O(n^2 log n), and outputs many discovered
% patterns, even for small inputs. Collins, Thurlow, Laney, Willis, and
% Garthwaite (2010) describe an algorithm called a compactness trawler (CT,
% and hence SIACT) for reducing this output, whilst increasing the level of
% agreement between the algorithm's output and the findings of a music
% analyst for certain pieces.

% The computational complexity of SIA/SIACT is reduced with the latest
% algorithm, SIARCT, and additional filters could reduce further the amount
% of output, especially as similar versions of the same pattern tend to
% appear.

% Some example pieces are provided in the same folder as this file
% (including staff notation and MIDI files), to demonstrate the algorithms
% at work. Please try out the code below! Runtime refers to time (seconds)
% in a Matlab environment running on a 2.4 GHz machine with 3.6 GB of RAM.


% Let's start with an excerpt of a piece by Frederic Chopin (1810-1849).
% The arguments in the call to the function discoverRepeatedPatterns are:
% o fileName, specifying which point set (also called dataset) to load.
% o cardinatThresh. Only patterns with cardinality (number of points)
%  greater than or equal to the variable cardinaThresh will be output.
% o compactThresh. Only patterns with compactness (number of points divided
%  by number of points in the 'region' spanned by the pattern) greater than
%  or equal to compactThresh will be output.
% o regionType. There are various definitions of 'region'. This argument
%  accepts one of 'lexicographic' or 'convex hull'.
% o topN. The discovery algorithms run on different versions of the dataset
%  called projections, corresponding to the different types of repetition
%  mentioned above (exact repetition, transposition, durational, etc.). For
%  each projection the topN-rated patterns are combined into a vector of
%  structs and returned as S. The original dataset D is also output.
params = patterns2012Globals;
fileName = fullfile(params.musicDatasetsRoot, 'chopinOp59No1',...
    'CSV_datasets', 'Chopin-op59-no1.txt');
tic
[S, D] = discoverRepeatedPatterns(fileName, 2/3, 5, 'lexicographic', 10);
toc
% Elapsed time is 15.733224 seconds.

% To take a look at the top-rated discovered pattern, execute
plotPattern(S, D, 1)
% and press any button on the keyboard to see the first occurrence
% highlighted. Press a button again to see the second occurrence, and so
% on.

% To take a look at, say, the third-rated discovered pattern, execute
plotPattern(S, D, 3)
% etc.

% Here we look at the same Chopin excerpt, but alter some of the
% parameters.
tic
[S, D] = discoverRepeatedPatterns(fileName, 1, 5, 'convex hull', 10);
toc
% Elapsed time is 32.181570 seconds.

% Again, to take a look at the discovered patterns, execute, say,
plotPattern(S, D, 24)
% and press any button on the keyboard to see each occurrence highlighted.


% Now we move on to an excerpt of a piece by Domenico Scarlatti (1685-
% 1757).
fileName = fullfile(params.musicDatasetsRoot, 'scarlattiL10',...
    'CSV_datasets', 'Scarlatti-L10.txt');
tic
[S, D] = discoverRepeatedPatterns(fileName, 2/3, 5, 'lexicographic', 10);
toc
% Elapsed time is 15.339139 seconds.

% Again, to take a look at the discovered patterns, execute, say,
plotPattern(S, D, 3)
% and press any button on the keyboard to see each occurrence highlighted.

% Here we look at the same Scarlatti excerpt, but alter some of the
% parameters.
tic
[S, D] = discoverRepeatedPatterns(fileName, 1, 5, 'convex hull', 10);
toc
% Elapsed time is 29.471385 seconds.

% Again, to take a look at the discovered patterns, execute, say,
plotPattern(S, D, 28)
% and press any button on the keyboard to see each occurrence highlighted.


% Finally, we move on to an excerpt from a song called 'Hey there Delilah'
% by the Plain White T's (2005). The code takes a little longer to run, as
% it is a larger excerpt.
fileName = fullfile(params.musicDatasetsRoot, 'heyThereDelilah',...
    'CSV_datasets', 'Hey-there-Delilah.txt');
tic
[S, D] = discoverRepeatedPatterns(fileName, 2/3, 5, 'lexicographic', 10);
toc
% Elapsed time is 53.122223 seconds.

% Again, to take a look at the discovered patterns, execute, say,
plotPattern(S, D, 3)
% and press any button on the keyboard to see each occurrence highlighted.


% MAIN REFERENCES

% Tom Collins. Computational models of translational pattern discovery.
% Submitted, 2012.

% Tom Collins. Improved methods for pattern discovery in music, with
% applications in automated stylistic composition. PhD thesis, Faculty of
% Mathematics, Computing and Technology, The Open University, 2011.
% Available from http://oro.open.ac.uk/30103/

% ALSO RELEVANT

% Tom Collins, Robin Laney, Alistair Willis, and Paul H. Garthwaite.
% Modeling pattern importance in Chopin's mazurkas. In Music Perception,
% 28(4):387-414, 2011.

% Tom Collins, Jeremy Thurlow, Robin Laney, Alistair Willis, and Paul H.
% Garthwaite. A comparative evaluation of algorithms for discovering
% translational patterns in Baroque keyboard works. In J. Stephen Downie
% and Remco Veltkamp, editors, Proceedings of the International Symposium
% on Music Information Retrieval, pages 3-8, Utrecht, 2010. International
% Society for Music Information Retrieval. Available from
% http://oro.open.ac.uk/21837/

% David Meredith, Kjell Lemstrom, and Geraint A. Wiggins. Algorithms for
% discovering repeated patterns in multidimensional representations of
% polyphonic music. Journal of New Music Research, 31(4):321-345, 2002.

% Esko Ukkonen, Kjell Lemstrom, and Veli Makinen. Geometric algorithms for
% transposition invariant content-based music retrieval. In Holger H. Hoos
% and David Bainbridge, editors, Proceedings of the International Symposium
% on Music Information Retrieval, pages 193-199, Baltimore, MD.
% International Society for Music Information Retrieval.
