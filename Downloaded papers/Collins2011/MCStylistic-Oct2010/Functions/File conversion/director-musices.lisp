#| Copyright 2008-2011 Tom Collins
   Tuesday 17 November 2009

Director musices is a music file format. It is not as
common as kern or musicXML, but it uses a list format,
which makes it amenable to Lisp import using only a
few functions. As far as I can tell, the director
musices format does not handle multiple voices on one
stave. Some of the functions here, like
MIDI-morphetic-pair2pitch\&octave, are called by
music-import and export functions for different
formats. |#

; REQUIRED PACAKGES
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/list-processing.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Pattern rating/projection.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/File conversion/text-files.lisp"))

#|
\noindent Example:
\begin{verbatim}
(check-pitch&octave "C3")
--> "C3"
\end{verbatim}

\noindent This function tests whether a supplied
pitch\&octave is in an acceptable format and range. I
was intending to allow pitches from C0 to C8 (MNNs 12
to 108) but this function will not allow C8, so could
be adjusted in future. If acceptable the pitch\&octave
is returned, and nil otherwise. |#

(defun check-pitch&octave
       (a-string &optional (l (length a-string)))
  (if (and
       (> (length a-string) 1)
       (find
	(subseq a-string 0 (- l 1))
	'("B#" "C" "Dbb" "B##" "C#" "Db" "C##" "D"
	  "Ebb" "D#" "Eb" "D##" "E" "Fbb" "E#" "F"
	  "Gbb" "E##" "F#" "Gb" "F##" "G" "Abb" "G#"
	  "Ab" "G##" "A" "Bbb" "Cbb" "A#" "Bb" "A##"
	  "B" "Cb") :test #'string=)
       (parse-integer
	(subseq a-string (- l 1) l)
	:junk-allowed t)
       (>=
	(parse-integer
	(subseq a-string (- l 1) l)
	:junk-allowed t) 0)
       (<=
	(parse-integer
	(subseq a-string (- l 1) l)
	:junk-allowed t) 7))
    (identity a-string) ()))

#|
\noindent Example:
\begin{verbatim}
(director-musice2datapoint
 7 1
 '(bar 1 n ("C3" 1/2) key "C" modus "maj" mm 192
   meter (2 2))
 3 "C3" 1/2)
--> (7 48 53 1/2 1 nil)
\end{verbatim}

\noindent This function converts one line of a
director musices file into a datapoint consisting of
ontime, MIDI note number, morphetic pitch number,
duration, stave, and T if the note is tied over. |#

(defun director-musice2datapoint
       (ontime stave dm-note index
	pitches duration &optional
	(pitches
	 (if (listp pitches)
	   (identity pitches) (list pitches)))
	(MIDI-morphetic-pair
	 (if (first pitches)
	   (pitch&octave2MIDI-morphetic-pair
	    (first pitches)))))
  (if (null (first pitches)) ()
    (cons
     (list
      ontime
      (first MIDI-morphetic-pair)
      (second MIDI-morphetic-pair)
      duration
      stave
      (if (equalp (length pitches) 1)
	(string= (nth (+ index 1) dm-note) 'tie)))
     (director-musice2datapoint
      ontime stave dm-note index (rest pitches)
      duration))))

#|
\noindent Example:
\begin{verbatim}
(director-musices2dataset
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/scarlatti-L1-bars11-13.dm"))
--> ((0 79 71 1 1) (0 86 75 7/3 0) (1 43 50 1 1)
     (2 38 47 2 1) (7/3 84 74 1/3 0) (8/3 83 73 1/3 0)
     (3 81 72 1/3 0) (10/3 83 73 1/3 0)
     (11/3 84 74 1/3 0) (4 79 71 1 1) (4 83 73 1/3 0)
     (13/3 84 74 1/3 0) (14/3 86 75 1/3 0)
     (5 43 50 1 1) (5 86 75 4/3 0) (6 38 47 2 1)
     (19/3 84 74 1/3 0) (20/3 83 73 1/3 0)
     (7 81 72 1/3 0) (22/3 83 73 1/3 0)
     (23/3 84 74 1/3 0) (8 79 71 1 1) (8 83 73 1/3 0))
\end{verbatim}

\noindent This function converts a piece of music
represented in the director-musices format into a
dataset where each datapoint consists of an ontime,
MIDI note number, morphetic pitch number, duration and
stave. |#

(defun director-musices2dataset (path&name)
  (resolve-ties
   (director-musices2dataset-chunked
    path&name)))

#|
\noindent Example:
\begin{verbatim}
(director-musices2dataset-chunked
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/scarlatti-L1-bars11-13.dm"))
--> ((0 86 75 2 0 T) (2 86 75 1/3 0 NIL)
     (7/3 84 74 1/3 0 NIL) (8/3 83 73 1/3 0 NIL)
     (3 81 72 1/3 0 NIL) (10/3 83 73 1/3 0 NIL)
     (11/3 84 74 1/3 0 NIL) (4 83 73 1/3 0 NIL)
     (13/3 84 74 1/3 0 NIL) (14/3 86 75 1/3 0 NIL)
     (5 86 75 1 0 T) (6 86 75 1/3 0 NIL)
     (19/3 84 74 1/3 0 NIL) (20/3 83 73 1/3 0 NIL)
     (7 81 72 1/3 0 NIL) (22/3 83 73 1/3 0 NIL)
     (23/3 84 74 1/3 0 NIL) (8 83 73 1/3 0 NIL)
     (0 79 71 1 1 NIL) (1 43 50 1 1 NIL)
     (2 38 47 2 1 NIL) (4 79 71 1 1 NIL)
     (5 43 50 1 1 NIL) (6 38 47 2 1 NIL)
     (8 79 71 1 1 NIL))
\end{verbatim}

\noindent This function converts a piece of music
represented in the director-musices format into a
chunked dataset, chunked in the sense that ties still
have to be resolved. |#

(defun director-musices2dataset-chunked
       (path&name &optional
	(dm-notes
	 (read-from-file path&name))
	(result nil)
	(ontime 0)
	(stave -1)
	(dm-note (first dm-notes))
	(index
	 (if (and (listp dm-note) dm-note)
	   (if (string= (first dm-note) 'n)
	     (identity 1) (identity 3))))
	(pitches
	 (if index
	   (first (nth index dm-note))))
	(duration
	 (if index
	     (* 4 (second (nth index dm-note))))))
  (if (null dm-notes)
    (identity result)
    (if index
      (director-musices2dataset-chunked
       path&name (rest dm-notes)
       (append
	result
	(director-musice2datapoint
	 ontime stave dm-note index pitches duration))
       (+ ontime duration) stave)
      (director-musices2dataset-chunked
       path&name (rest (rest dm-notes))
       result 0 (+ stave 1)))))

#|
\noindent Example:
\begin{verbatim}
(guess-morphetic-in-C-major 68)
--> 65
\end{verbatim}

\noindent This function takes a MIDI note number as
its only argument. It attempts to guess (very naively)
the corresponding morphetic pitch number, assuming a
key of or close to C major. |#

(defun guess-morphetic-in-C-major
       (y &optional
        (octave (floor (- (/ y 12) 1)))
        (MIDI-residue (- y (* 12 (+ octave 1))))
        (morphetic-residue
         (second
	  (assoc
	   MIDI-residue
	   '((0 0) (1 0) (2 1) (3 2) (4 2) (5 3) (6 3)
             (7 4) (8 5) (9 5) (10 6) (11 6))
           :test #'equalp))))
  (+ morphetic-residue (+ (* 7 octave) 32)))

#|
\noindent Example:
\begin{verbatim}
(index-of-1st-tie
 '((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)))
--> 0
\end{verbatim}

\noindent This function returns the index of the first
element of a list of lists whose last value
(indicating a tie) is T. |#

(defun index-of-1st-tie
       (chunked-dataset &optional (index 0))
  (if (null chunked-dataset) ()
    (if (my-last (first chunked-dataset))
      (identity index)
      (index-of-1st-tie
       (rest chunked-dataset) (+ index 1)))))

#|
\noindent Example:
\begin{verbatim}
(indices-of-ties
 '((4 62 61 1 0 T) (5 62 61 1 0 T)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)
   (6 62 61 1/4 0 NIL)) 0)
--> (1 8)
\end{verbatim}

\noindent This function returns the indices of
elements that have the same MIDI-morphetic pairs as
the element indicated by the second argument, so long
as these elements continue to be tied. This function
may not be robust enough: replacing the last MNN by 63
results in an infinite loop. |#

(defun indices-of-ties
       (chunked-dataset index &optional
	(j (+ index 1))
	(MIDI-note-number
	 (second (nth index chunked-dataset)))
	(morphetic-pitch-number
	 (third (nth index chunked-dataset)))
	(indices nil))
  (if (and
       (equalp
	(second (nth j chunked-dataset))
	MIDI-note-number)
       (equalp
	(third (nth j chunked-dataset))
	morphetic-pitch-number))
    (if (sixth (nth j chunked-dataset))
      (indices-of-ties
       chunked-dataset index (+ j 1)
       MIDI-note-number morphetic-pitch-number
       (append indices (list j)))
      (append indices (list j)))
    (indices-of-ties
     chunked-dataset index (+ j 1)
     MIDI-note-number morphetic-pitch-number
     indices)))

#|
\noindent Example:
\begin{verbatim}
(MIDI-morphetic-pair2pitch&octave '(70 65))
--> "A#4"
\end{verbatim}

\noindent This function returns the pitch and octave
of an input MIDI note number and morphetic pitch
number. |#

(defun MIDI-morphetic-pair2pitch&octave
       (MIDI-morphetic-pair &optional
	(octave
	 (floor
	  (/ (- (second MIDI-morphetic-pair) 32) 7)))
	(MIDI-residue
	 (- (first MIDI-morphetic-pair)
	    (* 12 (+ octave 1))))
	(morphetic-residue
	 (- (second MIDI-morphetic-pair)
	    (+ (* 7 octave) 32)))
	(pitch
	 (second
	  (assoc
	   (list MIDI-residue morphetic-residue)
	   '(((12 6) "B#") ((0 0) "C") ((0 1) "Dbb")
	     ((13 6) "B##") ((1 0) "C#") ((1 1) "Db")
	     ((2 0) "C##") ((2 1) "D") ((2 2) "Ebb" )
	     ((3 1) "D#") ((3 2) "Eb") ((4 1) "D##")
	     ((4 2) "E") ((3 3) "Fbb") ((5 2) "E#")
	     ((5 3) "F") ((5 4) "Gbb") ((6 2) "E##")
	     ((6 3) "F#") ((6 4) "Gb") ((7 3) "F##")
	     ((7 4) "G") ((7 5) "Abb") ((8 4) "G#")
	     ((8 5) "Ab") ((9 4) "G##") ((9 5) "A")
	     ((9 6) "Bbb") ((-2 0) "Cbb")
	     ((10 5) "A#") ((10 6) "Bb")
	     ((11 5) "A##") ((11 6) "B")
	     ((-1 0) "Cb")) :test #'equalp))))
  (if pitch
    (concatenate
     'string
     pitch
     (write-to-string octave))))

#|
\noindent Example:
\begin{verbatim}
(modify-to-check-dataset
 '((0 50 54 5/4 1) (5/4 52 55 1/8 1)
   (11/8 54 56 1/8 1) (3/2 55 57 1/8 1)
   (13/8 57 58 1/8 1) (7/4 59 59 1/8 1))) 
--> ((0 50 1250 1 90) (1250 52 125 1 90)
     (1375 54 125 1 90) (1500 55 125 1 90)
     (1625 57 125 1 90) (1750 59 125 1 90))
\end{verbatim}

\noindent This function converts standard vector
representation to events for saving as a MIDI file. |#

(defun modify-to-check-dataset
       (dataset &optional
        (scale 1000) (channel 1) (velocity 90))
  (if (null dataset) ()
    (cons
     (append
      (list (* scale (first (first dataset)))
	    (second (first dataset))
	    (* scale (fourth (first dataset))))
      (list channel velocity))
     (modify-to-check-dataset
      (rest dataset) scale channel velocity))))

#|
\noindent Example:
\begin{verbatim}
(pitch&octave2MIDI-morphetic-pair "A#4")
--> (70 65)
\end{verbatim}

\noindent This function returns the MIDI note number
and morphetic pitch number of an input pitch and
octave. |#

(defun pitch&octave2MIDI-morphetic-pair
       (pitch&octave &optional
	(length-arg (length pitch&octave))
	(pitch
	 (subseq
	  pitch&octave
	  0 (- length-arg 1)))
	(octave
	 (parse-integer
	  (subseq
	   pitch&octave
	   (- length-arg 1))))
	(pair
	 (second
	  (assoc
	   pitch
	   '(
	     ("B#" (12 6)) ("C" (0 0)) ("Dbb" (0 1))
	     ("B##" (13 6)) ("C#" (1 0)) ("Db" (1 1))
	     ("C##" (2 0)) ("D" (2 1)) ("Ebb" (2 2))
	     ("D#" (3 1)) ("Eb" (3 2)) ("Fbb" (3 3))
             ("D##" (4 1)) ("E" (4 2)) ("Fb" (4 3))
	     ("E#" (5 2)) ("F" (5 3)) ("Gbb" (5 4))
             ("E##" (6 2)) ("F#" (6 3)) ("Gb" (6 4))
             ("F##" (7 3)) ("G" (7 4)) ("Abb" (7 5))
             ("G#" (8 4)) ("Ab" (8 5))
             ("G##" (9 4)) ("A" (9 5)) ("Bbb" (9 6))
             ("Cbb" (-2 0)) ("A#" (10 5))
             ("Bb" (10 6))
	     ("A##" (11 5)) ("B" (11 6))
	     ("Cb" (-1 0))) :test #'string=))))
  (list
   (+ (* 12 (+ octave 1)) (first pair))
   (+ (+ (* 7 octave) 32) (second pair))))

#|
\noindent Example:
\begin{verbatim}
(resolve-tie
 '((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)))
--> ((4 62 61 5/4 0 NIL) (21/4 64 62 1/8 0 NIL)
     (43/8 66 63 1/8 0 NIL) (11/2 67 64 1/8 0 NIL)
     (45/8 69 65 1/8 0 NIL) (23/4 71 66 1/8 0 NIL)
     (47/8 73 67 1/8 0 NIL))
\end{verbatim}

\noindent This function locates notes relevant to a
tie, creates a single appropriately defined note, and
removes the redundant notes. |#

(defun resolve-tie
       (chunked-dataset &optional
	(index
	 (index-of-1st-tie chunked-dataset))
	(tied-forward
	 (if index
	   (nth index chunked-dataset)))
	(relevant-indices
	 (if index
	   (cons
	    index
	    (indices-of-ties
	     chunked-dataset index))))
	(tied-back
	 (if index
	   (nth (my-last relevant-indices)
		chunked-dataset))))
  (if index
    (cons
     (append
      (firstn 3 tied-forward)
      (list
       (- (+ (first tied-back) (fourth tied-back))
	  (first tied-forward)))
      (list (fifth tied-forward) nil))
     (remove-nth-list
      relevant-indices chunked-dataset
      (reverse relevant-indices)))
    (identity "all ties resolved")))

#|
\noindent Example:
\begin{verbatim}
(resolve-ties
 '((4 62 61 1 0 T) (5 62 61 1/4 0 NIL)
   (5 74 68 1 0 T)
   (21/4 64 62 1/8 0 NIL) (43/8 66 63 1/8 0 NIL)
   (11/2 67 64 1/8 0 NIL) (45/8 69 65 1/8 0 NIL)
   (23/4 71 66 1/8 0 NIL) (47/8 73 67 1/8 0 NIL)
   (6 74 68 1 0 T) (7 74 68 1/4 0 NIL)))
--> ((4 62 61 5/4 0) (5 74 68 9/4 0)
     (21/4 64 62 1/8 0) (43/8 66 63 1/8 0)
     (11/2 67 64 1/8 0) (45/8 69 65 1/8 0)
     (23/4 71 66 1/8 0) (47/8 73 67 1/8 0))
\end{verbatim}

\noindent This function applies the function
resolve-tie recursively until all ties have been
resolved. At this point the input dataset is projected
to remove the tie dimension. |#

(defun resolve-ties 
       (dataset &optional
	(unchunking-dataset
	 (resolve-tie dataset)))
  (if (stringp unchunking-dataset)
    (orthogonal-projection-unique-equalp
     dataset '(1 1 1 1 1 0))
    (resolve-ties
     unchunking-dataset)))
