#| Copyright 2008-2011 Tom Collins
   Monday 19 October 2009

These functions aid the calculation musical
attributes, such as the number of intervallic leaps in
a melody. Some of the attributes are implementations
of definitions from
\citet*{pearce2007,vonHippel2000,eerola2000}. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/geometric-operations.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/list-processing.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/sort-by.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/stats-sampling.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/vector-operations.lisp"))

#|
\noindent Example:
\begin{verbatim}
(cons-ith-while-floor-jth-constantp
 '((13 55) (13 60) (13 64) (27/2 63) (14 55) (15 55)
   (15 59) (15 65) (16 55) (17 72) (18 55) (19 55)
   (22 55) (23 60) (24 55) (24 59) (25 55)) 1 0)
--> (55 60 64 63)
\end{verbatim}

\noindent This function makes a list from the $i$th
item of each list in a list of lists, so long as the
floor of the $j$th item is constant. |#

(defun cons-ith-while-floor-jth-constantp
       (a-list i j &optional
	(probe
	 (floor (nth j (first a-list))))
	(result nil))
  (if (or
       (null a-list)
       (not (equalp (floor (nth j (first a-list)))
		    probe)))
    (identity result)
    (cons-ith-while-floor-jth-constantp
     (rest a-list) i j probe
     (append result (list (nth i (first a-list)))))))

#|
\noindent Example:
\begin{verbatim}
(cons-ith-while-jth-constantp
 '((13 55) (13 60) (13 64) (14 55) (15 55) (15 59)
   (15 65) (16 55) (17 72) (18 55) (19 55) (22 55)
   (23 60) (24 55) (24 59) (25 55) (25 67)) 1 0)
--> (55 60 64)
\end{verbatim}

\noindent This function makes a list from the $i$th
item of each list in a list of lists, so long as the
$j$th item is constant. |#

(defun cons-ith-while-jth-constantp
       (a-list i j &optional
	(probe
	 (nth j (first a-list)))
	(result nil))
  (if (or
       (null a-list)
       (not (equalp (nth j (first a-list)) probe)))
    (identity result)
    (cons-ith-while-jth-constantp
     (rest a-list) i j probe
     (append result (list (nth i (first a-list)))))))

#|
\noindent Example:
\begin{verbatim}
(density
 '((13 55) (13 60) (13 64) (27/2 63) (14 55)) 13)
--> 4
\end{verbatim}

\noindent In a pattern
$P = \{ \mathbf{p}_1, \mathbf{p}_2,\ldots,
\mathbf{p}_l \}$, let $\mathbf{p}_i$ have ontime
$x_i,\ i = 1, 2,\ldots, l$. The tactus beats are then
the integers from $a = \lfloor x_1\rfloor$ to $b =
\lfloor x_l\rfloor$, assuming that beats coincide with
integer ontimes and that the bottom number in the time
signature does not change over the course of the
pattern. The rhythmic density of the pattern at beat
$c \in [a, b]$, denoted $\rho(P, c)$, is given by the
cardinality of the set of all pattern points such that
$\lfloor x_i\rfloor = c$. |#

(defun density (sub-pattern integer)
  (length
   (cons-ith-while-floor-jth-constantp
    sub-pattern 1 0 integer)))

#|
\noindent Example:
\begin{verbatim}
(intervallic-leaps
 '((13 57) (13 60) (13 62) (14 57) (15 57) (15 59)
   (15 63) (16 57) (17 67) (18 57) (19 57) (22 57)
   (23 60) (24 57) (24 59) (25 57) (25 64)))
--> 7
\end{verbatim}

\noindent This variable counts the number of
intervallic leaps present in the melody line of a
pattern, the intuition being that leaping melodies may
be rated as more noticeable or important. Any interval
larger than a major third counts, and the same `top-
line' rule as in the function small-intervals is
observed. |#

(defun intervallic-leaps
       (pattern &optional (leap 2) (index 1)
	(intervals
	 (spacing-items
	  (top-line pattern index))) (counter 0))
  (if (null intervals) (identity counter)
    (intervallic-leaps
     pattern leap index (rest intervals)
     (if (> (abs (first intervals)) leap)
       (+ counter 1) (identity counter)))))

#|
\noindent Example:
\begin{verbatim}
(max-pitch-centre
 '(((0 60) (1 61)) ((3 48) (4 49))) 1
 '((0 60) (1 61) (2 62) (3 48) (3 57) (4 49)))
--> 23/3
\end{verbatim}

\noindent Pitch centre is defined as `the absolute
distance, in semitones, of the mean pitch of a
[pattern]$\ldots$from the mean pitch of the dataset'
\citep[p.~78]{pearce2007}. By taking the maximum pitch
centre over all occurrences of a pattern, I hope to
isolate either unusually high, or unusually low
occurrences. |#

(defun max-pitch-centre
       (TEC-with-MIDI-note-numbers index dataset
	&optional
	(dataset-MIDI-note-numbers
	 (nth-list-of-lists 1 dataset))
	(dataset-MIDI-note-numbers-mean
	 (mean dataset-MIDI-note-numbers))
	(result nil)
	(pattern-MIDI-note-numbers
	 (if TEC-with-MIDI-note-numbers
	  (nth-list-of-lists
	   index
	   (first TEC-with-MIDI-note-numbers))))
	(pattern-MIDI-note-numbers-mean
	 (if TEC-with-MIDI-note-numbers
	   (mean pattern-MIDI-note-numbers))))
  (if (null TEC-with-MIDI-note-numbers)
    (max-item result)
    (max-pitch-centre
     (rest TEC-with-MIDI-note-numbers)
     index dataset dataset-MIDI-note-numbers
     dataset-MIDI-note-numbers-mean
     (append
      result
      (list 
       (abs
        (-
         dataset-MIDI-note-numbers-mean
         pattern-MIDI-note-numbers-mean)))))))

#|
\noindent Example:
\begin{verbatim}
(pitch-centre
 '(60 61 62) '((0 60) (1 61) (2 62) (3 48) (3 57)))
--> 17/5
\end{verbatim}

\noindent Pitch centre is defined as `the absolute
distance, in semitones, of the mean pitch of a
[pattern]$\ldots$from the mean pitch of the dataset'
\citep[p.~78]{pearce2007}. By taking the maximum pitch
centre over all occurrences of a pattern, I hope to
isolate either unusually high, or unusually low
occurrences. |#

(defun pitch-centre
       (pattern-MIDI-note-numbers dataset &optional
	(dataset-MIDI-note-numbers
	 (nth-list-of-lists 1 dataset))
	(dataset-MIDI-note-numbers-mean
	 (mean dataset-MIDI-note-numbers))
	(pattern-MIDI-note-numbers-mean
	 (mean pattern-MIDI-note-numbers)))
  (abs
   (- dataset-MIDI-note-numbers-mean
      pattern-MIDI-note-numbers-mean)))

#|
\noindent Example:
\begin{verbatim}
(pitch-range '((0 60) (1 61) (3 62)) 1)
--> 2

\end{verbatim}

\noindent Pitch range is the range in semitones of a
pattern. |#

(defun pitch-range
       (pattern index &optional
	(pattern-MIDI-note-numbers
	 (nth-list-of-lists index pattern)))
  (range pattern-MIDI-note-numbers))

#|
\noindent Example:
\begin{verbatim}
(restn '((13 55) (13 60) (13 64) (14 55) (15 55)) 3)
--> ((14 55) (15 55))
\end{verbatim}

\noindent Applies the function rest $n$ times. |#

(defun restn (a-list n)
  (if (<= n 0) (identity a-list)
    (restn (rest a-list) (- n 1))))

#|
\noindent Example:
\begin{verbatim}
(rhythmic-density
 '((13 55) (13 60) (13 64) (27/2 63) (14 55) (17 48)))
--> 6/5
\end{verbatim}

\noindent The rhythmic density of a pattern is defined
as `the mean number of events per tactus beat'
\citep[p.~78]{pearce2007}. See the function density
for further definitions. |#

(defun rhythmic-density
       (pattern &optional
	(a (floor (first (first pattern))))
	(b (floor (first (my-last pattern))))
	(i a)
	(result 0)
	(density-summand
	 (if pattern
	   (density pattern i))))
  (if (null pattern)
    (/ result (+ (- b a) 1))
    (rhythmic-density
     (restn pattern density-summand)
     a b (+ i 1) (+ result density-summand))))

#|
\noindent Example:
\begin{verbatim}
(rhythmic-variability
 '((0 64 1) (1 55 1/2) (1 65 1) (2 55 1/2) (2 72 1/3)
   (3 55 1) (4 55 2) (5 55 1/2) (5 60 1)
   (6 59 1/3) (6 67 1/2)) 2)
--> 0.5354223
\end{verbatim}

\noindent The rhythmic variability of a pattern is
defined as `the degree of change in note duration
(i.e., the standard deviation of the log of the event
durations)' \citep[p.~78]{pearce2007}. The intuition
is that patterns with much rhythmic variation are
likely to be noticeable. |#

(defun rhythmic-variability
       (pattern index &optional (base (exp 1))
	(log-rhythms
	 (mapcar
          #'(lambda (z) (log z base))
          (nth-list-of-lists index pattern))))
  (sd log-rhythms))

#|
\noindent Example:
\begin{verbatim}
(small-intervals
 '((13 57) (13 60) (13 62) (14 57) (15 57) (15 59)
   (15 63) (16 57) (17 67) (18 57) (19 57) (22 57)
   (23 60) (24 57) (24 59) (25 57) (25 64)))
--> 3
\end{verbatim}

\noindent The small intervals variable counts the
number of such intervals present in the melody line of
a pattern, the intuition being that scalic, static or
stepwise melodies may be rated as more noticeable or
important. As sometimes the melody is not obvious in
polyphonic music, I use a `top-line' rule: at each of
the pattern's distinct ontimes there will be at least
one datapoint present. At this ontime the melody takes
the value of the maximum morphetic pitch number
present. |#

(defun small-intervals
       (pattern &optional (index 1)
	(intervals
	 (spacing-items
	  (top-line pattern index))) (counter 0))
  (if (null intervals) (identity counter)
    (small-intervals
     pattern index (rest intervals)
     (if (find
	  (abs (first intervals)) '(0 1)
	  :test #'equalp)
       (+ counter 1) (identity counter)))))

#|
\noindent Example:
\begin{verbatim}
(top-line
 '((13 55) (13 60) (13 64) (14 55) (15 55) (15 59)
   (15 65) (16 55) (17 72) (18 55) (19 55) (22 55)
   (23 60) (24 55) (24 59) (25 55) (25 67)) 1)
--> (64 55 65 55 72 55 55 55 60 59 67)
\end{verbatim}

\noindent For each distinct ontime, this function
returns the maximum pitch as a member of a list. |#

(defun top-line
       (pattern index &optional (result nil)
	(ontime (first (first pattern)))
	(relevant-melody-notes
	 (if ontime
	   (cons-ith-while-jth-constantp
	    pattern index 0 ontime))))
  (if (null pattern) (identity result)
    (top-line
     (restn pattern (length relevant-melody-notes))
     index
     (append
      result
      (list (max-item relevant-melody-notes))))))
