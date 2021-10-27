#| Copyright 2008-2011 Tom Collins
   Friday 16 January 2009

The fundamental functions here are used to segment
datapoints based on ontime and offtime. Subsequent
functions do things like computing chord spacing and
holding types. |#

; REQUIRED PACKAGES:
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
  "/Maths foundation/sort-by.lisp"))

#|
\noindent Example:
\begin{verbatim}
(append-offtimes '((0 48 2) (1 60 1) (1 57 1/2)))
--> '((0 48 2 2) (1 60 1 2) (1 57 1/2 3/2))
\end{verbatim}

\noindent This function takes a list, assumed to be
datapoints, and appends the offset of each datapoint
as the final item. |#

(defun append-offtimes
       (a-list &optional (duration-index 2))
  (if (null a-list) ()
    (cons
     (append
      (first a-list)
      (list (+ (first (first a-list))
	       (nth duration-index (first a-list)))))
     (append-offtimes (rest a-list) duration-index))))

#|
\noindent Example:
\begin{verbatim}
(chord-candidates-offtimes
 '((1579 66 191 2 49 1770 5) (1974 64 191 3 49 2165 2)
   (1974 67 191 2 49 2165 3) (2368 66 191 2 49 2559 0)
   (2368 62 191 3 49 2559 4) (2763 64 191 2 49 2954 6)
   (2763 57 191 3 49 2954 7) (2800 72 191 1 49 2991 8)
   (3158 38 191 4 49 3349 9)
   (1579 62 1920 3 49 3499 1)
   (3158 54 385 3 49 3543 10)
   (3158 62 385 2 49 3543 11)
   (3553 42 191 4 49 3744 12)
   (3947 45 191 4 49 4138 13)
   (4342 50 191 4 49 4533 14)) 15 2368 0)
--> (5 2 3)
\end{verbatim}

\noindent There are four arguments to this function: a
list of datapoints (ordered by offtimes ascending and
appended with an enumeration), the length $l$ of the
list, a point in time $x$, and an index $s$ from which
to begin searching. When the $n$th offtime equals or
exceeds $x$, the search stops. As subsequent calls to
this function use larger values of $x$, the search can
begin at the $s$th offtime. |#

(defun chord-candidates-offtimes (a-list l x s)
  (let ((datapoint (nth s a-list)))
    (if (or (null x)
            (equal s l)
            (>= (sixth datapoint) x)) ()
      (cons
       (my-last datapoint)
       (chord-candidates-offtimes
	a-list l x (+ s 1))))))

#|
\noindent Example:
\begin{verbatim}
(chord-candidates-offtimes-strict
 '((3 55 57 3/4 1 15/4 1) (3 60 60 3/4 1 15/4 2)
   (3 67 64 3/4 0 15/4 3) (3 76 69 3/4 0 15/4 4)
   (15/4 59 59 1/4 1 4 5) (15/4 65 63 1/4 0 4 6)
   (15/4 74 68 1/4 0 4 7) (3 48 53 3 1 6 0)
   (4 60 60 2 1 6 8) (4 64 62 2 0 6 9)
   (4 72 67 2 0 6 10)) 11 15/4 0)
--> (1 2 3 4)
\end{verbatim}

\noindent Contrast the output of this function with
the function chord-candidate-offtimes. The difference
is that the present function will not return indices
of datapoints whose offtimes coincide with the
provided time $x$. There are four arguments to this
function: a list of datapoints (ordered by offtimes
ascending and appended with an enumeration), the
length $l$ of the list, a point in time $x$, and an
index $s$ from which to begin searching. When the
$n$th offtime equals or exceeds $x$, the search stops.
As subsequent calls to this function use larger values
of $x$, the search can begin at the $s$th offtime. |#

(defun chord-candidates-offtimes-strict (a-list l x s)
  (let ((datapoint (nth s a-list)))
    (if (or (null x)
            (equal s l)
            (> (sixth datapoint) x)) ()
      (cons
       (my-last datapoint)
       (chord-candidates-offtimes-strict
	a-list l x (+ s 1))))))

#|
\noindent Example:
\begin{verbatim}
(chord-candidates-ontimes
 '((1579 62 1920 3 49 3499 1)
   (1579 66 191 2 49 1770 5) (1974 64 191 3 49 2165 2)
   (1974 67 191 2 49 2165 3) (2368 66 191 2 49 2559 0)
   (2368 62 191 3 49 2559 4) (2763 64 191 2 49 2954 6)
   (2763 57 191 3 49 2820 7) (2800 72 191 1 49 2991 8)
   (3158 38 191 4 49 3349 9)
   (3158 54 385 3 49 3543 10)
   (3158 62 385 2 49 3543 11)
   (3553 42 191 4 49 3744 12)
   (3947 45 191 4 49 4138 13)
   (4342 50 191 4 49 4533 14)) 15 2368 0)
--> (1 5 2 3 0 4)
\end{verbatim}

\noindent There are four arguments to this function: a
list of datapoints (ordered by ontimes and appended
with offtimes and an enumeration), the length $l$ of
the list, a point in time $x$, and an index $s$ from
which to begin searching. When the $n$th ontime
exceeds $x$, the search stops. As subsequent calls to
this function use larger values of $x$, the search can
begin at the $s$th ontime. |#

(defun chord-candidates-ontimes (a-list l x s)
  (let ((datapoint (nth s a-list)))
    (if (or (null x)
            (equal s l)
	    (> (first datapoint) x)) ()
      (cons
       (my-last datapoint)
       (chord-candidates-ontimes
	a-list l x (+ s 1))))))

#|
\noindent Example:
\begin{verbatim}
(enumerate-append '((3 53) (6 0) (42 42)))
--> ((3 53 0) (6 0 1) (42 42 2))
\end{verbatim}

\noindent This function enumerates a list by appending
the next natural number, counting from 0, to the end
of each list. |#
  
(defun enumerate-append
       (a-list &optional (i 0) (n (length a-list)))
  (if (>= i n) ()
    (cons (append (first a-list) (list i))
          (enumerate-append
	   (rest a-list) (+ i 1) n))))

#|
\noindent Example:
\begin{verbatim}
(prepare-for-segments
 '((2368 66 191 2 49 2559 0)
   (1579 62 1920 3 49 3499 1)
   (1974 64 191 3 49 2165 2) (1974 67 191 2 49 2165 3)
   (2368 62 191 3 49 2559 4) (1579 66 191 2 49 1770 5)
   (2763 64 191 2 49 2954 6) (2763 57 191 3 49 2954 7)
   (2800 72 191 1 49 2991 8) (3158 38 191 4 49 3349 9)
   (3158 54 385 3 49 3543 10)
   (3158 62 385 2 49 3543 11)
   (3553 42 191 4 49 3744 12)
   (3947 45 191 4 49 4138 13)
   (4342 50 191 4 49 4533 14)))
--> (((1579 62 1920 3 49 3499 1)
      (1579 66 191 2 49 1770 5)
      (1974 64 191 3 49 2165 2)
      (1974 67 191 2 49 2165 3)
      (2368 66 191 2 49 2559 0)
      (2368 62 191 3 49 2559 4)
      (2763 64 191 2 49 2954 6)
      (2763 57 191 3 49 2820 7)
      (2800 72 191 1 49 2991 8)
      (3158 38 191 4 49 3349 9)
      (3158 54 385 3 49 3543 10)
      (3158 62 385 2 49 3543 11)
      (3553 42 191 4 49 3744 12)
      (3947 45 191 4 49 4138 13)
      (4342 50 191 4 49 4533 14))
     ((1579 66 191 2 49 1770 5)
      (1974 64 191 3 49 2165 2)
      (1974 67 191 2 49 2165 3)
      (2368 66 191 2 49 2559 0)
      (2368 62 191 3 49 2559 4)
      (2763 64 191 2 49 2954 6)
      (2763 57 191 3 49 2954 7)
      (2800 72 191 1 49 2991 8)
      (3158 38 191 4 49 3349 9)
      (1579 62 1920 3 49 3499 1)
      (3158 54 385 3 49 3543 10)
      (3158 62 385 2 49 3543 11)
      (3553 42 191 4 49 3744 12)
      (3947 45 191 4 49 4138 13)
      (4342 50 191 4 49 4533 14)))
\end{verbatim}

\noindent The datapoints already have offtimes
appended and are enumerated. They are sent to two
lists; one ordered by ontime, the other by offtime. |#

(defun prepare-for-segments
       (a-list &optional
	(ontime-index 0) (offtime-index 5))
  (append
   (list (sort-by
	  (list (list ontime-index "asc")) a-list))
   (list (sort-by
	  (list (list offtime-index "asc")) a-list))))

#|
\noindent Example:
\begin{verbatim}
(segment
 1579
 '((2368 66 191 2 49) (1579 62 1920 3 49)
   (1974 64 191 3 49) (1974 67 191 2 49)
   (2368 62 191 3 49) (1579 66 191 2 49)
   (2763 64 191 2 49) (2763 57 191 3 49)
   (2800 72 191 1 49) (3158 38 191 4 49)
   (3158 54 385 3 49) (3158 62 385 2 49)
   (3553 42 191 4 49) (3947 45 191 4 49)
   (4342 50 191 4 49))
 15
 '(((1579 62 1920 3 49 3499 1)
    (1579 66 191 2 49 1770 5)
    (1974 64 191 3 49 2165 2)
    (1974 67 191 2 49 2165 3)
    (2368 66 191 2 49 2559 0)
    (2368 62 191 3 49 2559 4)
    (2763 64 191 2 49 2954 6)
    (2763 57 191 3 49 2820 7)
    (2800 72 191 1 49 2991 8)
    (3158 38 191 4 49 3349 9)
    (3158 54 385 3 49 3543 10)
    (3158 62 385 2 49 3543 11)
    (3553 42 191 4 49 3744 12)
    (3947 45 191 4 49 4138 13)
    (4342 50 191 4 49 4533 14))
   ((1579 66 191 2 49 1770 5)
    (1974 64 191 3 49 2165 2)
    (1974 67 191 2 49 2165 3)
    (2368 66 191 2 49 2559 0)
    (2368 62 191 3 49 2559 4)
    (2763 64 191 2 49 2954 6)
    (2763 57 191 3 49 2954 7)
    (2800 72 191 1 49 2991 8)
    (3158 38 191 4 49 3349 9)
    (1579 62 1920 3 49 3499 1)
    (3158 54 385 3 49 3543 10)
    (3158 62 385 2 49 3543 11)
    (3553 42 191 4 49 3744 12)
    (3947 45 191 4 49 4138 13)
    (4342 50 191 4 49 4533 14))))
--> (((1579 66 191 2 49) (1579 62 1920 3 49))
     (1 5) (14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))
\end{verbatim}

\noindent This function takes an ontime $t$, a list of
datapoints of length $N$, with offsets and enumeration
appended, but in original order, as well as the
datapoints having had the function prepared-for-
segments applied. It returns any datapoints which
exist at the point $t$, as well as lists which help
speed up any subsequent searches. |#

(defun segment
       (ontime datapoints-with-offs&enums N
	prepared-for-segments &optional
	(previous-ontimes ())
	(previous-offtimes
	 (add-to-list
	  -1 (reverse (first-n-naturals N)))))
  (let* ((candidate-ontimes
	  (chord-candidates-ontimes
	   (first prepared-for-segments)
	   N ontime (length previous-ontimes)))
	 (candidate-offtimes
	  (chord-candidates-offtimes
	   (second prepared-for-segments)
	   N ontime (- N (length previous-offtimes))))
	 (new-ontimes 
	  (append previous-ontimes candidate-ontimes))
	 (new-offtimes
	  (set-difference
	   previous-offtimes candidate-offtimes)))
    (append
     (list
      (nth-list
       (intersection new-ontimes new-offtimes)
       datapoints-with-offs&enums))
     (list new-ontimes)
     (list new-offtimes))))

#|
\noindent Example:
\begin{verbatim}
(segment-strict
 15/4
 '((3 48 53 3 1 6 0) (3 55 57 3/4 1 15/4 1)
   (3 60 60 3/4 1 15/4 2) (3 67 64 3/4 0 15/4 3)
   (3 76 69 3/4 0 15/4 4) (15/4 59 59 1/4 1 4 5)
   (15/4 65 63 1/4 0 4 6) (15/4 74 68 1/4 0 4 7)
   (4 60 60 2 1 6 8) (4 64 62 2 0 6 9)
   (4 72 67 2 0 6 10))
 11
 '(((3 48 53 3 1 6 0) (3 55 57 3/4 1 15/4 1)
    (3 60 60 3/4 1 15/4 2) (3 67 64 3/4 0 15/4 3)
    (3 76 69 3/4 0 15/4 4) (15/4 59 59 1/4 1 4 5)
    (15/4 65 63 1/4 0 4 6) (15/4 74 68 1/4 0 4 7)
    (4 60 60 2 1 6 8) (4 64 62 2 0 6 9)
    (4 72 67 2 0 6 10))
   ((3 55 57 3/4 1 15/4 1) (3 60 60 3/4 1 15/4 2)
    (3 67 64 3/4 0 15/4 3) (3 76 69 3/4 0 15/4 4)
    (15/4 59 59 1/4 1 4 5) (15/4 65 63 1/4 0 4 6)
    (15/4 74 68 1/4 0 4 7) (3 48 53 3 1 6 0)
    (4 60 60 2 1 6 8) (4 64 62 2 0 6 9)
    (4 72 67 2 0 6 10))))
--> (((15/4 74 68 1/4 0 4 7) (15/4 65 63 1/4 0 4 6)
      (15/4 59 59 1/4 1 4 5) (3 48 53 3 1 6 0))
     (0 1 2 3 4 5 6 7) (10 9 8 7 6 5 0))
\end{verbatim}

\noindent This function uses the function
chord-candidate-offtimes-strict instead of
chord-candidate-offtimes, and performs a sort on the
output, according to the optional argument sort-index.
The function takes an ontime $t$, a list of datapoints
of length $N$, with offsets and enumeration appended,
but in original order, as well as the datapoints
having had the function prepared-for-segments applied.
It returns any datapoints which exist at the point
$t$, as well as lists which help speed up any
subsequent searches. |#

(defun segment-strict
       (ontime datapoints-with-offs&enums N
	prepared-for-segments &optional (sort-index 1)
	(previous-ontimes ())
	(previous-offtimes
	 (add-to-list
	  -1 (reverse (first-n-naturals N)))))
  (let* ((candidate-ontimes
	  (chord-candidates-ontimes
	   (first prepared-for-segments)
	   N ontime (length previous-ontimes)))
	 (candidate-offtimes
	  (chord-candidates-offtimes-strict
	   (second prepared-for-segments)
	   N ontime (- N (length previous-offtimes))))
	 (new-ontimes 
	  (append previous-ontimes candidate-ontimes))
	 (new-offtimes
	  (set-difference
	   previous-offtimes candidate-offtimes)))
    (append
     (list
      (sort-by
       (list (list sort-index "asc"))
       (nth-list
	(intersection new-ontimes new-offtimes)
	datapoints-with-offs&enums)))
     (list new-ontimes)
     (list new-offtimes))))

#|
\noindent Example:
\begin{verbatim}
(segments
 '((2368 66 191 2 49) (1579 62 1920 3 49)
   (1974 64 191 3 49) (1974 67 191 2 49)
   (2368 62 191 3 49) (1579 66 191 2 49)
   (2763 64 191 2 49) (2763 57 191 3 49)
   (2800 72 191 1 49) (3158 38 191 4 49)
   (3158 54 385 3 49) (3158 62 385 2 49)
   (3553 42 191 4 49) (3947 45 191 4 49)
   (4342 50 191 4 49)))
--> ((1579 ((1579 66 191 2 49 1770 5)
	    (1579 62 1920 3 49 3499 1)))
     (1770 ((1579 66 191 2 49 1770 5)
	    (1579 62 1920 3 49 3499 1)))
     (1974 ((1974 67 191 2 49 2165 3)
	    (1974 64 191 3 49 2165 2)
	    (1579 62 1920 3 49 3499 1)))
     (2165 ((1974 67 191 2 49 2165 3)
	    (1974 64 191 3 49 2165 2)
	    (1579 62 1920 3 49 3499 1)))
     (2368 ((2368 62 191 3 49 2559 4)
	    (2368 66 191 2 49 2559 0)
	    (1579 62 1920 3 49 3499 1)))
     (2559 ((2368 62 191 3 49 2559 4)
	    (2368 66 191 2 49 2559 0)
	    (1579 62 1920 3 49 3499 1)))
     (2763 ((2763 57 191 3 49 2954 7)
	    (2763 64 191 2 49 2954 6)
	    (1579 62 1920 3 49 3499 1)))
     (2800 ((2800 72 191 1 49 2991 8)
	    (2763 57 191 3 49 2954 7)
	    (2763 64 191 2 49 2954 6)
	    (1579 62 1920 3 49 3499 1)))
     (2954 ((2800 72 191 1 49 2991 8)
	    (2763 57 191 3 49 2954 7)
	    (2763 64 191 2 49 2954 6)
	    (1579 62 1920 3 49 3499 1)))
     (2991 ((2800 72 191 1 49 2991 8)
	    (1579 62 1920 3 49 3499 1)))
     (3158 ((3158 62 385 2 49 3543 11)
	    (3158 54 385 3 49 3543 10)
	    (3158 38 191 4 49 3349 9)
	    (1579 62 1920 3 49 3499 1)))
     (3349 ((3158 62 385 2 49 3543 11)
	    (3158 54 385 3 49 3543 10)
	    (3158 38 191 4 49 3349 9)
	    (1579 62 1920 3 49 3499 1)))
     (3499 ((3158 62 385 2 49 3543 11)
	    (3158 54 385 3 49 3543 10)
	    (1579 62 1920 3 49 3499 1)))
     (3543 ((3158 62 385 2 49 3543 11)
	    (3158 54 385 3 49 3543 10)))
     (3553 ((3553 42 191 4 49 3744 12)))
     (3744 ((3553 42 191 4 49 3744 12)))
     (3947 ((3947 45 191 4 49 4138 13)))
     (4138 ((3947 45 191 4 49 4138 13)))
     (4342 ((4342 50 191 4 49 4533 14)))
     (4533 ((4342 50 191 4 49 4533 14))))
\end{verbatim}

\noindent This function takes a list of datapoints as
its argument. First it creates a variable containing
the distinct times (on and off) of the datapoints.
Then it returns the segment for each of these
times. |#

(defun segments
       (datapoints &optional (duration-index 2)
        (datapoints-with-offs&enums
	 (enumerate-append
	  (append-offtimes
	   datapoints duration-index)))
	(unique-times
	 (remove-duplicates
	  (sort
	   (append
	    (nth-list-of-lists
	     0 datapoints-with-offs&enums)
	    (nth-list-of-lists
	     5 datapoints-with-offs&enums)) #'<)
	  :test #'equalp))
	#| Old version
	(unique-times
	 (unique-equalp
	  (append
	   (nth-list-of-lists
	    0 datapoints-with-offs&enums)
	   (nth-list-of-lists
	    5 datapoints-with-offs&enums))))
	|#
	(N (length datapoints))		 
	(prepared-for-segments
	 (prepare-for-segments
	  datapoints-with-offs&enums))
	(previous-ontimes ())
	(previous-offtimes
	 (add-to-list
	  -1 (reverse (first-n-naturals N)))))
  (let ((current-segment
	 (segment
	  (first unique-times)
	  datapoints-with-offs&enums N
	  prepared-for-segments previous-ontimes
	  previous-offtimes)))
    (if (null unique-times) ()
      (cons (list (first unique-times)
                  (first current-segment))
	    (segments
	     datapoints duration-index
	     datapoints-with-offs&enums
             (rest unique-times) N
	     prepared-for-segments
	     (second current-segment)
	     (third current-segment))))))

#|
\noindent Example:
\begin{verbatim}
(segments-strict
 '((3 48 53 3 1) (3 67 64 3/4 0) (3 76 69 3/4 0)
   (15/4 65 63 1/4 0) (15/4 74 68 1/4 0) (4 64 62 2 0)
   (4 72 67 2 0) (13/2 61 60 1/2 0) (7 62 61 1/2 0)
   (15/2 64 62 1/2 0) (8 50 54 1 1) (8 65 63 1 0))
 1 3)
--> ((3 ((3 48 53 3 1 6 0) (3 67 64 3/4 0 15/4 1)
	 (3 76 69 3/4 0 15/4 2)))
     (15/4 ((3 48 53 3 1 6 0) (15/4 65 63 1/4 0 4 3)
	    (15/4 74 68 1/4 0 4 4)))
     (4 ((3 48 53 3 1 6 0) (4 64 62 2 0 6 5)
	 (4 72 67 2 0 6 6)))
     (6 NIL)
     (13/2 ((13/2 61 60 1/2 0 7 7)))
     (7 ((7 62 61 1/2 0 15/2 8)))
     (15/2 ((15/2 64 62 1/2 0 8 9)))
     (8 ((8 50 54 1 1 9 10) (8 65 63 1 0 9 11)))
     (9 NIL))
\end{verbatim}

\noindent This function uses the function
segment-strict instead of segement. The function takes
a list of datapoints as its argument. First it creates
a variable containing the distinct times (on and off)
of the datapoints. Then it returns the segment for
each of these times. |#

(defun segments-strict
       (datapoints &optional (sort-index 1)
	(duration-index 2)
        (datapoints-with-offs&enums
	 (enumerate-append
	  (append-offtimes
	   datapoints duration-index)))
	(unique-times
	 (remove-duplicates
	  (sort
	   (append
	    (nth-list-of-lists
	     0 datapoints-with-offs&enums)
	    (nth-list-of-lists
	     5 datapoints-with-offs&enums)) #'<)
	  :test #'equalp))
	(N (length datapoints))		 
	(prepared-for-segments
	 (prepare-for-segments
	  datapoints-with-offs&enums))
	(previous-ontimes ())
	(previous-offtimes
	 (add-to-list
	  -1 (reverse (first-n-naturals N)))))
  (let ((current-segment
	 (segment-strict
	  (first unique-times)
	  datapoints-with-offs&enums N
	  prepared-for-segments sort-index
	  previous-ontimes previous-offtimes)))
    (if (null unique-times) ()
      (cons (list (first unique-times)
                  (first current-segment))
	    (segments-strict
	     datapoints sort-index duration-index
	     datapoints-with-offs&enums
             (rest unique-times) N
	     prepared-for-segments
	     (second current-segment)
	     (third current-segment))))))
