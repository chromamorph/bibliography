#| Copyright 2008-2011 Tom Collins
   Wednesday 20 October 2010

These functions filter the results of applying a
pattern discovery algorithm. The result is a list
consisting of hash tables, where each hash table
consists of the keys: index, name, cardinality,
occurrences, MTP vectors, rating, compactness,
expected occurrences, compression ratio, pattern,
region, and translators. The most important function
in this file is called
prepare-for-pattern-inheritance. |#

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Pattern discovery/evaluation-for-SIACT.lisp"))
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
  "/Maths foundation/set-operations.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Pattern discovery"
  "/structural-induction-merge.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/vector-operations.lisp"))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(indices-of-patterns-equalp-trans&intersect
 (gethash '"pattern" (first patterns-hash))
 (gethash '"translators" (first patterns-hash))
 (rest patterns-hash))
--> nil
\end{verbatim}

\noindent This function takes information about a
pattern from the first hash table in a list of hash
tables. It then compares this with each pattern in the
rest of the list. If a pair of patterns have the same
translation vectors and their first occurrences have
intersecting datapoints, then the second pattern in
the pair will have to be removed. To this end, the
index of the second pattern is returned. |#

(defun indices-of-patterns-equalp-trans&intersect
       (head-pattern head-translators
        rest-patterns-hash &optional
        (n (length rest-patterns-hash)) (i 0)
        (pattern-hash
         (if (< i n) (nth i rest-patterns-hash)))
        (remove-patternp
         (if pattern-hash
           (and
            (equalp
             (gethash '"translators" pattern-hash)
             head-translators)
            (intersection-multidimensional
             (gethash '"pattern" pattern-hash)
             head-pattern)))))
  (if (>= i n) ()
    (append
     (if remove-patternp (list i))
     (indices-of-patterns-equalp-trans&intersect
      head-pattern head-translators
      rest-patterns-hash n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   SIACT-output
   '((((0 36 46) (0 48 53) (0 55 57) (0 60 60)
       (0 64 62) (1/2 36 46) (1/2 48 53) (1/2 55 57)
       (1/2 62 61) (1/2 65 63) (1 36 46) (1 48 53)
       (1 55 57) (1 64 62) (1 67 64) (2 48 53)
       (2 65 63) (2 69 65) (3 36 46) (3 48 53)
       (3 55 57) (3 64 62) (3 67 64) (7/2 36 46)
       (7/2 48 53) (7/2 55 57) (7/2 60 60) (7/2 64 62)
       (4 36 46) (4 48 53) (4 55 57)) 31/33 (6 0 0))
     (((1 36 46) (1 48 53) (1 55 57) (1 64 62)
       (1 67 64) (2 48 53) (2 65 63) (2 69 65)
       (3 36 46) (3 48 53)) 5/6 (6 0 0))
     (((6 60 60) (7 55 57) (9 55 57) (9 60 60)
       (10 48 53) (10 55 57) (10 64 62) (13 55 57)
       (13 59 59) (14 55 57) (14 59 59) (19 55 57)
       (19 59 59) (20 55 57) (20 59 59) (25 55 57)
       (25 62 61) (26 55 57) (26 62 61) (27 72 67)
       (28 48 53) (28 55 57) (28 64 62) (31 55 57)
       (31 62 61) (32 55 57) (32 62 61) (33 72 67)
       (34 48 53) (34 55 57) (34 64 62) (37 55 57)
       (37 59 59)) 11/50 (6 0 0))
     (((3 48 53) (7/2 48 53) (4 48 53))
      3/11 (45 26 15) 3/11 (3 7 4))
     (((11 71 66) (23/2 72 67) (12 54 56) (12 57 58)
       (12 60 60) (12 62 61) (12 74 68) (25/2 74 68)
       (13 53 56) (13 55 57) (13 59 59) (13 62 61)
       (13 74 68) (14 53 56) (14 55 57) (14 59 59)
       (14 62 61) (14 67 64) (29/2 71 66) (15 52 55)
       (15 55 57) (15 60 60) (15 72 67) (31/2 74 68)
       (16 48 53) (16 55 57) (16 64 62) (16 76 69)
       (33/2 77 70) (17 79 71) (35/2 80 71) (18 43 50)
       (18 81 72) (37/2 81 72) (19 55 57) (19 59 59)
       (19 65 63) (20 55 57) (20 59 59) (20 65 63)
       (20 79 71) (41/2 74 68) (21 48 53) (21 77 70)
       (43/2 77 70) (22 57 58) (22 65 63) (22 77 70)
       (23 55 57) (23 64 62) (23 76 69) (24 54 56)
       (24 57 58) (24 60 60) (24 62 61) (24 74 68)
       (49/2 74 68) (25 53 56) (25 55 57) (25 59 59)
       (25 62 61) (25 74 68) (26 53 56) (26 55 57)
       (26 59 59) (26 62 61) (26 67 64) (53/2 71 66)
       (27 52 55) (27 55 57) (27 60 60) (27 72 67)
       (55/2 74 68) (28 48 53) (28 55 57) (28 64 62)
       (28 76 69) (57/2 77 70) (29 79 71) (59/2 84 74)
       (30 43 50) (30 83 73) (123/4 81 72) (31 55 57)
       (31 62 61) (31 65 63) (31 77 70) (63/2 74 68)
       (32 55 57) (32 62 61) (32 65 63) (32 71 66)
       (65/2 67 64) (33 48 53) (33 64 62) (33 72 67)
       (100/3 74 68) (67/2 67 64) (101/3 76 69)
       (34 48 53) (34 55 57) (34 60 60) (34 64 62)
       (34 72 67)) 104/105 (24 0 0))
     (((12 54 56) (12 57 58) (12 60 60) (12 62 61)
       (12 74 68) (25/2 74 68) (13 53 56) (13 55 57)
       (13 59 59) (13 62 61) (13 74 68) (14 53 56)
       (14 55 57) (14 59 59) (14 62 61) (14 67 64)
       (29/2 71 66) (15 52 55) (15 55 57) (15 60 60)
       (15 72 67) (31/2 74 68) (16 48 53) (16 55 57)
       (16 64 62) (16 76 69) (33/2 77 70) (17 79 71)
       (18 43 50) (19 55 57) (19 65 63) (20 55 57)
       (20 65 63) (21 48 53)) 17/21 (12 0 0))
     (((36 54 56) (36 57 58) (36 60 60) (36 62 61)
       (36 74 68) (73/2 74 68) (37 53 56) (37 55 57)
       (37 59 59) (37 62 61) (37 74 68) (38 53 56)
       (38 55 57) (38 59 59) (38 62 61) (38 67 64)
       (77/2 71 66) (39 52 55) (39 55 57) (39 60 60)
       (39 72 67) (79/2 74 68) (40 48 53) (40 55 57)
       (40 64 62) (40 76 69) (81/2 77 70) (41 79 71)
       (42 43 50) (43 55 57) (43 65 63) (44 55 57)
       (44 65 63) (45 48 53)) 34/41 (12 0 0))))
  (setq
   dataset-all
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-68-1-ed.txt")))
  (setq dataset-mini (subseq dataset-all 0 350))
  (setq
   projected-dataset
   (orthogonal-projection-unique-equalp
    dataset-mini '(1 1 1 0 0)))
  "Data imported.")
(setq
 patterns-hash
 (prepare-for-pattern-inheritance
  SIACT-output projected-dataset 1))
--> (#<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041ABBC9D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x3000419F57AD>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004199959D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x3000419CE8AD>)
\end{verbatim}

\noindent This function applies functions that
prepare the output of SIACT run on a dataset for
random generation Markov chain (RGMC) with pattern
inheritance. |#

(defun prepare-for-pattern-inheritance
       (pattern-compact-vec-triples projected-dataset
        &optional (duration-threshold 3)
        (coefficients
	 (list 4.277867 3.422478734 -0.038536808
	       0.651073171))
        (norm-coeffs
	 (list 73.5383283152 0.02114878519)))
  (subset-scores-of-patterns+
   (remove-patterns-equalp-trans&intersect
    (remove-overlapping-translators-of-patterns
     (translate-patterns-to-1st-occurrences
      (evaluate-variables-of-patterns2hash
       (remove-patterns-shorter-than
        pattern-compact-vec-triples
        duration-threshold)
       projected-dataset coefficients
       norm-coeffs))))))

#|
\noindent Example:
\begin{verbatim}
(remove-overlapping-translators
 3 '((0 0 0) (1 2 3) (4 0 0) (5 0 0) (7 0 0) (8 2 1)))
--> ((0 0 0) (4 0 0) (7 0 0))
\end{verbatim}

\noindent This function takes the duration of a
pattern and its translators as arguments, and returns
a list of those translators that do not produce
overlapping patterns (in the sense of the argument
pattern-duration). |#

(defun remove-overlapping-translators
       (pattern-duration translators &optional
        (result (list (first translators)))
        (vector1 (first translators))
        (vector2 (second translators))
        (overlapp
         (if vector2
           (<
            (- (first vector2) (first vector1))
            pattern-duration))))
  (if (null vector2) result
    (if overlapp
      (remove-overlapping-translators
       pattern-duration
       (cons vector1 (rest (rest translators)))
       result)
      (remove-overlapping-translators
       pattern-duration
       (cons vector2 (rest (rest translators)))
       (append result (list vector2))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(setq
 patterns-hash2
 (remove-overlapping-translators-of-patterns
  patterns-hash))
--> (#<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A38CFD>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A386ED>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A380DD>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A37ACD>)
\end{verbatim}

\noindent This function applies the function
remove-overlapping-translators recursively to a list
consisting of hash tables. Each hash table contains
information about a discovered pattern, as returned by
the function evaluate-variables-of-patterns2hash. The
output is an updated hash table. |#

(defun remove-overlapping-translators-of-patterns
       (patterns-hash &optional
        (pattern-hash (first patterns-hash))
        (pattern
         (if pattern-hash
           (gethash '"pattern" pattern-hash)))
        (new-translators
         (if pattern-hash
           (remove-overlapping-translators
            (- (ceiling (first (my-last pattern)))
               (floor (first (first pattern))))
            (gethash '"translators" pattern-hash)))))
  (if (null patterns-hash) ()
    (if (> (length new-translators) 1)
      (cons
       (progn
         (setf
          (gethash '"translators" pattern-hash)
          new-translators)
         pattern-hash)
       (remove-overlapping-translators-of-patterns
        (rest patterns-hash)))
      (remove-overlapping-translators-of-patterns
        (rest patterns-hash)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(setq
 patterns-hash3
 (remove-patterns-equalp-trans&intersect
  patterns-hash))
--> (#<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A38CFD>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A386ED>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A380DD>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x300041A37ACD>)
\end{verbatim}

\noindent This function applies the function
indices-of-patterns-equalp-trans&intersect
recursively. The result is that the lower-rating of
any pair of patterns is removed if the two patterns
have the same translation vectors and their first
occurrences have intersecting datapoints. It is
assumed that each pattern has already been arranged so
that its first translation vector is the zero
vector. |#

(defun remove-patterns-equalp-trans&intersect
       (patterns-hash &optional
        (result (list (first patterns-hash)))
        (pattern-hash (first patterns-hash))
        (rest-patterns-hash (rest patterns-hash))
        (new-patterns-hash
         (if rest-patterns-hash
           (remove-nth-list
           (indices-of-patterns-equalp-trans&intersect
            (gethash '"pattern" pattern-hash)
            (gethash '"translators" pattern-hash)
            rest-patterns-hash)
           rest-patterns-hash))))
  (if (null patterns-hash) result
    (remove-patterns-equalp-trans&intersect
     new-patterns-hash
     (if new-patterns-hash
       (append
        result (list (first new-patterns-hash)))
       result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 pattern&sources
 '((((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
     (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
     (7/2 60 60 1/2))
    16/23 (140 5 0) 1 (104 -5 0) 4/5 (96 -5 0))
   (((1/2 72 67 1/2) (3/2 79 71 1/2))
    1 (130 0 0) 2/3 (100 0 1/2))))
(remove-patterns-shorter-than pattern&sources 3)
--> ((((1/2 72 67 1/2) (1 76 69 1/2)
       (3/2 79 71 1/2) (2 84 74 2) (5/2 67 64 1/2)
       (3 64 62 1/2) (7/2 60 60 1/2))
      16/23 (140 5 0) 1 (104 -5 0) 4/5 (96 -5 0)))
\end{verbatim}

\noindent Let a be the floor of the first ontime
and b be the ceiling of the last offtime of a
pattern. If this is less than the optional variable
duration-threshold, then this pattern will not
appear in the output of this function. |#

(defun remove-patterns-shorter-than
       (pattern-compact-vec-triples &optional
        (duration-threshold 3)
        (first-triple
         (first pattern-compact-vec-triples))
        (first-pattern (first first-triple)))
  (if (null first-pattern) ()
    (if (>= (- (ceiling
                (first (my-last first-pattern)))
               (floor
                (first (first first-pattern))))
            duration-threshold)
      (cons
       first-triple
       (remove-patterns-shorter-than
        (rest pattern-compact-vec-triples)
        duration-threshold))
      (remove-patterns-shorter-than
        (rest pattern-compact-vec-triples)
        duration-threshold))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(subset-score-of-pattern
 (gethash '"pattern" (nth 3 patterns-hash))
 3 patterns-hash)
--> 1
\end{verbatim}

\noindent This function takes a pattern as its first
argument, called the probe pattern, and a hash table
of patterns as its second argument. It counts and
returns the number of patterns in the hash table
(including translations) of which the probe pattern is
a subset. |#

(defun subset-score-of-pattern
       (probe-pattern index-in-patterns-hash
        patterns-hash &optional
        (n (length patterns-hash)) (i 0)
        (growing-result 0)
        (pattern-hash
         (if (< i n) (nth i patterns-hash)))
        (result
         (if (and
              pattern-hash
              (not (equalp i index-in-patterns-hash)))
           (my-last
            (fibonacci-list
             (mapcar
              #'(lambda (x)
                  (if (subset-multidimensional
                       probe-pattern x) 1 0))
              (translations
               (gethash
                '"pattern" pattern-hash)
               (gethash
                '"translators" pattern-hash)))))
           0)))
  (if (>= i n) growing-result
    (subset-score-of-pattern
     probe-pattern index-in-patterns-hash
     patterns-hash n (+ i 1)
     (+ growing-result result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(setq
 patterns-hash4
 (subset-scores-of-patterns+ patterns-hash))
--> (#<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188F86D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188F25D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188EC4D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188E63D>)
\end{verbatim}

\noindent This function applies the function subset-
score-of-pattern to each pattern (including
translations) listed in a hash table of patterns. It
also creates inheritance indices (for example the
first occurrence of the highest-rating pattern is
labelled $P_{0,0}$) and a variable called inheritance
addressed, set to "No" by default, but will revert to
"Yes" when patterns are incorporated into the
generated passage. This function is the last step in
preparing a hash table of patterns for generation with
pattern inheritance. |#

(defun subset-scores-of-patterns+
       (patterns-hash &optional
        (n (length patterns-hash)) (i 0)
        (pattern-hash
         (if (< i n) (nth i patterns-hash)))
        (pattern
         (if pattern-hash
           (gethash '"pattern" pattern-hash)))
        (translators
         (if pattern-hash
           (gethash '"translators" pattern-hash)))
        (m (length translators))
        (subset-scores
         (if pattern
           (mapcar
            #'(lambda (x)
                (subset-score-of-pattern
                 x i patterns-hash n))
            (translations pattern translators))))
        (inheritance-indices
         (if pattern
           (mapcar
            #'(lambda (x)
                (list i (- x 1)))
            (reverse (first-n-naturals m)))))
        (inheritance-addressed
         (if pattern "No" )))
  (if (>= i n) ()
    (cons
     (progn
       (setf
        (gethash '"subset scores" pattern-hash)
        subset-scores)
       (setf
        (gethash '"inheritance indices" pattern-hash)
        inheritance-indices)
       (setf
        (gethash
         '"inheritance addressed" pattern-hash)
        inheritance-addressed)
       pattern-hash)
     (subset-scores-of-patterns+
      patterns-hash n (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 pattern
 '((1/2 72 67 1/2) (1 76 69 1/2) (3/2 79 71 1/2)
   (2 84 74 2) (5/2 67 64 1/2) (3 64 62 1/2)
   (7/2 60 60 1/2)))
(setq translators '((-1/2 0 0 0) (0 0 0 0) (3 2 1 0)))
(translate-pattern-to-1st-occurrence
 pattern translators)
--> (((0 72 67 1/2) (1/2 76 69 1/2) (1 79 71 1/2)
      (3/2 84 74 2) (2 67 64 1/2) (5/2 64 62 1/2)
      (3 60 60 1/2))
     ((0 0 0 0) (1/2 0 0 0) (7/2 2 1 0)))
\end{verbatim}

\noindent Sometimes an occurrence of a pattern is
found, other than the first occurrence in a piece.
This function takes such instances and rearranges the
pattern and the translators, so it is the first
occurrence which is displayed. |#

(defun translate-pattern-to-1st-occurrence
       (pattern translators &optional
        (first-translator (first translators))
        (zero-vector
         (constant-vector
          0 (length first-translator))))
  (if (vector<vector-t-or-nil
       first-translator zero-vector)
    (list
     (translation pattern first-translator)
     (translation
      translators
      (multiply-list-by-constant
       first-translator -1)))
    (list pattern translators)))

#|
\noindent Example:
\begin{verbatim}
(setq
 patterns-hash
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(setq
 patterns-hash5
 (translate-patterns-to-1st-occurrences
  patterns-hash))
--> (#<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188F86D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188F25D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188EC4D>
     #<HASH-TABLE :TEST EQUAL
     size 15/60 #x30004188E63D>)
\end{verbatim}

\noindent This function applies the function
translate-pattern-to-1st-occurrence recursively to a
list consisting of hash tables. Each hash table
contains information about a discovered pattern, as
returned by the function evaluate-variables-of-
patterns2hash. The output is an updated hash table. |#

(defun translate-patterns-to-1st-occurrences
       (patterns-hash &optional
        (pattern-hash (first patterns-hash))
        (pattern&translators
         (if pattern-hash
           (translate-pattern-to-1st-occurrence
            (gethash '"pattern" pattern-hash)
            (gethash '"translators" pattern-hash))))
        (region&translators
         (if pattern-hash
           (translate-pattern-to-1st-occurrence
            (gethash '"region" pattern-hash)
            (gethash '"translators" pattern-hash)))))
  (if (null patterns-hash) ()
    (cons
     (progn
       (setf
        (gethash '"pattern" pattern-hash)
        (first pattern&translators))
       (setf
        (gethash '"region" pattern-hash)
        (first region&translators))
       (setf
        (gethash '"translators" pattern-hash)
        (second pattern&translators))
       pattern-hash)
     (translate-patterns-to-1st-occurrences
      (rest patterns-hash)))))
