#| Copyright 2008-2011 Tom Collins
   Wednesday 6 October 2010

The functions below are for finding summary
statistics, and for taking random samples from
data. |#

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
  "/Maths foundation/vector-operations.lisp"))

; (defvar *rs* (make-random-state t))

#|
\noindent Example:
\begin{verbatim}
(choose-one '(1 2 4))
--> 4
\end{verbatim}

\noindent A random, equiprobable choice is made
between elements of a list. |#

(defun choose-one
       (a-list &optional (n (length a-list)))
  (if (null a-list) ()
    (nth (random n *rs*) a-list)))

#|
\noindent Example:
\begin{verbatim}
(mean '(6 7 4))
--> 17/3
\end{verbatim}

\noindent The mean of a list of numbers is
returned. |#

(defun mean (a-list)
  (/
   (my-last (fibonacci-list a-list)) (length a-list)))

#|
\noindent Example:
\begin{verbatim}
(random-permutation '("A" "B" "C" "D" "E"))
--> ("C" "A" "E" "D" "B")
\end{verbatim}

\noindent The output of this function is a random
permutation of an input list. |#

(defun random-permutation
       (a-list &optional (n (length a-list))
        (indices
         (sample-integers-no-replacement n n)))
  (nth-list indices a-list))

#|
\noindent Example:
\begin{verbatim}
(range '(60 61 62))
--> 2

\end{verbatim}

\noindent Range is the maximum member of a list, minus
the minimum member. |#

(defun range (data)
  (- (max-item data) (min-item data)))

#|
\noindent Example:
\begin{verbatim}
(sample-integers-no-replacement 10 7)
--> (5 4 8 2 0 3 9)
\end{verbatim}

\noindent The first argument to this function, $n$, is
an integer, as is the second $m \leq n$. The output is
a random sample (without replacement) from the
integers $0,\ldots, n-1$ of size $m$. If $m > n$, we
set $m = n$. |#

(defun sample-integers-no-replacement
       (n m &optional
        (m (if (> m n) n m))
        (integer-subset
         (add-to-list
          -1 (reverse (first-n-naturals n))))
        (sample nil)
        (choice
         (choose-one integer-subset n)))
  (if (zerop m) sample
    (sample-integers-no-replacement
     (- n 1) m (- m 1)
     (remove choice integer-subset :test #'equalp)
     (cons choice sample))))

#|
\noindent Example:
\begin{verbatim}
(sd '(64 55 65 55 72 55 55 55 60 59 67))
--> 5.7178855
\end{verbatim}

\noindent The standard deviation of the sample (using
a denominator of $n$, where $n$ is the sample
size). |#

(defun sd
       (data &optional (x-bar (mean data))
        (square-deviations
         (mapcar
          #'(lambda (x) (expt (- x x-bar) 2))
          data)))
  (sqrt
   (/
    (my-last (fibonacci-list square-deviations))
    (length data))))
