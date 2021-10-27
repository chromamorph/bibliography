#| Copyright 2008-2011 Tom Collins
   Friday 29 January 2010

These functions allow common vector operations, such
as taking norms, calculating dot products and distance
functions. |#

; REQUIRED PACKAGES
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
(fibonacci-list '(0 1 2 4 8))
-->
'(0 1 3 7 15).
\end{verbatim}

\noindent The $n$th element of the list returned is
the sum of the previous $n-1$ elements, with the
convention that a sum over an empty set is zero. |#

(defun fibonacci-list (a-list &optional (last-sum 0))
  (if (null a-list) ()
    (let ((next-sum (+ (first a-list) last-sum)))
      (cons
       next-sum
       (fibonacci-list (rest a-list) next-sum)))))

#|
\noindent Example:
\begin{verbatim}
(multiply-two-lists '(4 7 -3) '(8 -2 -3))
--> (32 -14 9).
\end{verbatim}

\noindent Multiplies two lists element-by-element. It
is assumed that elements of list arguments are
numbers, and the list arguments are of the same
length. An empty first (but not second) argument will
be tolerated. |#

(defun multiply-two-lists (a-list b-list)
  (if (null a-list) ()
    (cons (* (first a-list) (first b-list))
          (multiply-two-lists (rest a-list)
                         (rest b-list)))))

#|
\noindent Example:
\begin{verbatim}
(normalise-0-1 '(4 7 -3 2))
--> (7/10 1 0 1/2).
\end{verbatim}

\noindent Normalises data (linearly) to $[0, 1]$. |#

(defun normalise-0-1
       (a-list &optional
        (min-a-list (reduce #'min a-list))
        (max-a-list (reduce #'max a-list)))
  (if (or
       (equal min-a-list max-a-list)
       (and (equal min-a-list 0)
             (equal max-a-list 1)))
    (identity a-list)
    (normalise-0-1-checks-done
     a-list min-a-list max-a-list)))

#|
\noindent Example:
\begin{verbatim}
(normalise-0-1-checks-done '(4 7 -3 2))
--> (7/10 1 0 1/2).
\end{verbatim}

\noindent Normalises data (linearly) to $[0, 1]$,
assuming that the data is not constant and that the
min and max are not already 0, 1 respectively. |#

(defun normalise-0-1-checks-done
       (a-list min-a-list max-a-list &optional
        (denom (- max-a-list min-a-list)))
  (mapcar
    #'(lambda (x)
        (/ (- x min-a-list) denom))
    a-list))
