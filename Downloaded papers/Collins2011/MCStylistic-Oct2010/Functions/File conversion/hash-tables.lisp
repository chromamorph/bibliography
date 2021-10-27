#| Copyright 2008-2011 Tom Collins
   Monday 25 January 2010

The functions below are for saving, reading,
displaying and querying hash-tables. It can be
convenient to work with lists, each of whose elements
is a hash table. |#

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
  "/Maths foundation/set-operations.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Pattern discovery/structural-induction-mod.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/File conversion/text-files.lisp"))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-hash-table :test #'equal))
(setf (gethash '"hair colour" A) "brown")
(setq B (copy-hash-table A))
(list A B)
--> (#<HASH-TABLE
        :TEST EQUAL size 1/60 #x300041A694FD>
     #<HASH-TABLE
        :TEST EQUAL size 1/60 #x300041A683DD>)
\end{verbatim}

\noindent This function returns a copy of hash table,
with the same keys and values. The copy has the same
properties as the original, unless overridden by the
keyword arguments.

Before each of the original values is set into the new
hash-table, key is invoked on the value. As key
defaults to cl:identity, a shallow copy is returned by
default. Adapted from http://common-lisp.net/project
/alexandria/darcs/alexandria/hash-tables.lisp. |#

(defun copy-hash-table
       (table &key key test size
        rehash-size rehash-threshold)
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size
        (or rehash-size
            (hash-table-rehash-size table)))
  (setf rehash-threshold
        (or rehash-threshold
            (hash-table-rehash-threshold table)))
  (let ((copy
         (make-hash-table
          :test test :size size
          :rehash-size rehash-size
          :rehash-threshold rehash-threshold)))
    (maphash
     (lambda (k v)
       (setf (gethash k copy) (funcall key v)))
     table)
    copy))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-list-of-hash-tables 2))
(setf (gethash '"hair colour" (first A)) "brown")
(setf (gethash '"eye colour" (first A)) "brown")
(setf (gethash '"hair colour" (second A)) "blond")
(setf (gethash '"gender" (second A)) "male")
(disp-ht-el (first A))
--> (("hair colour" . "brown")
     ("eye colour" . "brown"))
\end{verbatim}

\noindent This function displays the contents of a
hash table. |#

(defun disp-ht-el
       (hash-table-el &optional
        (copy-el (copy-hash-table hash-table-el))
        (result nil))
  (progn
    (maphash
     #'(lambda (key val) 
         (progn
           (setf
            result
            (append result (list (cons key val))))
           (remhash key copy-el))) 
     copy-el)
    (identity result)))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-hash-table :test #'equalp))
(setf (gethash '"hair colour" A) "brown")
(setf (gethash '"eye colour" A) "brown")
(setf (gethash '"gender" A) "male")
(disp-ht-key A)
--> ("hair colour" "eye colour" "gender")
\end{verbatim}

\noindent This function displays the keys of a hash
table. |#

(defun disp-ht-key
       (hash-table-el &optional
        (copy-el (copy-hash-table hash-table-el))
        (result nil))
  (progn
    (maphash
     #'(lambda (key value)
         value
         (progn
           (setf
            result
            (append result (list key)))
           (remhash key copy-el)))
     copy-el)
    (identity result)))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-list-of-hash-tables 2))
(setf (gethash '"hair colour" (first A)) "brown")
(setf (gethash '"eye colour" (first A)) "brown")
(setf (gethash '"name" (first A)) "Chris")
(setf (gethash '"hair colour" (second A)) "blond")
(setf (gethash '"gender" (second A)) "male")
(setf (gethash '"name" (second A)) "Chris")
(setq
 B
 (hash-tables-with-key-value-pairs
  A '(("name" . "Chris") ("hair colour" . "brown"))))
--> (#<HASH-TABLE
     :TEST EQUAL size 3/60 #x3000417DF6FD>)
\end{verbatim}

\noindent This function returns those hash tables in
a list that have key-value pairs equalp to those
specified in the second argument. The example returns
a list of one hash table because only the first hash
table contains information for somebody called Chris
with brown hair. |#

(defun hash-tables-with-key-value-pairs
       (list-of-hash-tables key-value-pairs &optional
        (logical-outcome
         (constant-vector t (length key-value-pairs)))
        (hash-table (first list-of-hash-tables)))
  (if (null hash-table) ()
    (if (equalp
         (mapcar
          #'(lambda (x)
              (equalp
               (gethash (car x) hash-table) (cdr x)))
          key-value-pairs)
         logical-outcome)
      (cons
       hash-table
       (hash-tables-with-key-value-pairs
        (rest list-of-hash-tables) key-value-pairs
        logical-outcome))
      (hash-tables-with-key-value-pairs
       (rest list-of-hash-tables) key-value-pairs
       logical-outcome))))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(disp-ht-el (fourth A))
(index-target-translation-in-hash-tables
 '((4 38 47) (9/2 38 47) (5 38 47)) A "pattern")
--> 3
\end{verbatim}

\noindent The hash tables each contain a value
specified by the third argument (a key). We think of
these as patterns, and want to know if any of the
patterns are translations of the first argument, the
target. The index of the first extant translation is
returned, and nil otherwise. |#

(defun index-target-translation-in-hash-tables
       (target hash-tables key &optional (i 0))
  (if (null hash-tables) ()
    (if (test-translation
	 target
	 (gethash key (first hash-tables)))
      (identity i)
      (index-target-translation-in-hash-tables
       target (rest hash-tables) key (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(disp-ht-el (fourth A))
(index-target-translation-mod-in-hash-tables
 '((0 36 46) (1/2 48 46) (1 36 46)) A 12 "pattern")
--> 3
\end{verbatim}

\noindent This function is very similar to the
function index-target-translation-in-hash-tables,
except that in the second dimension translations are
carried out modulo the third argument. |#

(defun index-target-translation-mod-in-hash-tables
       (target hash-tables n key &optional (i 0))
  (if (null hash-tables) ()
    (if (test-translation-mod-2nd-n
	 target
	 (gethash key (first hash-tables)) n)
      (identity i)
      (index-target-translation-mod-in-hash-tables
       target (rest hash-tables) n key (+ i 1)))))

#|
\noindent Example:
\begin{verbatim}
(make-list-of-hash-tables 3)
--> (#<HASH-TABLE
        :TEST EQUAL size 0/60 #x300041BB8A5D>
     #<HASH-TABLE
        :TEST EQUAL size 0/60 #x300041BB84AD>
     #<HASH-TABLE
        :TEST EQUAL size 0/60 #x300041BB7EFD>)
\end{verbatim}

\noindent This function returns a list, each of whose
elements is an empty hash table of type `equal'. |#

(defun make-list-of-hash-tables (n)
  (if (<= n 0) ()
    (cons
     (make-hash-table :test #'equal)
     (make-list-of-hash-tables (- n 1)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(number-of-targets-trans-mod-in-hash-tables
 '(((0 36 46) (1/2 60 46) (1 36 46))
   ((0 60 46) (0 48 53) (0 55 57) (0 60 60) (0 64 62)
    (1/2 36 46) (1/2 48 53) (1/2 55 57) (1/2 62 61)
    (1/2 65 63) (1 36 46) (1 48 53) (1 55 57)
    (1 64 62) (1 67 64) (2 48 53) (2 65 63) (2 69 65)
    (3 36 46) (3 48 53) (3 55 57) (3 64 62) (3 67 64)
    (7/2 36 46) (7/2 48 53) (7/2 55 57) (7/2 60 60)
    (7/2 64 62) (4 36 46) (4 48 53) (4 55 57)))
 A 12 "pattern")
--> 2
\end{verbatim}

\noindent This function is very similar to the
function number-of-targets-translation-in-hash-tables,
except that in the second dimension translation is
performed modulo the third argument. |#

(defun number-of-targets-trans-mod-in-hash-tables
       (targets hash-tables n key
        &optional (result 0))
  (if (null targets) (identity result)
    (if (index-target-translation-mod-in-hash-tables
	 (first targets) hash-tables n key)
      (number-of-targets-trans-mod-in-hash-tables
       (rest targets) hash-tables n key (+ result 1))
      (number-of-targets-trans-mod-in-hash-tables
       (rest targets) hash-tables n key result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(number-of-targets-translation-in-hash-tables
 '(((6 48 53) (13/2 48 53) (7 48 53))
   ((24 29 42) (24 41 49) (24 48 53) (24 53 56)
    (24 57 58) (49/2 29 42) (49/2 41 49) (49/2 48 53)
    (49/2 55 57) (49/2 58 59) (25 29 42) (25 41 49)
    (25 48 53) (25 57 58) (25 60 60) (26 41 49)
    (26 58 59) (26 62 61) (27 29 42) (27 41 49)
    (27 48 53) (27 57 58) (27 60 60) (55/2 29 42)
    (55/2 41 49) (55/2 48 53) (55/2 53 56)
    (55/2 57 58) (28 29 42) (28 41 49) (28 48 53)))
 A "pattern")
--> 2
\end{verbatim}

\noindent The function test-target-translation-in-
hash-tables is applied recursively to each member of
the first argument of this function. This argument is
a list of targets. Each time a translation of a target
is detected, the output (initially set to zero) is
incremented by one. |#

(defun number-of-targets-translation-in-hash-tables
       (targets hash-tables key &optional (result 0))
  (if (null targets) (identity result)
    (if (test-target-translation-in-hash-tables
	 (first targets) hash-tables key)
      (number-of-targets-translation-in-hash-tables
       (rest targets) hash-tables key (+ result 1))
      (number-of-targets-translation-in-hash-tables
       (rest targets) hash-tables key result))))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/small-hash-table.txt")))
(setq A (re-index-list-of-hash-tables A 780))
(gethash '"index" (first A))
--> 780
    T
\end{verbatim}

\noindent This function re-indexes a list of hash
tables beginning from an optional second argument. |#

(defun re-index-list-of-hash-tables
       (a-list &optional (starting-index 0))
  (loop for i from 0 to (- (length a-list) 1) do
    (setf
     (gethash '"index" (nth i a-list))
     (+ starting-index i)))
  (identity a-list))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/small-hash-table.txt")))
(disp-ht-el (second A))
--> (("height" . "6ft") ("name" . "Justin")
     ("index" . 77))
\end{verbatim}

\noindent This function reads a balanced list of hash
tables that have been written to a file, by the
function write-to-file-balanced-hash-table. It is
assumed that the hash tables are homogeneous or
balanced in the sense that they contain exactly the
same keys. |#

(defun read-from-file-balanced-hash-table
       (path&name &optional
        (all-info (read-from-file path&name))
        (n (first all-info))
        (m (second all-info))
        (hash-list (third all-info))
        (list-of-hash-tables
	 (make-list-of-hash-tables n)))
  (loop for j from 0 to (- n 1) do
    (loop for i from 0 to (- m 1) do
      (setf
       (gethash (car (nth i (nth j hash-list)))
                (nth j list-of-hash-tables))
       (cdr (nth i (nth j hash-list))))))
  (identity list-of-hash-tables))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-list-of-hash-tables 2))
(setf (gethash '"hair colour" (first A)) "brown")
(setf (gethash '"eye colour" (first A)) "brown")
(setf (gethash '"hair colour" (second A)) "blond")
(setf (gethash '"gender" (second A)) "male")
(set-each-hash-table-element A "height" "tall")
(list
 (gethash '"height" (first A))
 (gethash '"height" (second A)))
--> ("tall" "tall")
\end{verbatim}

\noindent This function is useful if you have a list
of hash tables and you want to set each hash table to
have an identical key-value pair. |#

(defun set-each-hash-table-element
       (list-of-hash-tables key value)
  (progn
    (mapcar
     #'(lambda
           (x)
         (setf (gethash key x) value))
     list-of-hash-tables)
    (identity list-of-hash-tables)))

#|
\noindent Example:
\begin{verbatim}
(setq
 A
 (read-from-file-balanced-hash-table
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/patterns-hash.txt")))
(disp-ht-el (fourth A))
(test-target-translation-in-hash-tables
 '((6 48 53) (13/2 48 53) (7 48 53)) A "pattern")
--> T
\end{verbatim}

\noindent The hash tables each contain a value
specified by the third argument (a key). We think of
these as patterns, and want to know if any of the
patterns are translations of the first argument, the
target. T is returned if a translation does exist
among the hash tables, and nil otherwise. |#

(defun test-target-translation-in-hash-tables
       (target hash-tables key)
  (if (null hash-tables) ()
    (if (test-translation
	 target
	 (gethash key (first hash-tables)))
      (identity T)
      (test-target-translation-in-hash-tables
       target (rest hash-tables) key))))

#|
\noindent Example:
\begin{verbatim}
(setq A (make-list-of-hash-tables 2))
(setf (gethash '"hair colour" (first A)) "brown")
(setf (gethash '"eye colour" (first A)) "brown")
(setf (gethash '"height" (first A)) 187)
(setf (gethash '"hair colour" (second A)) "blond")
(setf (gethash '"gender" (second A)) "male")
(write-to-file-balanced-hash-table
 A
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/small-hash-table 2.txt"))
--> T
\end{verbatim}

\noindent This function takes as its first argument a
list, each of whose elements is a hash table. It
applies the function disp-ht-el to each element,
collects the output and writes it to a text file. At
the top of the file are two integers $n$ and $m$,
referring to the length of the list and the number of
elements in a hash table respectively. It is assumed
that the hash tables are homogeneous or balanced in
the sense that they contain exactly the same keys, but
as the example demostrates, this does not have to be
the case. (Balanced hash tables are easier to read
back in.) |#

(defun write-to-file-balanced-hash-table
       (hash-table path&name &optional
        (n (length hash-table))
        (first-displayed
         (if hash-table
           (disp-ht-el (first hash-table))))
        (m (length first-displayed))
        (rest-displayed
         (if (second hash-table)
           (mapcar
            #'(lambda (x) (disp-ht-el x))
            (rest hash-table)))))
  (write-to-file
   (list
    n m
    (append
     (list first-displayed) rest-displayed))
   path&name))
