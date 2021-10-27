#| Copyright 2008-2011 Tom Collins
   Friday 30 July 2010

These functions enable the conversion of csv files
into lists of lists, and vice versa. Despite using
terms like dataset below, neither representation has
to be balanced, that is, rows/lists can contain
different numbers of elements. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/File conversion/text-files.lisp"))

#|
\noindent Example:
\begin{verbatim}
(comma-positions "uh,ktr,3,4")
--> (2 6 8)
\end{verbatim}

\noindent This function returns the positions at which
commas occur in a string. |#

(defun comma-positions
       (string &optional (start 0)
        (local-result
         (position
          #\,
          (subseq string start)))
        (result
         (if local-result
           (+ start local-result))))
  (if (null result) ()
    (cons result
          (comma-positions string (+ result 1)))))

#|
\noindent Example:
\begin{verbatim}
(comma-separated-integers2list "1.00, 3.00")
--> (1 3)
\end{verbatim}

\noindent This function applies the function parse-
integer recursively, once the string supplied as an
argument has had the function comma-separated-
string2list applied. |#

(defun comma-separated-integers2list (a-string)
  (mapcar
    #'(lambda (x)
        (parse-integer x :junk-allowed t))
    (comma-separated-string2list a-string)))

#|
\noindent Example:
\begin{verbatim}
(comma-separated-string2list "uh,ktr,3,4")
--> ("uh" "ktr" "3" "4")
\end{verbatim}

\noindent This function turns a comma-separated string
into a list, where formerly each item was preceded or
proceeded by a comma. |#

(defun comma-separated-string2list
       (comma-separated-string &optional
	(commas-positioned
	 (cons -1
	       (comma-positions
		comma-separated-string)))
	(result nil))
  (if (equal (length commas-positioned) 1)
    (append
     result
     (list (subseq comma-separated-string
		   (+ (first commas-positioned) 1))))
    (comma-separated-string2list
     comma-separated-string
     (rest commas-positioned)
     (append
      result
      (list (subseq comma-separated-string
		    (+ (first commas-positioned) 1)
		    (second commas-positioned)))))))

#|
\noindent Example:
\begin{verbatim}
(csv2dataset
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list.csv"))
--> ((1 3) (2 6) (5 2 2) (6 2))
\end{verbatim}

\noindent This function converts a file in comma-
separated-value (CSV) format to a dataset. At present
it is assumed each value is an integer, but it would
be worth making this more robust. |#

(defun csv2dataset (path&name)
  (mapcar
    #'(lambda (x)
        (comma-separated-integers2list x))
    (read-from-file-arbitrary path&name)))

#|
\noindent Example:
\begin{verbatim}
(dataset2csv
 '((1/3 3) (2/6 6) (5 2 2) (6 2))
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/csv-export-test.csv"))
--> T
\end{verbatim}

\noindent This function converts a dataset (a list of
lists of equal length) to a csv file. The first
argument is either the path where the dataset resides
or the dataset itself. The example causes a file to be
created in the specified location. |#

(defun dataset2csv
       (data-path csv-path &optional
	(dataset
         (if (stringp data-path)
           (read-from-file data-path)
           (identity data-path)))
	(file
	 (open
	  csv-path
	  :direction :output :if-does-not-exist
	  :create :if-exists :overwrite)))
  (if (null dataset) (close file)
    (progn
      (format file "~{~10$~^, ~}~%" (first dataset))
      (dataset2csv
       data-path csv-path (rest dataset) file))))

#|
\noindent Example:
\begin{verbatim}
(list-of-lists2csv
 '((1/3 3) (2/6 6) (5 2 2) (6 2))
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/csv-export-test 2.csv"))
--> T
\end{verbatim}

\noindent This function converts a balanced list of
lists to a csv file. The first argument is either the
path where the dataset resides or the dataset
itself. The example causes a file to be created in the
specified location. The function list-of-lists2csv is
very similar to the function dataset2csv, the
difference being that the former does not round
fractions to decimal places. |#

(defun list-of-lists2csv
       (data-path csv-path &optional
	(dataset
         (if (stringp data-path)
           (read-from-file data-path)
           (identity data-path)))
	(file
	 (open
	  csv-path
	  :direction :output :if-does-not-exist
	  :create :if-exists :overwrite)))
  (if (null dataset) (close file)
    (progn
      (format file "~{~a~^,~}~%" (first dataset))
      (list-of-lists2csv
       data-path csv-path (rest dataset) file))))
