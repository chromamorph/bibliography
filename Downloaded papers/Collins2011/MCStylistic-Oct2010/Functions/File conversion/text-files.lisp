#| Copyright 2008-2011 Tom Collins
   Friday 15 January 2010

The functions below will export a list to a text
file, and import such text into the Lisp
environment as lists. |#

; REQUIRED PACKAGES
; (in-package :common-lisp-user)

#|
\noindent Example:
\begin{verbatim}
(read-from-file
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list.txt"))
--> ((9 23 1 19) (14 9 14 5 20 25) (16 5 18 3 5 14 20)
     ("sure" 9 4) (13 9 19 8 5 1 18 4) (8 5 18))
\end{verbatim}

\noindent This function returns the contents of a file
specified by the variable path\&name. It returns each
row of the file as a list, in a list of lists. |#

(defun read-from-file (path&name)
  (if (null path&name)
    ()
    (with-open-file (stream path&name)
      (loop for row = (read-line stream nil)
        while row do
        (setf row (read-from-string row nil))
        when row collect row into results
        finally (return results)))))

#|
\noindent Example:
\begin{verbatim}
(read-from-file-arbitrary
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 2.txt"))
--> ("first line consisting of anything"
     "sure 9 4" "second line consisting of &^%$")
\end{verbatim}

\noindent This function is similar to the function
read-from-file. The difference is that read-from-file-
arbitrary will parse any file, converting each line to
a string for further processing. |#

(defun read-from-file-arbitrary (path&name)
  (if (null path&name)
    ()
    (with-open-file (stream path&name)
      (loop for row = (read-line stream nil)
        while row do (setf row row)
        when row collect row into results
        finally (return results)))))

#|
\noindent Example:
\begin{verbatim}
(read-from-file
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 3.txt"))
--> (((0 7) (0 60) (2 63))
     ((2 2) (2 72))
     ((3 -1) (0 60) (3 67))
     ((6 0) (3 67) (5 66)))
(update-written-file
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list")
 3 '(6 60) '((2 2) . ((2 72))))
--> T
(read-from-file
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 3.txt"))
--> (((0 7) . ((0 60) (2 63)))
     ((2 2) . ((2 72) (6 60)))
     ((3 -1) . ((0 60) (3 67)))
     ((6 0) . ((3 67) (5 66))))
\end{verbatim}

\noindent This function updates the contents of a
specifed file by removing the row associated with the
variable updatee, and replacing it with updater
appended within updatee. It should overwrite the
existing file. The position of the row is
preserved. |#

(defun update-written-file
       (path&name filename-counter updater updatee
        &optional
        (list-to-update
	 (read-from-file
          (concatenate
           'string
           path&name " "
           (write-to-string filename-counter)
           ".txt"))))
  (progn
    (rplacd (assoc (car updatee) list-to-update
                   :test #'equalp)
            (append
	     (cdr updatee) (list updater)))
    (write-to-file
     list-to-update
     (concatenate
      'string
      path&name " "
      (write-to-string filename-counter)
      ".txt"))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file
 '(5 7 8 "hello" 9)
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 4.txt"))
--> T
\end{verbatim}

\noindent This function writes the data provided in
the first list to a file with the path and name
provided in the second list. The s in the format
argument is essential for retaining strings as they
appear in the data. |#

(defun write-to-file
       (variable path&name
        &optional
        (file
         (open
          path&name :direction :output
          :if-does-not-exist :create
          :if-exists :overwrite)))
  (if (null variable) (close file)
    (progn
      (format file "~s~%" (first variable))
      (write-to-file
       (rest variable) path&name file))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file-append
 '(10 "goodbye")
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 4.txt"))
--> T
\end{verbatim}

\noindent The only difference between this and the
function write-to-file is that an existing file will
be opened and new data appended, rather than
overwritten. |#

(defun write-to-file-append
       (variable path&name &optional
	(file
	 (open
	  path&name :direction :output
	  :if-does-not-exist :create
	  :if-exists :append)))
  (if (null variable) (close file)
    (progn (format file "~s~%" (first variable))
      (write-to-file-append
       (rest variable) path&name file))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file
 '(5 7 8 "hello" 9)
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 5.txt"))
--> T
(write-to-file-supersede
 '(10 "goodbye")
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/short-list 5.txt"))
--> T
\end{verbatim}

\noindent The only difference between this and the
function write-to-file is that an existing file will
be superseded, rather than overwritten. |#

(defun write-to-file-supersede
       (variable path&name &optional
	(file
	 (open
	  path&name :direction :output
	  :if-does-not-exist :create
	  :if-exists :supersede)))
  (if (null variable) (close file)
    (progn (format file
                   "~s~%" (first variable))
      (write-to-file (rest variable)
                     path&name
                     file))))

#|
\noindent Example:
\begin{verbatim}
(write-to-file-with-open-file
 "hello"
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/New folder/short-list.txt"))
--> T
\end{verbatim}

\noindent There was a problem with the function write-
to-file in Emacs, because it would not export to a
directory that did not already exist. This was
remedied using the functions with-open-file and
ensure-directories-exist. However, this function
only works with a single (i.e. non-list) variable.
Once you have used it to create the directory, use
the function write-to-file as per usual. |#

(defun write-to-file-with-open-file
       (variable path&name)
  (with-open-file
      (out
       (ensure-directories-exist path&name)
       :direction :output)
    (format out "~s~%" variable)))
