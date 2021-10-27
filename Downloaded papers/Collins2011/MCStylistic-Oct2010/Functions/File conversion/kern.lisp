#| Copyright 2008-2011 Tom Collins
   Wednesday 30 June 2010

The functions below will parse a kern file
(http://kern.ccarh.org/) and convert it to a dataset.
The main function is kern-file2dataset. Occasionally
there are conflicts between kern's relative encoding
and the timewise parsing function. These could be
resolved by writing a partwise parsing function. |#

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/File conversion/director-musices.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/File conversion/text-files.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/vector-operations.lisp"))

#|
\noindent Example:
\begin{verbatim}
(accidental-char-p #\a)
--> nil
\end{verbatim}

\noindent This function returns true if the input
character is associated with kern's representation of
accidentals. |#

(defun accidental-char-p (a-char)
  (if (find a-char '(#\# #\- #\n) :test #'equalp)
    (identity t) (identity nil)))

#|
\noindent Example:
\begin{verbatim}
(index-of-backward-tie
 '((4 62 61 1 0 "[") (5 63 61 1 0 "[")
   (21/4 64 62 1/8 0 "[") (43/8 63 61 1/8 0 "]")
   (45/8 64 62 1/8 0 "][") (23/4 62 61 1/8 0 "]")
   (47/8 64 62 1/8 0 "]") (6 63 61 1/4 0 "]")) 2)
--> 5
\end{verbatim}

\noindent This function returns the index of the
element that has the same MIDI-morphetic pairs as the
element indicated by the second argument, so long as
this element is tied backward. |#

(defun index-of-backward-tie
       (tied-datapoints index &optional
        (j (+ index 1)) (n (length tied-datapoints))
	(MIDI-note-number
	 (second (nth index tied-datapoints)))
	(morphetic-pitch-number
	 (third (nth index tied-datapoints))))
  (if (equalp j n) ()
    (if (and
         (equalp
          (second (nth j tied-datapoints))
          MIDI-note-number)
         (equalp
          (third (nth j tied-datapoints))
          morphetic-pitch-number)
         (string=
          (sixth (nth j tied-datapoints)) "]"))
      (identity j)
      (index-of-backward-tie
       tied-datapoints index (+ j 1) n
       MIDI-note-number morphetic-pitch-number))))

#|
\noindent Example:
\begin{verbatim}
(kern-dur-pitch2pitch&octave-dur "8e#")
--> ("E#4" 1/2)
\end{verbatim}

\noindent This function converts a kern note into
pitch-and-octave-number and a duration. It is assumed
that any irrelevant symbols have already been removed
via the function remove-if in combination with the
test function not-tie-dur-pitch-char-p as applied to
\texttt{*kern-note*}. Non-notes should then result
in nil being returned. |#

(defun kern-dur-pitch2pitch&octave-dur
       (dur-pitch-chars &optional
        (kern-duration
         (multiple-value-list
          (parse-integer
           dur-pitch-chars :junk-allowed t)))
        (duration
         (if (first kern-duration)
           (/ 4 (first kern-duration))))
        (dotted-pitch-chars
         (if duration
           (subseq
            dur-pitch-chars
            (second kern-duration))))
        (dotted
         (if (and
              dotted-pitch-chars
              (not (string= dotted-pitch-chars ""))
              (equalp
               (char dotted-pitch-chars 0) #\.))
           (identity 1) (identity 0)))
        (duration
         (if (and duration (equal dotted 1))
           (* duration 3/2) (identity duration)))
        (pitch-chars
         (if duration
           (subseq
            dotted-pitch-chars
            dotted)))
        (pitch&octave
         (if (and
              pitch-chars
              (not (string= pitch-chars ""))
              (not (string= pitch-chars "-")))
           (kern-pitch-chars2pitch&octave
            pitch-chars))))
  (if pitch&octave
    (list pitch&octave duration)))

#|
\noindent Example:
\begin{verbatim}
(firstn
 10
 (kern-file2dataset
  (concatenate
   'string
   *MCStylistic-Oct2010-example-files-path*
   "/vivaldi-op6-no3-2.txt")))
--> ((0 49 53 1 3) (0 49 53 1 5) (0 69 65 1 2)
     (0 76 69 1 1) (0 79 71 1 0) (1 49 53 1 3)
     (1 49 53 1 5) (1 69 65 1 2) (1 76 69 1 1)
     (1 79 71 1 0))
\end{verbatim}

\noindent This function converts a text file in the
kern format into a dataset, where each datapoint
consists of an ontime, MIDI note number, morphetic
pitch number, duration, and staff number. |#

(defun kern-file2dataset
       (path&name &optional
        (kern-rows
         (read-from-file-arbitrary path&name))
        (staves-variable
         (staves-info2staves-variable kern-rows))
        (kern-rows
         (subseq
          kern-rows (second staves-variable)))
        (staves-variable (first staves-variable))
        (ontime 0) (dataset nil) (tied-datapoints nil)
        (kern-row (first kern-rows))
        (result
         (if kern-rows
           (parse-kern-row
            kern-row staves-variable ontime))))
  (if (null kern-row)
    (sort-dataset-asc
     (resolve-ties-kern tied-datapoints dataset))
    (kern-file2dataset
     path&name nil nil (rest kern-rows)
     (first result) (second result)
     (append dataset (third result))
     (append tied-datapoints (fourth result)))))

#|
\noindent Example:
\begin{verbatim}
(kern-pitch-chars2pitch&octave "e#")
--> "E#4"
\end{verbatim}

\noindent This function converts kern pitch characters
into the pitch-and-octave-number representation. It
can accept junk input, but may produce junk output.
For example, try `.' or `$\ast$v' as input. |#

(defun kern-pitch-chars2pitch&octave
       (pitch-chars &optional
        (accidental-index
         (position-if
          #'accidental-char-p pitch-chars))
        (accidental-info
         (if accidental-index
           (remove
            #\n
            (substitute
             #\b #\-
             (subseq
              pitch-chars accidental-index)))
           (identity "")))
        (note-name
         (if accidental-index
           (subseq pitch-chars 0 accidental-index)
           (if (not (string= pitch-chars "r"))
             (identity pitch-chars))))
        (octave
         (if note-name
           (if (upcase-p (char note-name 0))
             (write-to-string
              (- 4 (length note-name)))
             (write-to-string
              (+ 3 (length note-name)))))))
  (if note-name
    (concatenate
     'string (string-upcase (char note-name 0))
     accidental-info octave)
    (identity "rest")))

#|
\noindent Example:
\begin{verbatim}
(kern-tie-dur-pitch2list "[8e#]")
--> ("E#4" 1/2 "][")
\end{verbatim}

\noindent This function converts a kern note into a
list consisting of pitch-and-octave, duration, and tie
type. It is assumed that any irrelevant symbols have
already been removed via the function remove-if in
combination with the test function
not-tie-dur-pitch-char-p as applied to
\texttt{*kern-note*}. Non-notes should then result in
nil being returned. |#

(defun kern-tie-dur-pitch2list
       (tie-dur-pitch-chars &optional
        (find-forward
         (find
          #\[ tie-dur-pitch-chars :test #'equalp))
        (find-backward
         (find
          #\] tie-dur-pitch-chars :test #'equalp)))
  (if find-forward
    (if find-backward
      (if (zerop
           (count-if
            #'number-chars-p tie-dur-pitch-chars))
        () ; avoids key sigs mistaken for notes 
        (append
         (kern-dur-pitch2pitch&octave-dur
          (remove
           #\[
           (remove #\] tie-dur-pitch-chars)))
         (list "][")))
      (append
       (kern-dur-pitch2pitch&octave-dur
        (remove #\[ tie-dur-pitch-chars))
       (list "[")))
    (if find-backward
      (append
       (kern-dur-pitch2pitch&octave-dur
        (remove #\] tie-dur-pitch-chars))
       (list "]"))
      (kern-dur-pitch2pitch&octave-dur
       tie-dur-pitch-chars))))

#|
\noindent Example:
\begin{verbatim}
(not-tie-dur-pitch-char-p #\h)
--> T
\end{verbatim}

\noindent This function returns true if the input
character is not associated with kern's representation
of pitch. |#

(defun not-tie-dur-pitch-char-p (a-char)
  (if (find
       a-char
       '(#\[ #\] #\.
         #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
         #\# #\- #\n
         #\r #\a #\b #\c #\d #\e #\f #\g)
       :test #'equalp)
    (identity nil) (identity t)))

#|
\noindent Example:
\begin{verbatim}
(number-chars-p #\2)
--> nil
\end{verbatim}

\noindent This function returns true if the input
character is 0-9. |#

(defun number-chars-p (a-char)
  (if (find
       a-char
       '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       :test #'equalp)
    (identity t)))

#|
\noindent Example:
\begin{Verbatim}[showtabs=true]
(parse-kern-row
 "2d#/ 2f#/	4A\	12cc#\L]	."
 '((1 2) (0 1) (1/2 1)) 15)
--> (((1 2) (0 1) (1/2 1))
     46/3
     ((15 63 61 2 1) (15 66 63 2 1) (15 57 58 1 1))
     ((15 73 67 1/3 0 "]")))
(parse-kern-row
 "*	*v	*v	*"
 '((1 1) (0 2) (1/2 1)) 15)
--> (((1 1) (0 1) (1/2 1)) 15)
(parse-kern-row
 ".	.	16r	."
 '((1 2) (0 1) (1/2 1)) 15)
--> (((1 2) (0 1) (1/2 1)) 61/4)
\end{Verbatim}

\noindent This function parses a kern row, consisting
of notes/rests, changes to the staves variable, or
irrelevant information for our purposes. The ouptut is
the staves variable, the new ontime, new datapoints,
and new tied datapoints. |#

(defun parse-kern-row
       (row staves-variable ontime &optional
        (parsed-row
         (mapcar
          #'(lambda (x)
              (mapcar
               #'(lambda (y)
                   (remove-if
                    #'not-tie-dur-pitch-char-p y))
               (space-bar-separated-string2list x)))
          (tab-separated-string2list row)))
        (minimum-duration 0)
        (min-dur&datapoints
         (parse-kern-row-as-notes
          parsed-row staves-variable ontime
          minimum-duration)))
  (if (second min-dur&datapoints)
    (list
     staves-variable
     (+ ontime (first min-dur&datapoints))
     (return-lists-of-length-n
      (second min-dur&datapoints) 5)
     (return-lists-of-length-n
      (second min-dur&datapoints) 6))
    (if (> (first min-dur&datapoints) 0)
      (list
       staves-variable
       (+ ontime (first min-dur&datapoints)))
      (if (or
           (string= (subseq row 0 2) "*	")
           (string= (subseq row 0 2) "*^")
           (string= (subseq row 0 2) "*v"))
        (list
         (update-staves-variable
          staves-variable row) ontime)
        (list staves-variable ontime)))))

#|
\noindent Example:
\begin{verbatim}
(parse-kern-row-as-notes
 '(("2d#" "2f#") ("4A") ("12cc#]") (""))
 '((1 2) (0 1) (1/2 1)) 15)
--> (1/3
     ((15 63 61 2 1) (15 66 63 2 1) (15 57 58 1 1)
      (15 73 67 1/3 0 "]")))
\end{verbatim}

\noindent This function converts a kern row consisting
of tabbed notes into a list of datapoints, and also
returns the minimum duration of those notes. It
recurses over the staves-variable to ensure that each
note is labelled correctly according to staff. It is
assumed that any irrelevant symbols have already been
removed via the the function remove-if in combination
with the test function not-tie-dur-pitch-char-p as
applied to \texttt{*kern-note*}. Non-notes/rests
should then result in '(0 NIL) being returned. A lone
crotchet rest should result in '(1 NIL) being
returned, etc. |#

(defun parse-kern-row-as-notes
       (a-list staves-variable ontime &optional
        (minimum-duration 0) (datapoints nil) (i 0)
        (n
         (if staves-variable
           (second (first staves-variable))
           (identity 0)))
        (staff-index (first (first staves-variable)))
        (min-dur&datapoints
         (if (< i n)
           (parse-kern-spaced-notes
            (first a-list) staff-index ontime
            minimum-duration))))
  (if (null staves-variable)
    (list minimum-duration datapoints)
    (if (equalp i n)
      (parse-kern-row-as-notes
       a-list (rest staves-variable) ontime
       minimum-duration datapoints)
      (parse-kern-row-as-notes
       (rest a-list) staves-variable ontime
       (if (zerop minimum-duration)
         (first min-dur&datapoints)
         (min
          minimum-duration
          (first min-dur&datapoints)))
       (append datapoints (second min-dur&datapoints))
       (+ i 1) n staff-index))))

#|
\noindent Example:
\begin{verbatim}
(parse-kern-spaced-notes
 '("[4C#" "4E#" "4B" "8g#") 0 3 0)
--> (1/2
     ((3 49 53 1 0 "[") (3 53 55 1 0) (3 59 59 1 0)
      (3 68 64 1/2 0)))
\end{verbatim}

\noindent This function converts a kern row
consisting of spaced notes into a list of datapoints,
and also returns the minimum duration of those notes.
It is assumed that any irrelevant symbols have already
been removed via the the function remove-if in
combination with the test function
not-tie-dur-pitch-char-p as applied to
\texttt{*kern-note*}. Non-notes/rests should then
result in '(0 NIL) being returned. A lone crotchet
rest should result in '(1 NIL) being returned, etc. |#

(defun parse-kern-spaced-notes
       (a-list staff-index ontime minimum-duration
        &optional (datapoints nil)
        (half-data
         (if a-list
           (kern-tie-dur-pitch2list
            (first a-list))))
        (MIDI-morphetic-pair
         (if half-data
           (if (not
                (string= (first half-data) "rest"))
             (pitch&octave2MIDI-morphetic-pair
              (first half-data))))))
  (if (null a-list)
    (list minimum-duration datapoints)
    (if (null half-data)
      (parse-kern-spaced-notes
       (rest a-list) staff-index ontime
       minimum-duration datapoints)
      (if (null MIDI-morphetic-pair)
        (parse-kern-spaced-notes
         (rest a-list) staff-index ontime
         (if (zerop minimum-duration)
           (second half-data)
           (min minimum-duration (second half-data)))
         datapoints)
        (parse-kern-spaced-notes
         (rest a-list) staff-index ontime
         (if (zerop minimum-duration)
           (second half-data)
           (min minimum-duration (second half-data)))
         (if (equalp (length half-data) 3)
           (append
            datapoints
            (list
             (list
              ontime (first MIDI-morphetic-pair)
              (second MIDI-morphetic-pair)
              (second half-data) staff-index
              (third half-data))))
           (append
            datapoints
            (list
             (list
              ontime (first MIDI-morphetic-pair)
              (second MIDI-morphetic-pair)
              (second half-data) staff-index)))))))))

#|
\noindent Example:
\begin{verbatim}
(resolve-ties-kern
 '((4 62 61 1 0 "[") (5 63 61 1 0 "[")
   (21/4 64 62 1/8 0 "[") (43/8 63 61 1/8 0 "]")
   (45/8 64 62 1/8 0 "][") (23/4 62 61 1/8 0 "]")
   (47/8 64 62 1/8 0 "]") (6 63 61 1/4 0 "]"))
 '((0 60 60 1 0)))
--> ((0 60 60 1 0) (4 62 61 15/8 0) (5 63 61 1/2 0)
     (21/4 64 62 3/4 0))
\end{verbatim}

\noindent This function resolves tied datapoints by
applying the function index-of-backward-tie
recursively. It is quite similar to the function
resolve-ties, which was defined for reading director-
musices files. |#

(defun resolve-ties-kern
       (tied-datapoints dataset &optional
        (index
         (if (and
              tied-datapoints
              (string=
               (sixth (first tied-datapoints)) "["))
           (index-of-backward-tie
            tied-datapoints 0))))
  (if (null tied-datapoints)
    (identity dataset)
    (if index
      (resolve-ties-kern
       (rest
        (append
         (subseq tied-datapoints 0 index)
         (subseq tied-datapoints (+ index 1))))
       (append
        dataset
        (list
         (append
          (subseq (first tied-datapoints) 0 3)
          (list
           (-
            (+ (first (nth index tied-datapoints))
               (fourth (nth index tied-datapoints)))
            (first (first tied-datapoints)))
           (fifth (first tied-datapoints)))))))
      (resolve-ties-kern
       (rest tied-datapoints) dataset))))

#|
\noindent Example:
\begin{verbatim}
(return-lists-of-length-n
 '((1 0) (0) (2 -1) nil (1 2 3) (7 -2)) 2)
--> ((1 0) (2 -1) (7 -2))
\end{verbatim}

\noindent Returns all lists in a list of lists that
are of length n. |#

(defun return-lists-of-length-n
       (a-list n)
  (if (null a-list) ()
    (if (equalp (length (first a-list)) n)
      (cons
       (first a-list)
       (return-lists-of-length-n (rest a-list) n))
      (return-lists-of-length-n (rest a-list) n))))

#|
\noindent Example:
\begin{\begin{Verbatim}[showtabs=true]}
(space-bar-positions
 "4C#\ 4G#\ 4B\	8e#/L	<")
--> (3 7)
\end{Verbatim}

\noindent This function returns the positions at which
space-bar symbols occur in a string. |#

(defun space-bar-positions
       (string &optional (start 0)
        (local-result
         (position #\ 	
                   (subseq string start)))
        (result
         (if local-result
           (+ start local-result))))
  (if (null result) ()
    (cons result
          (space-bar-positions
	   string (+ result 1)))))

#|
\noindent Example:
\begin{Verbatim}[showtabs=true]
(space-bar-separated-string2list
 "4C#\ 4G#\ 4B\	8e#/L	<")
--> ("4C#" "4G#" "4B	8e#/L	<")
\end{Verbatim}

\noindent This function turns a space-bar-separated
string into a list, where formerly each item was
preceded or proceeded by a space. |#

(defun space-bar-separated-string2list
       (space-separated-string &optional
	(spaces-positioned
	 (cons -1
               (space-bar-positions
                space-separated-string)))
	(result nil))
  (if (equal (length spaces-positioned) 1)
    (append
     result
     (list (subseq space-separated-string
		   (+ (first spaces-positioned) 1))))
    (space-bar-separated-string2list
     space-separated-string
     (rest spaces-positioned)
     (append
      result
      (list (subseq space-separated-string
		    (+ (first spaces-positioned) 1)
		    (second spaces-positioned)))))))

#|
\noindent Example:
\begin{verbatim}
(split-or-collapse-index 6 '(2 3 5 7 8))
--> 3
(split-or-collapse-index nil '(2 3 5 7 8))
--> nil
(split-or-collapse-index 8 '(2 3 5 7 8))
--> nil
\end{verbatim}

\noindent Returns the index of the second argument at
which the first argument is exceeded. Deals with
degenerate cases as indicated. |#

(defun split-or-collapse-index
       (crude-index staves-fibonacci &optional (i 0))
  (if (null crude-index)
    ()
    (if (null staves-fibonacci)
      ()
      (if (< crude-index (first staves-fibonacci))
        (identity i)
        (split-or-collapse-index
         crude-index (rest staves-fibonacci)
         (+ i 1))))))

#|
\noindent Example:
\begin{verbatim}
(staff-char-p #\2)
--> nil
\end{verbatim}

\noindent This function returns true if the input
character is `$\ast$', `s', `t', `a', or `f'. |#

(defun staff-char-p (a-char)
  (if (find
       a-char
       '(#\* #\s #\t #\a #\f)
       :test #'equalp)
    (identity t)))

#|
\noindent Example:
\begin{Verbatim}[showtabs=true]
(staves-info2staves-variable
 '("!!!COM: Chopin, Frederic"
   "!!!CDT: 1810///-1849///"
   "!!!OTL: Mazurka in F-sharp Minor, Op. 6, No. 1"
   "!!!OPS: Op. 6" "!!!ONM: No. 1"
   "!!!ODT: 1830///-1832///"
   "!!!PDT: 1832///-1833///"
   "!!!PPP: Leipzig (1832); Paris (1833) and London"
   "!!!ODE: Pauline Plater"
   "**kern	**kern	**dynam"
   "*thru	*thru	*thru"
   "*staff2	*staff1	*staff1/2"
   "*Ipiano	*Ipiano	*Ipiano"
   "*>A	*>A	*>A"))
--> ((1 1) (0 1) (-1/2 1))
\end{Verbatim}

\noindent This function looks through the first few
rows of a parsed kern file and determines how many
staves there are, leading to the definition of the
staves variable. |#

(defun staves-info2staves-variable
       (rows &optional (i 0))
  (if (null rows) ()
    (if (string=
         (subseq (first rows) 0 6) "*staff")
      (list
       (mapcar
        #'(lambda (x)
            (list
             (-
              (read-from-string
               (remove-if #'staff-char-p x)) 1)
             1))
        (tab-separated-string2list (first rows))) i)
      (staves-info2staves-variable
       (rest rows) (+ i 1)))))

#|
\noindent Example:
\begin{Verbatim}[showtabs=true]
(tab-positions "4C#\ 4G#\ 4B\	8e#/L	<")
--> (10 16)
\end{Verbatim}

\noindent This function returns the positions at which
tabs occur in a string. |#

(defun tab-positions
       (string &optional (start 0)
        (local-result
         (position #\	
                   (subseq string start)))
        (result
         (if local-result
           (+ start local-result))))
  (if (null result) ()
    (cons result
          (tab-positions
	   string (+ result 1)))))

#|
\noindent Example:
\begin{Verbatim}[showtabs=true]
(tab-separated-string2list
 "4C#\ 4G#\ 4B\	8e#/L	<")
--> ("4C#\ 4G#\ 4B\" "8e#/L" "<")
\end{Verbatim}

\noindent This function turns a tab-separated string
into a list, where formerly each item was preceded or
proceeded by a tab. |#

(defun tab-separated-string2list
       (tab-separated-string &optional
	(tabs-positioned
	 (cons -1
	       (tab-positions
     tab-separated-string)))
	(result nil))
  (if (equal (length tabs-positioned) 1)
    (append
     result
     (list (subseq tab-separated-string
		   (+ (first tabs-positioned) 1))))
    (tab-separated-string2list
     tab-separated-string
     (rest tabs-positioned)
     (append
      result
      (list (subseq tab-separated-string
		    (+ (first tabs-positioned) 1)
		    (second tabs-positioned)))))))

#|
\noindent Example:
\begin{verbatim}
(tied-kern-note-p "12f#/L]")
--> T
\end{verbatim}

\noindent This function returns true if the input kern
note is tied over, from or both. |#

(defun tied-kern-note-p (a-string)
  (if (or
       (find "[" a-string :test #'string=)
       (find "]" a-string :test #'string=))
    (identity T)))

#|
\noindent Example:
\begin{verbatim}
(upcase-p #\a)
--> nil
\end{verbatim}

\noindent This function returns true if the input
character is upper case, and nil otherwise. |#

(defun upcase-p (a-char)
       (equal (string-upcase a-char)
              (string a-char)))

#|
\noindent Example:
\begin{Verbatim}[showtabs=true]
(update-staves-variable
 '((1 1) (0 1) (1/2 1)) "*	*^	*")
--> ((1 1) (0 2) (1/2 1))
(update-staves-variable
 '((1 1) (0 2) (1/2 1)) "*	*v	*v	*")
--> ((1 1) (0 1) (1/2 1))
(update-staves-variable
 '((1 2) (0 1) (1/2 1)) "*	*	*^	*")
--> ((1 2) (0 2) (1/2 1))
\end{Verbatim}

\noindent The staves-variable is a list of pairs. The
first of each pair gives the staff to which a note
belongs. The second of each pair indicates whether
that stave is split into multiple voices. The symbol
`$\ast$' means leave this staff as it is, the symbol
`$^$' means this staff is splitting into an extra
voice, and the symbol `v' means this staff is
collapsing into one less voice. |#

(defun update-staves-variable
       (staves-variable row &optional
        (parsed-row
         (tab-separated-string2list row))
        (staves-fibonacci
         (fibonacci-list
           (mapcar
            #'(lambda (x)
                (second x)) staves-variable)))
        (split
         (split-or-collapse-index
          (index-item-1st-occurs "*^" parsed-row)
          staves-fibonacci))
        (collapse
         (split-or-collapse-index
          (index-item-1st-occurs "*v" parsed-row)
          staves-fibonacci)))
  (if split
    (append
     (subseq staves-variable 0 split)
     (list
      (list
       (first (nth split staves-variable))
       (+ (second (nth split staves-variable)) 1)))
     (subseq staves-variable (+ split 1)))
    (if collapse
      (append
       (subseq staves-variable 0 collapse)
       (list
        (list
         (first (nth collapse staves-variable))
         (-
          (second (nth collapse staves-variable)) 1)))
       (subseq staves-variable (+ collapse 1)))
      (identity staves-variable))))
