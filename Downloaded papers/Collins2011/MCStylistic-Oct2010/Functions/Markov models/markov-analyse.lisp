#| Copyright 2008-2011 Tom Collins
   Tuesday 27 January 2009
   Completed Tuesday 24 August 2010

The functions here are designed to analyse data
according to a Markov-chain model. At present the code
handles a first-order analysis. The aim is to build a
state transition matrix for some variables (referenced
by variable-names and catalogue). Hence, the variable
variable-names points to some actual data (note the
use of the function symbol-value) which is indexed by
the variable catalogue. Using the function
write-to-file, the information can be sent to a text
file, to avoid having to display it. |#

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
  "/Markov models/spacing-states.lisp"))

; (defvar *transition-matrix* ())

#|
\noindent Example:
\begin{verbatim}
(accumulate-to-stm
 '(((4) ("Piece A")) ((2) ("Piece A")))
 '((4) (((1) ("Piece B")) ((2) ("Piece C"))))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B"))
         ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((4) (((2) ("Piece A")) ((1) ("Piece B"))
	    ((2) ("Piece C"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((2) (((5) ("Piece A"))))
      ((5) (((4) ("Piece B")))))
\end{verbatim}

\noindent The first argument is a listed state; the
second is the relevant row of the state transition
matrix; the third is the state transition matrix
itself. This function is called when the state of the
first item of the listed state has appeared in the
state transition matrix  before. The references of the
event are included. |#

(defun accumulate-to-stm
       (listed-state relevant-row stm)
  (cons (list (first (first listed-state))
              (cons (second listed-state)
		    (second relevant-row)))
	(remove relevant-row stm :test #'equalp)))

#|
\noindent Example:
\begin{verbatim}
(add-to-stm
 '(((3) ("Piece A")) ((4) ("Piece A")))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((3) (((4) ("Piece A"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((2) (((5) ("Piece A"))))
      ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
      ((5) (((4) ("Piece B")))))
\end{verbatim}

\noindent The first argument is a listed state; the
second is the state transition matrix. This function
is called when the state of the first item of the
listed state has not appeared in the state transition
matrix before. It is added. |#

(defun add-to-stm (listed-state stm)
  (cons
   (list
    (first (first listed-state))
    (list (second listed-state))) stm))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   variable-1
   '((0 30 1 1 84) (0 33 1 1 84) (1 40 1 1 84)
     (1 41 1 1 84)))
  (setq
   variable-2
   '((0 60 1 1 84) (0 63 1 1 84) (1 62 1 1 84)
     (1 63 1 1 84)))
  (setq *variable-names* '(variable-1 variable-2))
  (setq *catalogue* '("variable-1" "variable-2"))
  (construct-initial-states
   *variable-names* *catalogue*))
--> '((((3) (0 0))
       (NIL 1 "variable-1"
	    ((0 30 1 1 84 1 0) (0 33 1 1 84 1 1))))
      (((3) (0 0))
       (NIL 1 "variable-2"
	    ((0 60 1 1 84 1 0) (0 63 1 1 84 1 1)))))
\end{verbatim}

\noindent This recursion analyses one variable name at
a time, taking a catalogue name from the variable
catalogue, and outputs initial states accordingly. |#

(defun construct-initial-states
       (variable-names catalogue state-fn &optional
	(depth-check 10) (duration-index 3)
	(beats-in-bar 4) (sort-index 2))
  (if (null variable-names) ()
    (cons
     (first
      (if (string= state-fn "spacing-holding-states")
	(spacing-holding-states
	 (firstn
	  depth-check
	  (symbol-value (first variable-names)))
	 (first catalogue) duration-index)
	(beat-spacing-states
	 (firstn
	  depth-check
	  (symbol-value (first variable-names)))
	 (first catalogue) beats-in-bar sort-index
	 duration-index)))
     (construct-initial-states
      (rest variable-names) (rest catalogue) state-fn
      depth-check duration-index beats-in-bar
      sort-index))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   variable-1
   '((0 30 1 1 84) (0 33 1 1 84) (1 40 1 1 84)
     (1 41 1 1 84)))
  (setq
   variable-2
   '((0 60 1 1 84) (0 63 1 1 84) (1 62 1 1 84)
     (1 63 1 1 84)))
  (setq *variable-names* '(variable-1 variable-2))
  (setq *catalogue* '("variable-1" "variable-2"))
  (construct-stm
   *variable-names* *catalogue* "beat-spacing-states"
   2 4 1)
--> "Finished"
*transition-matrix*
--> (((1 (3))
      (((2 (1))
        (2 0 "variable-2"
         ((1 62 1 1 84 2 2) (1 63 1 1 84 2 3))))
       ((2 (1))
        (10 0 "variable-1"
         ((1 40 1 1 84 2 2) (1 41 1 1 84 2 3)))))))
\end{verbatim}

\noindent This recursion analyses one variable name at
a time, taking a catalogue name from the variable
catalogue, and updates the transition matrix
accordingly. The output "Finished" is preferable to
the transition matrix, which is large enough that it
can cause the Listener to crash. |#

(defun construct-stm
       (variable-names catalogue state-fn &optional
	(duration-index 3) (beats-in-bar 4)
	(sort-index 2))
  (if (null variable-names) (print "Finished")
    (progn
      (markov-analyse
       (if (string= state-fn "spacing-holding-states")
	(spacing-holding-states
	  (symbol-value (first variable-names))
	  (first catalogue) duration-index)
	(beat-spacing-states
	 (symbol-value (first variable-names))
	 (first catalogue) beats-in-bar sort-index
	 duration-index)))
      (construct-stm
       (rest variable-names) (rest catalogue) state-fn
       duration-index beats-in-bar sort-index
       ))))

#|
\noindent Example:
\begin{verbatim}
(firstn-list 3 '(1 2 3 4 5)
--> '((1 2 3) (2 3 4) (3 4 5))
\end{verbatim}

\noindent This function applies the function firstn
recursively to a list. It is like producing an n-
gram, and is useful for building a first-order
Markov model. I call the output `listed states'. |#

(defun firstn-list (n a-list)
  (if (equal (length a-list) (- n 1)) ()
    (cons (firstn n a-list)
          (firstn-list n (rest a-list)))))

#|
\noindent Example:
\begin{verbatim}
(markov-analyse
 '(((3) ("Piece A")) ((6) ("Piece A"))
   ((4) ("Piece A")) ((4) ("Piece A"))
   ((3) ("Piece A")) ((2) ("Piece A"))))
--> "Finished"
*transition-matrix*
--> '(((3) (((2) ("Piece A")) ((6) ("Piece A"))))
      ((4) (((3) ("Piece A")) ((4) ("Piece A"))))
      ((6) (((4) ("Piece A")))))
\end{verbatim}

\noindent This function has one argument - some states
which are to be analysed according to a first-order
Markov model. Note the need to define a variable here,
\texttt{*transition-matrix*}. The output "Finished" is
preferable to the transition matrix, which is large
enough that it can cause the Listener to crash. |#

(defun markov-analyse (states)
  (if (update-stm (firstn-list 2 states)) t)
  "Finished")

#|
\noindent Example:
\begin{verbatim}
(present-to-stm
 '(((4) ("Piece A")) ((2) ("Piece A")))
 '(((1) (((5) ("Piece A")) ((4) ("Piece B"))))
   ((2) (((5) ("Piece A"))))
   ((4) (((1) ("Piece B")) ((2) ("Piece C"))))
   ((5) (((4) ("Piece B"))))))
--> '(((4) (((2) ("Piece A")) ((1) ("Piece B"))
	    ((2) ("Piece C"))))
      ((1) (((5) ("Piece A")) ((4) ("Piece B"))))
      ((2) (((5) ("Piece A"))))
      ((5) (((4) ("Piece B"))))).
\end{verbatim}

\noindent This function calls either the function
accumulate-to-stm, or add-to-stm, depending on whether
the first argument, a listed-state, has appeared in
the second argument, a state-transition matrix,
before. The example above results in accumulate-to-stm
being called, as the state of (4) has occurred before.
However, changing this state to (3) in the argument
would result in add-to-stm being called. |#

(defun present-to-stm (listed-state stm)
  (let ((relevant-row
	 (assoc
	  (first (first listed-state)) stm
	  :test #'equalp)))
    (if (identity relevant-row) 
      (accumulate-to-stm
       listed-state relevant-row stm)
      (add-to-stm listed-state stm))))

#|
\noindent Example:
\begin{verbatim}
(update-stm
 '((((3) ("Piece A")) ((6) ("Piece A")))
   (((6) ("Piece A")) ((4) ("Piece A")))
   (((4) ("Piece A")) ((4) ("Piece A")))
   (((4) ("Piece A")) ((3) ("Piece A")))
   (((3) ("Piece A")) ((2) ("Piece A")))))
--> '(((3) (((2) ("Piece A")) ((6) ("Piece A"))))
      ((4) (((3) ("Piece A")) ((4) ("Piece A"))))
      ((6) (((4) ("Piece A")))))
\end{verbatim}

\noindent This function has as its argument listed
states, and it applies the function present-to-stm
recursively to these listed states. The variable
\texttt{*transition-matrix*} is updated as it
proceeds. |#

(defun update-stm
       (listed-states &optional
	(stm *transition-matrix*))
  (if (null listed-states)
    (setq *transition-matrix* stm)
    (update-stm (rest listed-states)
                (present-to-stm
                 (first listed-states) stm))))
