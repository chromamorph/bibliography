#| Copyright 2008-2011 Tom Collins
   Monday 25 October 2010

The main function here is
generate-beat-spacing<->pattern-inheritance, at the
heart of the model named Racchmaninof-Oct2010
(standing for RAndom Constrained CHain of Markovian
Nodes with INheritance Of Form). |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Markov models"
  "/generating-beat-MNN-spacing-for&back.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Markov models"
  "/generating-with-patterns-preliminaries.lisp"))
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/list-processing.lisp"))

; (defvar *rs* (make-random-state t))

#|
\noindent Example: see Stylistic composition with
Racchmaninof-Oct2010
(Sec.~\ref{sec:ex:Racchmaninof-Oct2010}, especially
lines 575-582).
\vspace{0.5cm}

\noindent This function is at the heart of the model
named Racchmaninof-Oct2010 (standing for RAndom
Constrained CHain of Markovian Nodes with INheritance
Of Form). It takes nine mandatory arguments and
twenty-two optional arguments. The mandatory arguments
are initial-state and state-transition lists, and also
information pertaining to a so-called \emph{template
with patterns}. The optional arguments are mainly for
controlling various criteria like: not too many
consecutive states from the same source, the range
must be comparable with that of the template, and the
likelihood of the states must be comparable with that
of the template. |#

(defun generate-beat-spacing<->pattern-inheritance
       (external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template patterns-hash
        whole-piece-interval &optional
        (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10)
        (c-sources 3) (c-bar 12) (c-min 7) (c-max 7)
        (c-beats 12) (c-prob 0.15) (c-forwards 3)
        (c-backwards 3) (sort-index 1)
        (duration-index 3)
        (interval-output-pairs nil)
        (existing-intervals
         (nth-list-of-lists 0 interval-output-pairs))
        (indexed-max-subset-score
         (indices-of-max-subset-score patterns-hash))
        (pattern-region
         (if indexed-max-subset-score
           (translation
            (gethash
             '"region"
             (nth
              (first indexed-max-subset-score)
              patterns-hash))
            (nth
             (second indexed-max-subset-score)
             (gethash
              '"translators"
              (nth
               (first indexed-max-subset-score)
               patterns-hash))))))
        (floor-ontime
         (floor
          (first
           (first
            (if pattern-region
              pattern-region dataset-template)))))
        (ceiling-ontime
         (ceiling
          (first
           (my-last
            (if pattern-region
              pattern-region dataset-template)))))
        (generation-intervals
         (generate-intervals
          floor-ontime ceiling-ontime
          existing-intervals))
        (new-interval-output-pairs
         (if generation-intervals
           (generate-beat-spacing-for-intervals
            generation-intervals whole-piece-interval
            interval-output-pairs
            external-initial-states
            internal-initial-states stm->
            external-final-states
            internal-final-states stm<-
            dataset-template pattern-region checklist
            beats-in-bar c-failures c-sources c-bar
            c-min c-max c-beats c-prob c-forwards
            c-backwards sort-index duration-index)))
        (new-translated-interval-output-pairs
         (if (and
              indexed-max-subset-score
              new-interval-output-pairs)
           (translate-to-other-occurrences
            new-interval-output-pairs
            interval-output-pairs
            indexed-max-subset-score patterns-hash
            existing-intervals))))
  (if (null indexed-max-subset-score)
    (merge-sort-by-vector<vector-car
     (append
      interval-output-pairs
      new-interval-output-pairs))
    (generate-beat-spacing<->pattern-inheritance
     external-initial-states internal-initial-states
     stm-> external-final-states internal-final-states
     stm<- dataset-template
     (progn
       (setf
        (gethash
         '"inheritance addressed"
         (nth
          (first indexed-max-subset-score)
          patterns-hash)) "Yes")
       patterns-hash)
     whole-piece-interval checklist beats-in-bar
     c-failures c-sources c-bar c-min c-max c-beats
     c-prob c-forwards c-backwards sort-index
     duration-index
     (if new-interval-output-pairs
       (merge-sort-by-vector<vector-car
        (append
         interval-output-pairs
         new-interval-output-pairs
         new-translated-interval-output-pairs))
       interval-output-pairs))))

#|
\noindent Example:
\begin{verbatim}
(progn
  (setq
   external-initial-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/initial-states.txt")))
  (setq
   internal-initial-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/internal-initial-states.txt")))
  (setq
   stm->
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/transition-matrix.txt")))
  (setq
   external-final-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/final-states.txt")))
  (setq
   internal-final-states
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/internal-final-states.txt")))
  (setq
   stm<-
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchmaninof-Oct2010 example"
     "/transition-matrix<-.txt")))
  (setq
   dataset-all
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-56-1-ed.txt")))
  (setq
   dataset-template
   (subseq dataset-all 0 132))
  "Data imported.")
(progn
  (setq generation-interval '(12 24))
  (setq
   whole-piece-interval
   (list
    (floor (first (first dataset-all)))
    (ceiling (first (my-last dataset-all)))))
  (setq A (make-hash-table :test #'equal))
  (setf
   (gethash
    '"united-candidates,1,1,superimpose" A)
   '((9 70 66 3 0) (9 79 71 1/2 0) (19/2 74 68 3/2 0)
     (10 55 57 1 1) (10 62 61 1 1) (11 55 57 1 1)
     (11 62 61 1 1) (12 38 47 1 1) (12 69 65 1/2 0)))
  (setq B (make-hash-table :test #'equal))
  (setf
   (gethash
    '"united-candidates,2,3,forwards-dominant" B)
   '((24 38 47 1 1) (49/2 66 63 1/2 0) (25 50 54 1 1)
     (25 57 58 1 1) (25 60 60 1 1) (25 62 61 1 0)
     (26 50 54 1 1) (26 57 58 1 1) (26 60 60 1 1)
     (26 62 61 1 1) (26 66 63 1 0) (26 70 66 3/4 0)
     (107/4 69 65 1/4 0) (27 43 50 1 1)
     (27 67 64 2 0)))
  (setq
   interval-output-pairs
   (list
    (list
     (list 9 12)
     (list
      "united-candidates,1,1,superimpose"
      (list nil nil A)))
    (list
     (list 24 27)
     (list
      "united-candidates,2,3,forwards-dominant"
      (list nil nil B)))))
  (setq
   pattern-region
   '((12 60 60) (12 64 62) (25/2 57 58) (51/4 64 62)
     (13 52 55) (13 64 62) (14 54 56) (29/2 62 61)
     (15 55 57) (15 59 59) (63/4 64 62) (16 43 50)
     (16 50 54) (16 66 63) (17 67 64) (18 57 58)
     (18 60 60) (18 64 62) (75/4 66 63) (19 43 50)
     (19 50 54) (19 67 64) (20 66 63) (20 69 65)
     (21 59 59) (21 67 64) (21 71 66) (87/4 74 68)
     (22 43 50) (22 50 54) (22 74 68) (23 62 61)
     (24 60 60) (24 64 62)))
  "Argument instances defined.")
(progn
  (setq
   checklist
   (list "originalp" "mean&rangep" "likelihoodp"))
  (setq beats-in-bar 3) (setq c-failures 10)
  (setq c-sources 4) (setq c-bar 48) (setq c-min 38)
  (setq c-max 38) (setq c-beats 38) (setq c-prob 1)
  (setq c-forwards 3) (setq c-backwards 3)
  (setq
   *rs* #.(CCL::INITIALIZE-RANDOM-STATE 56302 14832))
  "Parameters set.")
(progn
  (setq
   interval-output-pair
   (generate-beat-spacing-for-interval
    generation-interval whole-piece-interval
    interval-output-pairs external-initial-states
    internal-initial-states stm->
    external-final-states internal-final-states stm<-
    dataset-template pattern-region checklist
    beats-in-bar c-failures c-sources c-bar c-min
    c-max c-beats c-prob c-forwards c-backwards))
--> ((12 24)
     ("united,2,1,backwards-dominant"
      (#<HASH-TABLE
       :TEST EQUAL size 3/60 #x30004340CE3D>
       #<HASH-TABLE
       :TEST EQUAL size 3/60 #x30004340C82D>
       #<HASH-TABLE
       :TEST EQUAL size 27/60 #x30004340C21D>)))
\end{verbatim}

\noindent This function generates material for a
specified time interval, by calling the function
generate-beat-spacing-forced<->. |#

(defun generate-beat-spacing-for-interval
       (generation-interval whole-piece-interval
        interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1) (c-forwards 3) (c-backwards 3)
        (sort-index 1) (duration-index 3)
        (left-side (first generation-interval))
        (right-side (second generation-interval))
        (trimmed-template
         (remove-datapoints-with-nth-item>
          (remove-datapoints-with-nth-item<
           dataset-template left-side 0)
          right-side 0))
        #| A workaround for when there is no
        pattern-region (so original generate
        functions are called), so first and last
        ontimes have to be floored and ceilinged
        respectively. |#
        (rounded-template
         (if (null pattern-region)
           (append
            (list
             (append
              (list
               (floor
                (first (first trimmed-template))))
              (rest (first trimmed-template))))
            (rest (butlast trimmed-template))
            (list
             (append
              (list
               (ceiling
                (first (my-last trimmed-template))))
              (rest (my-last trimmed-template)))))
           trimmed-template))
        (generated-intervals
         (nth-list-of-lists 0 interval-output-pairs))
        (generated-left-sides
         (nth-list-of-lists 0 generated-intervals))
        (generated-right-sides
         (nth-list-of-lists 1 generated-intervals))
        (terminal->p
         (equalp
          left-side (first whole-piece-interval)))
        (terminal<-p
         (equalp
          right-side (second whole-piece-interval)))
        (index-context->
         (position
          left-side generated-right-sides
          :test #'equalp))
        (index-context<-
         (position
          right-side generated-left-sides
          :test #'equalp))
        (context-most-plausible->
         (if index-context->
           (first
            (second
             (nth
              index-context->
              interval-output-pairs)))))
        (context-most-plausible<-
         (if index-context<-
           (first
            (second
             (nth
              index-context<-
              interval-output-pairs)))))
        (context-datapoints->
         (if context-most-plausible->
           (gethash
            context-most-plausible->
            (third
             (second
              (second
               (nth
                index-context->
                interval-output-pairs)))))))
        (context-datapoints<-
         (if context-most-plausible<-
           (gethash
            context-most-plausible<-
            (third
             (second
              (second
               (nth
                index-context<-
                interval-output-pairs)))))))
        (state-context-pair->
         (if context-datapoints->
           (list
            (first
             (my-last
              (beat-spacing-states
               context-datapoints-> "no information"
               beats-in-bar sort-index
               duration-index)))
            (list
             nil nil "no information"
             (fourth
            (second
             (my-last
              (beat-spacing-states
               context-datapoints-> "no information"
               beats-in-bar sort-index
               duration-index))))))))
        (state-context-pair<-
         (if context-datapoints<-
           (list
            (first
             (first
              (beat-spacing-states<-
               context-datapoints<- "no information"
               beats-in-bar sort-index
               duration-index)))
            (list
             nil nil "no information"
             (fourth
              (second
               (first
                (beat-spacing-states<-
                 context-datapoints<- "no information"
                 beats-in-bar sort-index
                 duration-index))))))))
        (output
         (generate-beat-spacing-forced<->
          generation-interval terminal->p terminal<-p
          external-initial-states
          internal-initial-states stm->
          external-final-states internal-final-states
          stm<- rounded-template pattern-region
          state-context-pair-> state-context-pair<-
          checklist beats-in-bar c-failures c-sources
          c-bar c-min c-max c-beats c-prob c-forwards
          c-backwards sort-index duration-index)))
  (list
   generation-interval
   (list
    (most-plausible-join
     (third output) (/ (+ left-side right-side) 2)
     rounded-template stm-> duration-index
     beats-in-bar sort-index c-beats)
    output)))

#|
\noindent Example: see example for
\nameref{fun:generate-beat-spacing-for-interval}.
\vspace{0.5cm}

\noindent This function applies the function
generate-beat-spacing-for-interval to each member of
a list called \texttt{interval-output-pairs}. |#

(defun generate-beat-spacing-for-intervals
       (generation-intervals whole-piece-interval
        interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        &optional (checklist (list "originalp"))
        (beats-in-bar 4) (c-failures 10) (c-sources 3)
        (c-bar 12) (c-min 7) (c-max 7) (c-beats 12)
        (c-prob 0.1) (c-forwards 3) (c-backwards 3)
        (sort-index 1) (duration-index 3))
  (mapcar
   #'(lambda (x)
       (generate-beat-spacing-for-interval
        x whole-piece-interval interval-output-pairs
        external-initial-states
        internal-initial-states stm->
        external-final-states internal-final-states
        stm<- dataset-template pattern-region
        checklist beats-in-bar c-failures c-sources
        c-bar c-min c-max c-beats c-prob c-forwards
        c-backwards sort-index duration-index))
   generation-intervals))

#|
\noindent Example: see example for
\nameref{fun:unite-datapoints}.
\vspace{0.5cm}

\noindent This function applies the function
unite-datapoints, to convert the output for various
intervals into a dataset. |#

(defun interval-output-pairs2dataset
       (interval-output-pairs &optional
        (first-pair (first interval-output-pairs))
        (datapoints
         (if first-pair
           (gethash
            (first (second first-pair))
            (third (second (second first-pair)))))))
  (if (null first-pair) ()
    (unite-datapoints
     datapoints
     (interval-output-pairs2dataset
      (rest interval-output-pairs))
     (second (first first-pair))
     "backwards-dominant")))

#|
\noindent Example:
\begin{verbatim}
(translate-to-other-occurrence
 nil '(36 12 7) '((12 24))
 (make-hash-table :test #'equal)
 (translation
  '((12 38 47 1/2 1) (49/4 64 62 1/4 0)
    (25/2 50 54 1 1) (25/2 55 57 1 1) (25/2 58 59 1 1)
    (25/2 62 61 1 0)) '(36 12 7 0 0)))
--> (((48 60)
      ("translated material"
       (NIL NIL
        #<HASH-TABLE :TEST EQUAL
        size 1/60 #x3000436FF23D>))))
\end{verbatim}

\noindent This function takes a list of interval-
output pairs as its argument (from one iteration of
generate-beat-spacing<->pattern-inheritance) Its
second argument is a translation vector, by which
each of the output datasets must be translated. |#

(defun translate-to-other-occurrence
       (new-interval-output-pairs translation-vector
        &optional
        (interval-output-pair
         (first new-interval-output-pairs))
        (A (make-hash-table :test #'equal))
        (translated-datapoints
         (if interval-output-pair
           (translation
            (gethash
             (first (second interval-output-pair))
             (third
              (second (second interval-output-pair))))
            (append translation-vector (list 0 0))))))
  (if (null interval-output-pair) ()
    (cons
     (list
      (list
       (+
        (first (first interval-output-pair))
        (first translation-vector))
       (+
        (second (first interval-output-pair))
        (first translation-vector)))
      (list
       "translated material"
       (list
        nil
        nil
        (progn
          (setf
           (gethash '"translated material" A)
           translated-datapoints)
          A))))
     (translate-to-other-occurrence
      (rest new-interval-output-pairs)
      translation-vector))))

#|
\noindent Example: see example for
\nameref{fun:translate-to-other-occurrence}.
\vspace{0.5cm}

\noindent This function applies the function
translate-to-other-occurrence to each member of
a list called \texttt{translators}. It first
determines whether the location to which material will
be translated has been addressed. |#

(defun translate-to-other-occurrences
       (new-interval-output-pairs
        existing-interval-output-pairs
        indices-of-max-subset-score patterns-hash
        &optional
        (existing-intervals
         (nth-list-of-lists
          0 existing-interval-output-pairs))
        (translators
         (gethash
          '"translators"
          (nth
           (first indices-of-max-subset-score)
           patterns-hash)))
        (altered-translators
         (remove-nth
          (second indices-of-max-subset-score)
          (subtract-list-from-each-list
           translators
           (nth
            (second indices-of-max-subset-score)
            translators))))
        (n (length altered-translators)) (i 0)
        (translated-intervals
         (if (< i n)
           (mapcar
            #'(lambda (x)
                (list
                 (+
                  (first x)
                  (first (nth i altered-translators)))
                 (+
                  (second x)
                  (first
                   (nth i altered-translators)))))
            (nth-list-of-lists
             0 new-interval-output-pairs))))
        (generation-possiblep
         (if (< i n)
           (equalp
            (generate-intervals
             (first (first translated-intervals))
             (my-last (my-last translated-intervals))
             existing-intervals)
            translated-intervals))))
  (if (>= i n) ()
    (append
     (if generation-possiblep
       (translate-to-other-occurrence
        new-interval-output-pairs
        (nth i altered-translators)))
     (translate-to-other-occurrences
      new-interval-output-pairs
      existing-interval-output-pairs
      indices-of-max-subset-score patterns-hash
      existing-intervals translators
      altered-translators n (+ i 1)))))
