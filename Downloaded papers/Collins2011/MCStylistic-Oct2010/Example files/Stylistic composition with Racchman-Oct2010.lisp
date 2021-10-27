#| Copyright 2008-2011 Tom Collins
   Friday 5 August 2011

Stylistic composition with Racchman-Oct2010

This code demonstrates the use of a model for
automated stylistic composition called
Racchman-Oct2010, standing for RAndom Constrained
CHain of MArkovian Nodes. A date stamp is added in
case it is superseded by future work. Chapters 8 and 9
of Collins (2011) provide a full explanation of
Racchman-Oct2010. The model is similar in spirit to
the databases and programs referred to collectively as
Experiments in Musical Intelligence (EMI), as outlined
by Cope (1996, 2001, 2005).

Here I will exemplify the building of Racchman-Oct2010
for generating a passage of music in the style of a
mazurka by Frederic Chopin (1810-1849). The building
of the model takes about two hours on a 2.33 GHz
machine with 3 GB RAM, so for users not wishing to
wait that long, the resulting files have been placed
in the Example files folder, in a folder called
`Racchman-Oct2010 example'. The passage generation
takes about 260 seconds. More discussion of this code
can be found in the Documentation, Sec. 3.2.

References

Tom Collins. Improved methods for pattern discovery in
music, with applications in automated stylistic
composition. PhD thesis, Faculty of Mathematics,
Computing and Technology, The Open University, 2011.

David Cope. Experiments in musical intelligence. The
Computer Music and Digital Audio Series. A-R Editions,
Madison, WI, 1996.

David Cope. Virtual music: computer synthesis of
musical style. MIT Press, Cambridge, MA, 2001.
(Includes essays by Douglas Hofstadter, Eleanor
Selfridge-Field, Bernard Greenberg, Steve Larson,
Jonathan Berger, and Daniel Dennett).
 
David Cope. Computer models of musical creativity. MIT
Press, Cambridge, MA, 2005. |#


#| Step 1 - Import the dataset representations of
39 Chopin mazurkas. |#
(progn
  (setq
   *variable-names*
   '(C-6-1 C-6-3 C-6-4 C-7-1 C-7-2 C-7-3 C-7-5 C-17-1
     C-17-2 C-17-3 C-17-4 C-24-2 C-24-3 C-30-1 C-30-2
     C-30-3 C-30-4 C-33-1 C-33-2 C-33-3 C-41-1 C-41-2
     C-41-3 C-50-1 C-50-2 C-50-3 C-56-1 C-56-3 C-59-1
     C-59-2 C-63-1 C-63-2 C-67-2 C-67-3 C-67-4 C-68-1
     C-68-2 C-68-3 C-68-4))
  (setq
   *catalogue*
   '("C-6-1" "C-6-3" "C-6-4" "C-7-1" "C-7-2" "C-7-3"
     "C-7-5" "C-17-1" "C-17-2" "C-17-3" "C-17-4"
     "C-24-2" "C-24-3" "C-30-1" "C-30-2" "C-30-3"
     "C-30-4" "C-33-1" "C-33-2" "C-33-3" "C-41-1"
     "C-41-2" "C-41-3" "C-50-1" "C-50-2" "C-50-3"
     "C-56-1" "C-56-3" "C-59-1" "C-59-2" "C-63-1"
     "C-63-2" "C-67-2" "C-67-3" "C-67-4" "C-68-1"
     "C-68-2" "C-68-3" "C-68-4"))
  (setq
   C-6-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-6-1-ed.txt")))
  (setq
   C-6-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-6-3-ed.txt")))
  (setq
   C-6-4
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-6-4-ed.txt")))
  (setq
   C-7-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-7-1-ed.txt")))
  (setq
   C-7-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-7-2-ed.txt")))
  (setq
   C-7-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-7-3-ed.txt")))
  (setq
   C-7-5
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-7-5-ed.txt")))
  (setq
   C-17-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-17-1-ed.txt")))
  (setq
   C-17-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-17-2-ed.txt")))
  (setq
   C-17-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-17-3-ed.txt")))
  (setq
   C-17-4
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-17-4-ed.txt")))
  (setq
   C-24-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-24-2-ed.txt")))
  (setq
   C-24-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-24-3-ed.txt")))
  (setq
   C-30-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-30-1-ed.txt")))
  (setq
   C-30-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-30-2-ed.txt")))
  (setq
   C-30-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-30-3-ed.txt")))
  (setq
   C-30-4
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-30-4-ed.txt")))
  (setq
   C-33-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-33-1-ed.txt")))
  (setq
   C-33-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-33-2-ed.txt")))
  (setq
   C-33-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-33-3-ed.txt")))
  (setq
   C-41-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-41-1-ed.txt")))
  (setq
   C-41-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-41-2-ed.txt")))
  (setq
   C-41-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-41-3-ed.txt")))
  (setq
   C-50-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-50-1-ed.txt")))
  (setq
   C-50-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-50-2-ed.txt")))
  (setq
   C-50-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-50-3-ed.txt")))
  (setq
   C-56-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-56-1-ed.txt")))
  (setq
   C-56-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-56-3-ed.txt")))
  (setq
   C-59-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-59-1-ed.txt")))
  (setq
   C-59-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-59-2-ed.txt")))
  (setq
   C-63-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-63-1-ed.txt")))
  (setq
   C-63-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-63-2-ed.txt")))
  (setq
   C-67-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-67-2-ed.txt")))
  (setq
   C-67-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-67-3-ed.txt")))
  (setq
   C-67-4
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-67-4-ed.txt")))
  (setq
   C-68-1
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-68-1-ed.txt")))
  (setq
   C-68-2
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-68-2-ed.txt")))
  (setq
   C-68-3
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-68-3-ed.txt")))
  (setq
   C-68-4
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-68-4-ed.txt")))
  "Datasets imported.")

#| Step 2 - Create lists of initial/final state-
context pairs, and transition lists. |#
(progn
  (setq
   *initial-states*
   (construct-initial-states
    *variable-names* *catalogue* "beat-spacing-states"
    10 3 3 1))
  (write-to-file
   *initial-states*
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/Racchman-Oct2010 example/initial-states.txt"))
  (setq
   *final-states*
   (construct-final-states
    *variable-names* *catalogue* "beat-spacing-states"
    10 3 3 1))
  (write-to-file
   *final-states*
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/Racchman-Oct2010 example/final-states.txt"))
  "Initial/final state-context pairs exported.")
  
(progn
  (setq *transition-matrix* nil)
  (construct-stm
   *variable-names* *catalogue* "beat-spacing-states"
   3 3 1)
  (write-to-file
   *transition-matrix*
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/Racchman-Oct2010 example"
    "/transition-matrix.txt"))
  (setq *transition-matrix* nil)
  (construct-stm<-
   *variable-names* *catalogue* "beat-spacing-states"
   3 3 1)
  (write-to-file
   *transition-matrix*
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/Racchman-Oct2010 example"
    "/transition-matrix<-.txt"))
  "Transition lists exported.")

#| Step 3 - Define parameter values for
Racchman-Oct2010. |#
(progn
  (setq *beats-in-bar* 3) (setq *c-absrb* 10)
  (setq *c-src* 4) (setq *c-bar* 19) (setq *c-min* 19)
  (setq *c-max* 19) (setq *c-beat* 12)
  (setq *c-prob* 0.2) (setq *c-for* 3)
  (setq *c-back* 3)
  (setq
   *checklist*
   (list "originalp" "mean&rangep" "likelihoodp"))
  "Racchman-Oct2010 parameters defined.")

#| Step 4 - Import lists of initial/final state-
context pairs, and transition lists. It should be
noted that some variables, such as
*internal-initial-states*, are not required for this
example, so their import code is commented out. |#
(progn
  (setq
   *initial-states*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example/initial-states.txt")))
  #|
  (setq
   *internal-initial-states*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/internal-initial-states.txt")))
  |#
  (setq
   *internal-final-states*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/internal-final-states.txt")))
  #|
  (setq
   *final-states*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example/final-states.txt")))
  |#
  (setq
   *stm->*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/transition-matrix.txt")))
  (setq
   *stm<-*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/Racchman-Oct2010 example"
     "/transition-matrix<-.txt")))
  (concatenate
   'string
   "Initial state/context pairs and transition lists"
   " imported."))

#| Step 5 - Import the dataset of an existing Chopin
mazurka to be used as a template (op.56 no.2). A
template (cf. Def. 9.1 in Collins, 2011) consists of
basic information, such as tempo and the pitch of the
lowest-sounding note of the first chord, which is
transferred to the generated passage. |#
(progn
  (setq
   *dataset-all*
   (read-from-file
    (concatenate
     'string
     *MCStylistic-Oct2010-data-path*
     "/Dataset/C-56-2-ed.txt")))
  (setq
   *dataset-template*
   (subseq *dataset-all* 0 135))
  "Template imported.")

#| Step 6 - Generate candidate passages using
Racchman-Oct2010 and select one. |#
(progn
  (setq
   *rs* #.(CCL::INITIALIZE-RANDOM-STATE 46803 5306))
  (setq time-a (get-internal-real-time))
  (setq
   *output*
   (generate-beat-MNN-spacing<->
    *initial-states* *stm->* *internal-final-states*
    *stm<-* *dataset-template* *checklist*
    *beats-in-bar* *c-absrb* *c-src* *c-min* *c-max*
    *c-bar* *c-beat* *c-prob* *c-for* *c-back*))
  (setq time-b (get-internal-real-time))
  (float
   (/
    (- time-b time-a)
    internal-time-units-per-second)))
; 266.0195 seconds.
(most-plausible-join
 (third *output*) 23 *dataset-template* *stm->* 3 3 1
 *c-beat*)
; "united,3,3,backwards-dominant"

#| Step 7 - Export the generated passage to MIDI and
text files. |#
(progn
  (setq
   *output-datapoints*
   (gethash
    '"united,3,3,backwards-dominant"
    (third *output*)))
  (saveit
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/Racchman-Oct2010 example/generated-passage.mid")
   (modify-to-check-dataset
    (translation
     *output-datapoints*
     (list
      (- 0 (first (first *output-datapoints*)))
      0 0 0 0)) 950))
  (write-to-file
   *output-datapoints*
   (concatenate
    'string
    *MCStylistic-Oct2010-example-files-path*
    "/Racchman-Oct2010 example"
    "/generated-passage.txt"))
  (concatenate
   'string
   "Generated passage exported to MIDI and text"
   " files."))
