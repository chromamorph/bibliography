#|
(in-package :common-lisp-user)
|#

(progn
  (setq *print-length* nil)
  (setq *print-pretty* nil)
  (defvar
      *MCStylistic-Oct2010-path*
    (concatenate
     'string
     "/Applications/CCL/MCStylistic"
     "/MCStylistic-Oct2010"))
  (defvar
      *MCStylistic-Oct2010-data-path*
    (concatenate
     'string
     *MCStylistic-Oct2010-path* "/Data"))
  (defvar
      *MCStylistic-Oct2010-example-files-path*
    (concatenate
     'string
     *MCStylistic-Oct2010-path* "/Example files"))
  (defvar
      *MCStylistic-Oct2010-functions-path*
    (concatenate
     'string
     *MCStylistic-Oct2010-path* "/Functions"))
  (defvar *pitch-mod* 12)
  (defvar *rs* (make-random-state t))
  (defvar *transition-matrix* ())
  (defvar *chunk-type* ())
  (defvar *chunk-length* 0)
  (defvar *midi-file-format* 0)
  (defvar *midi-file-ntrks* 0)
  (defvar *midi-file-granularity* 24)
  (defvar *track-time* 0)
  (defvar *running-status* 0)
  (defvar *track-end* t)
  (defvar
      *sequence-strings*
    (make-array
     1
     :initial-contents #("sequence-strings")
     :fill-pointer t :adjustable t))
  (defvar
      *sequence-tempo-map*
    (make-array
     1
     :element-type 'list :initial-element '(0 0 1)
     :fill-pointer t :adjustable t))
  (defvar
      *sequence-notes*
    (make-array
     0
     :element-type 'list :initial-element '(0 0 0 0 0)
     :fill-pointer t :adjustable t))
  (defconstant
      kMThdHeaderBytes '(#x4D #x54 #x68 #x64))
  (defconstant kMThdHeaderLenBytes '(0 0 0 6))
  (defconstant kMThdHeaderFormatBytes '(0 1))
  (defconstant
      kMTrkHeaderBytes '(#x4D #x54 #x72 #x6B))
  (defconstant kMTrkEndBytes '(#x00 #xFF #x2F #x00))
  (defconstant kPPQNValue #x30)
  (defvar *events* nil)
  (defvar *channel-no-1* 1)
  (defvar *channel-no-2* 2)
  (defvar *channel-no-3* 3)
  (defvar *channel-no-4* 4)
  (defvar *channel-no-5* 5)
  (defvar *channel-no-6* 6)
  (defvar *channel-no-7* 7)
  (defvar *channel-no-8* 8)
  (defvar *channel-no-9* 9)
  (defvar *channel-no-10* 10)
  (defvar *channel-no-11* 11)
  (defvar *channel-no-12* 12)
  (defvar *channel-no-13* 13)
  (defvar *channel-no-14* 14)
  (defvar *channel-no-15* 15)
  (defvar *channel-no-16* 16)
  (defvar *overwrite-if-exists* t)
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Markov models/generating-with-patterns.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Markov models"
    "/pattern-inheritance-preliminaries.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Pattern discovery"
    "/further-structural-induction-algorithms.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Markov models/markov-analyse-backwards.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/File conversion/midi-import.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Pattern discovery/compactness-trawl.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Pattern discovery/evaluation-for-SIACT.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/File conversion/csv-files.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/Maths foundation/geometric-operations.lisp"))
  (load
   (concatenate
    'string
    *MCStylistic-Oct2010-functions-path*
    "/File conversion/kern.lisp"))
  "Welcome to MCStylistic-Oct2010.")
