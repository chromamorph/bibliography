#| Copyright 2008-2011 Tom Collins and Peter Elsea
   Monday 26 January 2009

The main function here is load-midi-file, for
importing a MIDI (Musical Instrument Digital
Interface) file into the Lisp environment as a list of
lists. |#

; REQUIRED PACKAGES
; (in-package :common-lisp-user)
(load
 (concatenate
  'string
  *MCStylistic-Oct2010-functions-path*
  "/Maths foundation/list-processing.lisp"))

; Two chunk types defined so far
; (defvar *chunk-type* ())
; Number of bytes in chunk
; (defvar *chunk-length* 0)
#| Type 0 is single track, type 1 is multitrack, type
2 is indepentent loops. |#
; (defvar *midi-file-format* 0)
; Number of tracks in file
; (defvar *midi-file-ntrks* 0)
; Number of ticks per crotchet
; (defvar *midi-file-granularity* 24)
; Unconverted track time, in ticks
; (defvar *track-time* 0)
; Running status is used
; (defvar *running-status* 0)
#| Flag for finding ends of tracks (rather than byte
counting). EOT sets this nil. |#
; (defvar *track-end* t)

; A place for metadata
#|
(defvar
    *sequence-strings*
  (make-array
   1
   :initial-contents #("sequence-strings")
   :fill-pointer t :adjustable t))
|#
#| A place to put tempos when converting from ticks to
time in ms. The format of each entry is (time-in-ticks
time-in-ms usec/qn). |#
#|
(defvar
    *sequence-tempo-map*
  (make-array
   1
   :element-type 'list :initial-element '(0 0 1)
   :fill-pointer t :adjustable t))
|#
#| A place to put note data. The variable 
*sequence-notes* has the format (time-ms  note-number
duration channel velocity). This is an array to
simplify setting durations when note off is
detected. |#
#|
(defvar
    *sequence-notes*
  (make-array
   0
   :element-type 'list :initial-element '(0 0 0 0 0)
   :fill-pointer t :adjustable t))
|#

#|
\noindent Example:
\begin{verbatim}
(add-tempo '(5012 5012 5012))
--> 2
\end{verbatim}

\noindent As tempo and granularity are needed to
convert ticks to ms, this function is invoked when the
number 81 is parsed (which indicates a tempo
change). The format of each entry here is
(time-in-ticks time-in-ms usec/qn). Storing the time
of the tempo change in both formats simplifies the
calculations. |#

(defun add-tempo (the-data)
  (let* ((us-qn
          (+ (ash (first the-data) 16)
             (ash (second the-data) 8)
             (third the-data)))
         (last-tempo-entry
          (elt *sequence-tempo-map* 
               (- (length *sequence-tempo-map*) 1)))
         (last-tempo-time (second last-tempo-entry))
         (last-tempo (third last-tempo-entry))
         (ticks
          (- *track-time* (first last-tempo-entry))))
    (vector-push-extend
     (list
      *track-time* 
      (+
       last-tempo-time
       (/
        (*
         ticks last-tempo)
        (* *midi-file-granularity* 1)))
      us-qn) *sequence-tempo-map*)))

#|
\noindent Example: within get-header example,
\begin{verbatim}
(convert-granularity (get-short input-stream))
--> 120
\end{verbatim}

\noindent The treatment of division is unusual.
Granularity is in ticks per beat (crotchet). |#

(defun convert-granularity (division)
  (let ((high-byte
         (ash division -8)) (low-byte (logand #XFF)))
    (case high-byte
      (#XE2 (* 30 low-byte)) (#XE3 (* 30 low-byte))
      (#XE7 (* 25 low-byte)) (#XE8 (* 24 low-byte))
      (t division))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (convert-vlq (get-vlq input-stream)))
--> 77
\end{verbatim}

\noindent This function converts an integer
represented using variable-length quantity (VLQ) into
the digit representation. In MIDI files, time is
listed as ticks in VLQs. |#

(defun convert-vlq (arg-list &optional (accum 0))
      (if (> (first arg-list) 127)
        (convert-vlq
         (rest arg-list)
         (+ (- (first arg-list) 128) (* accum 128)))
        (+ (first arg-list) (* accum 128))))

#|
\noindent Example:
\begin{verbatim}
(earlier '(5 60) '(6.5 61))
--> T
\end{verbatim}

\noindent This function returns T if the first element
of the first argument is less than the first element
of the second argument, and NIL otherwise. |#

(defun earlier (alist blist)
  (< (first alist) (first blist)))

#|
\noindent Example:
\begin{verbatim}
(first>= 60 '(59 64 67))
--> T
\end{verbatim}

\noindent This is a test function for tempo
searches. |#

(defun first>= (data alist) (>= data (first alist)))

#|
\noindent Example:
\begin{verbatim}
(with-open-file
    (s
     (concatenate
      'string
      *MCStylistic-Oct2010-example-files-path*
      "/temp-bytes")
     :direction :output :element-type 'unsigned-byte)
  (write-byte 101 s) (write-byte 111 s))
--> 111
(with-open-file
    (s
     (concatenate
      'string
      *MCStylistic-Oct2010-example-files-path*
      "/temp-bytes") :element-type 'unsigned-byte)
  (gather-bytes s 2))
--> (101 111)
\end{verbatim}

\noindent This function reads arbitrary bytes into a
list. |#

(defun gather-bytes (input-stream how-many)
  (if (zerop how-many) ()
    (cons
     (read-byte input-stream)
     (gather-bytes input-stream (1- how-many)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (setup)
  (get-header input-stream))
--> 120
\end{verbatim}

\noindent This function reads the file header. |#

(defun get-header (input-stream)
    (setq *chunk-type* (get-type input-stream))
    (setq *chunk-length* (get-word input-stream))
    (setq *midi-file-format* (get-short input-stream))
    (setq *midi-file-ntrks* (get-short input-stream))
    (setq
     *midi-file-granularity*
     (convert-granularity (get-short input-stream))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (get-metadata input-stream))
--> (84 104 100 0 0 0 6 0 1 0 12 0 120 77 84 114 107 0
     0 0 12 0 255 81 3 15 66 64 196 56 255 47 0 77 84
     114 107 0 0 1 196 0 192 6 0 144 49 64 0 255 3 11
     104 97 114 112 115 105 99 104 111 114 100 0 255 4
     11 104 97 114 112 115 105 99 104 111 114)
\end{verbatim}

\noindent This function reads a length, then gathers
that many bytes together (representing metadata). |#

(defun get-metadata (input-stream)
  (gather-bytes
   input-stream (read-byte input-stream)))

#|
\noindent Example: within get-header example,
\begin{verbatim}
(get-short input-stream)
--> 1
\end{verbatim}

\noindent This function is a 16-bit retriever. |#

(defun get-short (input-stream)
    (+
     (* (read-byte input-stream) 256)
     (read-byte input-stream)))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (setup)
  (get-header input-stream)
  (get-track-header input-stream))
--> 12
\end{verbatim}

\noindent This function reads a track header. |#

(defun get-track-header (input-stream)
    (setq *chunk-type* (get-type input-stream))
    (setq *chunk-length* (get-word input-stream)))

#|
\noindent Example: within get-header example,
\begin{verbatim}
(get-type input-stream)
--> "MThd"
\end{verbatim}

\noindent Helps to read header. |#

(defun get-type (input-stream)
    (let ((type-string (make-string 4)))
      (loop
        for i from 0 to 3 do
        (setf
         (char type-string i)
         (code-char (read-byte input-stream))))
      type-string))

#|
\noindent Example: within get-header example,
\begin{verbatim}
(get-word input-stream)
--> 6
\end{verbatim}

\noindent This function is a 32-bit retriever. |#

(defun get-word (input-stream)
    (let ((value 0))
      (loop for i from 0 to 3 do
        (setq
         value
         (+ (* value 256) (read-byte input-stream))))
      value))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (get-vlq input-stream))
--> (77)
\end{verbatim}

\noindent All events are seperated by a delta time, so
this function gets the VLQ. |#

(defun get-vlq (input-stream)
  (let ((new-byte (read-byte input-stream)))
    (if (< new-byte 128) (list new-byte)
        (cons new-byte (get-vlq input-stream)))))

#|
\noindent Example:
\begin{verbatim}
(handle-bend #XA0 60 3)
--> (160 60 3)
\end{verbatim}

\noindent This function discards bend data. |#

(defun handle-bend (status lsb msb)
  (list status lsb msb))

#|
\noindent Example:
\begin{verbatim}
(handle-control #XA0 60 84)
--> (160 60 84)
\end{verbatim}

\noindent This function discards control data. |#

(defun handle-control (status cn value)
  (list status cn value))

#|
\noindent Example:
\begin{verbatim}
(handle-note #XA0 60 84)
--> 0
\end{verbatim}

\noindent This function parses a note-on event. |#

(defun handle-note (status nn vel)
  (vector-push-extend
   (list
    (ticks-ms *track-time*) nn 0
    (+ (logand status #X0F) 1) vel)
   *sequence-notes*))

#|
\noindent Example:
\begin{verbatim}
(handle-off #XA0 60 84)
--> 0
\end{verbatim}

\noindent This function searches for the note-on event
to which a note-off event belongs and sets the
duration accordingly. This does not handle overlapping
notes of the same pitch. |#

(defun handle-off (status &rest nn)
  (let* ((channel (+ (logand status #X0F) 1)) 
	 (where
          (position
           (list channel (first nn))
           *sequence-notes* :test #'match-note
           :from-end t))
	 (the-note) (duration))
    (if (null where) () ; No matchng note on
      (progn
	(setf the-note (elt *sequence-notes* where))
	(setf
         duration
         (- (ticks-ms *track-time*) (first the-note)))
	(setf
         (third (elt *sequence-notes* where))
         duration)))))

#|
\noindent Example:
\begin{verbatim}
(handle-pressure #XA0 60)
--> (160 60)
\end{verbatim}

\noindent This function discards pressure data. |#

(defun handle-pressure (status pressure)
  (list status pressure))

#|
\noindent Example:
\begin{verbatim}
(handle-program #XA0 60)
--> (160 60)
\end{verbatim}

\noindent This function discards program data. |#

(defun handle-program (status pn) (list status pn))

#|
\noindent Example:
\begin{verbatim}
(handle-touch #XA0 60 84)
--> (160 60 84)
\end{verbatim}

\noindent This function discards touch data. |#

(defun handle-touch (status nn pressure)
  (list status nn pressure))

#|
\noindent Example:
\begin{verbatim}
(list-to-string '(84 104 111 109 97 115))
--> "Thomas."
\end{verbatim}

\noindent This function converts ASCII to strings.
Note that most metadata is text. |#

(defun list-to-string (ascii)
  (if (null ascii) #\. 
    (format
     nil "~A~A" (code-char (car ascii))
     (list-to-string (cdr ascii)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 imported-midi
 (load-midi-file
  (concatenate
   'string *MCStylistic-Oct2010-example-files-path*
   "/vivaldi-op6-no3-2.mid")))
(subseq imported-midi 0 10)
--> ((0 79 1 6 64) (0 69 1 4 64) (0 49 1 1 64)
     (0 49 1 3 64) (0 76 1 5 64) (1 79 1 6 64)
     (1 69 1 4 64) (1 76 1 5 64) (1 49 1 1 64)
     (1 49 1 3 64))
\end{verbatim}

\noindent This main function imports a MIDI file
into the Lisp environment. |#

(defun load-midi-file (fstring)
  (with-open-file
      (input-stream
       fstring :element-type '(unsigned-byte 8)
       :if-does-not-exist nil)
    (setup)
    (get-header input-stream)
    (do ((track-index 0 (+ track-index 1)))
	((>= track-index *midi-file-ntrks*) ())
      (read-track input-stream))
    (setq
     *sequence-notes*
     (sort *sequence-notes* #'earlier))
    (coerce *sequence-notes* 'list)))

#|
\noindent Example:
\begin{verbatim}
(match-note '(#XA0 62) '(4 62 0 #XA0))
--> T
\end{verbatim}

\noindent This function tests for a note-off event. |#

(defun match-note (status-nn target)
  (and
   (= (second status-nn) (second target))
   (= (first status-nn) (fourth target))
   (zerop (third target))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (parse-events #XA0 60 input-stream))
--> (160 60 77)
\end{verbatim}

\noindent This function handles track data. |#

(defun parse-events
       (status-byte data-byte input-stream)
  (let ((vel))
    (cond 
     ((< status-byte #X90)
      (handle-off
       status-byte data-byte
       (read-byte input-stream)))
     ((< status-byte #XA0)
      (if (zerop
           (setq vel (read-byte input-stream)))
        (handle-off status-byte data-byte)
        (handle-note status-byte data-byte vel)))
     ((< status-byte #XB0)
      (handle-touch
       status-byte data-byte
       (read-byte input-stream)))
     ((< status-byte #XC0)
      (handle-control
       status-byte data-byte
       (read-byte input-stream)))
     ((< status-byte #XD0)
      (handle-program status-byte data-byte))
     ((< status-byte #XE0)
      (handle-pressure status-byte data-byte))
     ((< status-byte #XF0)
      (handle-bend
       status-byte data-byte
       (read-byte input-stream)))
     ((= status-byte #XF0)
      (strip-sysex input-stream))
     ((= status-byte #XFF)
      (parse-metadata
       (cons
        data-byte (get-metadata input-stream))))
     (t ()))))

#|
\noindent Example:
\begin{verbatim}
(parse-metadata '(9 104))
--> 1
\end{verbatim}

\noindent This function extracts tempo and end-of-
track information from the metadata. |#

(defun parse-metadata (the-data)
  (case (car the-data)
    (0 ()) ; Sequence number
    ((1 2 3 4 5 6 7 8 9 10)
     (vector-push-extend
      (list-to-string (cdr the-data))
      *sequence-strings* )) ; Text 
    (#X20 ()) ; MIDI Channel prefix
    (#X2F (setq *track-end* nil)) ; End of track
    (#X51 ()) ; Set tempo usec/qn in
              ; *sequence-tempo-map*
    (#X54 ()) ; SMPTE offset H:M:S:F:F/100
    (#X58 ()) ; Time Signature nnn dd cc bb
    (#X59 ()) ; Key Signature
    (#X7F ()) ; Program specific
    (t ())    ; Unknown
    ))

#|
\noindent Example:
\begin{verbatim}
(re-time
 '((1 69 1 4 64) (0 79 1 6 64) (2 49 1 3 64)
   (0 69 1 4 64))
 '((1 69 1 4 64) (0 79 1 6 64) (2 49 1 3 64)
   (0 69 1 4 64)))
--> ((1 69 1 4 64) (0 79 1 6 64) (2 49 1 3 64)
     (0 69 1 4 64) (2 69 1 4 64) (1 79 1 6 64)
     (3 49 1 3 64) (1 69 1 4 64))
\end{verbatim}

\noindent This function appends some events (the
second argument) to other events (the first argument)
by translating them by the ontime of the last event
from the first argument. |#

(defun re-time (to-be-added-to-events events)
  (let* ((last-event (my-last events))
         (offset-time
          (if last-event
            (+
             (first last-event)
             (third last-event)) 0)))
    (append
     events
     (loop for event in to-be-added-to-events
       collect
       (cons
        (+ (first event) offset-time)
        (rest event))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (read-and-parse-event input-stream))
--> NIL
\end{verbatim}

\noindent This function deals with running status. |#

(defun read-and-parse-event (input-stream)
  (let ((first-byte (read-byte input-stream)))
    (if (>= first-byte #X80)
      (parse-events
       (setf *running-status* first-byte)
       (read-byte input-stream) input-stream)
      (parse-events
       *running-status* first-byte input-stream))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (setup)
  (get-header input-stream)
  (read-track input-stream))
--> NIL
\end{verbatim}

\noindent This function is called once per track,
and reads track data. |#

(defun read-track (input-stream)
  (get-track-header input-stream)
  (if (zerop *chunk-length*) ()
    (if (not (equal *chunk-type* "MTrk"))
      ; Discard alien chunks
      (gather-bytes
       input-stream *chunk-length*)
      (do ((*track-end* t) (*track-time* 0)
           (*running-status* 0))
	  ((null *track-end*) ())
	(set-track-time input-stream)
	(read-and-parse-event input-stream)))))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (set-track-time input-stream))
--> 77
\end{verbatim}

\noindent There are times between events, so
*track-time* must be accumulated across each track. |#

(defun set-track-time (input-stream)
  (incf
   *track-time* (convert-vlq (get-vlq input-stream))))

#|
\noindent Example:
\begin{verbatim}
(setup)
--> #()
\end{verbatim}

\noindent This function sets the values of three
variables. |#

(defun setup ()
  (setf
   *sequence-strings*
   (make-array
    1 :initial-contents #("sequence-strings")
    :fill-pointer t :adjustable t))
  (setq
   *sequence-tempo-map*
   (make-array
    1 :element-type 'list :initial-element '(0 0 1)
    :fill-pointer t :adjustable t))
  (setq
   *sequence-notes*
   (make-array
    0 :element-type 'list
    :initial-element '(0 0 0 0 0) :fill-pointer t
    :adjustable t)))

#|
\noindent Example:
\begin{verbatim}
(setq
 fstring
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/vivaldi-op6-no3-2.mid"))
(with-open-file
    (input-stream
     fstring :element-type '(unsigned-byte 8)
     :if-does-not-exist nil)
  (strip-sysex input-stream))
--> "Error: Unexpected end of file"
\end{verbatim}

\noindent This function deletes sysex data. The
example gives an error because the imported MIDI file
does not contain any sysex. |#

(defun strip-sysex (input-stream)
  (if (= (read-byte input-stream) #XF7) ()
      (strip-sysex input-stream)))

#|
\noindent Example:
\begin{verbatim}
(ticks-ms 50)
--> 5/12
\end{verbatim}

\noindent The time conversion function searches the
tempo map from the end to find tempo in effect at the
time. |#
 
(defun ticks-ms (ticks)
  (let* ((current-tempo-entry
          (find
           ticks *sequence-tempo-map*
           :test #'first>= :from-end t))
	 (current-tempo-time
          (second current-tempo-entry))
	 (current-tempo (third current-tempo-entry))
	 (delta-ticks
          (- ticks (first current-tempo-entry))))
    (+
     current-tempo-time
     (/
      (*
       delta-ticks current-tempo)
      (* *midi-file-granularity* 1)))))
