#| Copyright 2008-2011 Tom Collins, Paul Pelton, Soren
Goodman, and David Cope
   Tuesday 27 January 2009

The functions below are for exporting datapoints
(dimensions for ontime in milliseconds, MIDI note
number, duration in milliseconds, channel, and
velocity) to a MIDI file. The main function is
saveit. |#

; REQUIRED PACKAGES:
; (in-package :common-lisp-user)

#|
(defconstant
    kMThdHeaderBytes '(#x4D #x54 #x68 #x64))
|#
; (defconstant kMThdHeaderLenBytes '(0 0 0 6))
; (defconstant kMThdHeaderFormatBytes '(0 1))
#|
(defconstant
 kMTrkHeaderBytes '(#x4D #x54 #x72 #x6B))
|#
; (defconstant kMTrkEndBytes '(#x00 #xFF #x2F #x00))
; (defconstant kPPQNValue #x30)
; (defvar *events* nil)
; (defvar *channel-no-1* 1)
; (defvar *channel-no-2* 2)
; (defvar *channel-no-3* 3)
; (defvar *channel-no-4* 4)
; (defvar *channel-no-5* 5)
; (defvar *channel-no-6* 6)
; (defvar *channel-no-7* 7)
; (defvar *channel-no-8* 8)
; (defvar *channel-no-9* 9)
; (defvar *channel-no-10* 10)
; (defvar *channel-no-11* 11)
; (defvar *channel-no-12* 12)
; (defvar *channel-no-13* 13)
; (defvar *channel-no-14* 14)
; (defvar *channel-no-15* 15)
; (defvar *channel-no-16* 16)
; (defvar *overwrite-if-exists* t) 

#|
\noindent Example:
\begin{verbatim}
(convert-ontime-to-deltatime 500)
--> 24
    0
\end{verbatim}

\noindent This function converts an ontime to a
deltatime. |#

(defun convert-ontime-to-deltatime (ontime)
   (round (* (/ kPPQNValue 1000) ontime)))

#|
\noindent Example:
\begin{verbatim}
(create-midi-events
 '((0 36 1000 1 35) (0 48 1000 1 35) (500 60 500 7 40)
   (0 1 0 1 255)))
--> ((0 (144 36 35)) (48 (128 36 35)) (0 (144 48 35))
     (48 (128 48 35)) (24 (150 60 40))
     (48 (134 60 40)) (0 (192 0)))
\end{verbatim}

\noindent This function converts datapoints into the
format required for the function
create-midi-track-data. |#

(defun create-midi-events (events)
   (if (null events) ()
      (let* ((event (first events))
             (ontime (first event)))
         (append
          (cond
           ((= (fifth event) 255)
            (list
             (list
              (convert-ontime-to-deltatime ontime)
              (make-midi-pc-msg event))))
           (t
            (list
             (list
              (convert-ontime-to-deltatime ontime)
              (make-midi-note-msg event #x90))
             (list
              (convert-ontime-to-deltatime
               (+ ontime (third event)))
              (make-midi-note-msg event #x80)))))
          (create-midi-events (rest events))))))

#|
\noindent Example:
\begin{verbatim}
(setq
 outfilename
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/midi-export-test.mid"))
(create-midi-file outfilename)
--> #<BASIC-FILE-BINARY-OUTPUT-STREAM
    ((concatenate
      'string *MCStylistic-Oct2010-example-files-path*
      "/midi-export-test.mid")/13 ISO-8859-1)
    #x3000419BC3FD>
\end{verbatim}

\noindent This function creates an output stream. |#

(defun create-midi-file (outfilename)
  (open
   outfilename :direction :output
   :if-exists
   (if *overwrite-if-exists* :overwrite :error)
   :if-does-not-exist :create
   :element-type 'unsigned-byte))

#|
\noindent Example:
\begin{verbatim}
(create-midi-track-data '((0 (193 1)) (2 (193 1))))
--> (0 193 1 2 193 1)
\end{verbatim}

\noindent Each element of the variable midiEvents is
of the format
((deltaTime (byte3 byte2 byte1)).
They should be sorted in the order for the file and
their deltaTimes are relative to each other (each
relative to the previous). This function creates an
integer-stream representation. |#

(defun create-midi-track-data (midiEvents)
   (if (null midiEvents) ()
      (let ((midiEvt (first midiEvents)))
        (append
         (make-var-len (first midiEvt))
         (second midiEvt)
         (create-midi-track-data
          (rest midiEvents))))))

#|
\noindent Example:
\begin{verbatim}
(create-midi-tracks
 '((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255)
   (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255) 
   (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255)
   (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255)
   (0 13 0 13 255) (0 14 0 14 255) (0 15 0 15 255)
   (0 16 0 16 255) (0 60 1000 1 127)))
--> ((77 84 114 107 0 0 0 4 0 255 47 0)
     (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48
      128 60 127 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 193 1 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 194 2 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 195 3 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 196 4 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 197 5 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 198 6 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 199 7 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 200 8 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 201 9 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 202 10 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 203 11 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 204 12 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 205 13 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 206 14 0 255 47 0)
     (77 84 114 107 0 0 0 7 0 207 15 0 255 47 0))
\end{verbatim}

\noindent This function takes datapoints and lists
representing the ends of tracks, and converts them
into the integer streams in preparation for the
function write-to-midi-file. |#

(defun create-midi-tracks (events)
  (let ((tracks (create-tempo-track)))
    (dotimes (channel 16 (reverse tracks))
      (let ((channelEvents
             (get-channel-events
              (1+ channel) events)))
        (if channelEvents
          (push
           (create-MTrk channelEvents) tracks))))))

#|
\noindent Example:
\begin{verbatim}
(create-MThd 17)
--> (77 84 104 100 0 0 0 6 0 1 0 17 0 48)
\end{verbatim}

\noindent This function creates the integer stream
representing a MIDI file's header data. |#

(defun create-MThd (numtracks)
   (append
    kMThdHeaderBytes
    kMThdHeaderLenBytes
    kMThdHeaderFormatBytes
    (list 0 numtracks 0 kPPQNValue)))

#|
\noindent Example:
\begin{verbatim}
(create-MTrk '((0 1 0 1 255) (0 60 1000 1 127)))
--> (77 84 104 100 0 0 0 6 0 1 0 17 0 48)
\end{verbatim}

\noindent This function creates the integer stream
representing one track in a MIDI file. |#

(defun create-MTrk (events)
   (if (null events) ()
      (let* ((mtrkData
              (append
               (create-midi-track-data
                (fix-deltatime
                 0
                 (sort-by-deltatime
                  (create-midi-events events))))
               kMTrkEndBytes)))
        (append
         kMTrkHeaderBytes
         (split-bytes (length mtrkData) 4)
         mtrkData))))

#|
\noindent Example:
\begin{verbatim}
(create-tempo-track)
--> ((77 84 114 107 0 0 0 4 0 255 47 0))
\end{verbatim}

\noindent This function creates the integer
representation for a MIDI file's tempo track. |#

(defun create-tempo-track ()
   (list
    (append
     kMTrkHeaderBytes
     (split-bytes
      (length kMTrkEndBytes) 4) kMTrkEndBytes)))

#|
\noindent Example:
\begin{verbatim}
(fix-deltatime 40 '((0 (207 15))))
--> ((-40 (207 15)))
\end{verbatim}

\noindent This function shifts all of the deltatimes
back by the first argument. |#

(defun fix-deltatime (lasttime midiEvents)
   (if (null midiEvents) ()
      (let* ((midiEvt (first midiEvents))
            (newLastTime (first midiEvt)))
         (cons
          (list
           (- newLastTime lasttime)
           (second midiEvt))
          (fix-deltatime
           newLastTime (rest midiEvents))))))

#|
\noindent Example:
\begin{verbatim}
(get-channel-events
 1
 '((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255)
   (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255) 
   (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255)
   (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255)
   (0 13 0 13 255) (0 14 0 14 255) (0 15 0 15 255)
   (0 16 0 16 255) (0 60 1000 1 127)))
--> ((0 1 0 1 255) (0 60 1000 1 127))
\end{verbatim}

\noindent This function takes an integer between 1 and
16 (inclusive) as its first argument, and a list of
datapoints as its second argument. It returns elements
of this list whose fourth element is equal to the
first argument. |#

(defun get-channel-events (channel events)
   (cond
    ((null events) ())
    ((= channel (fourth (first events)))
     (cons
      (first events)
      (get-channel-events channel (rest events))))
    (t (get-channel-events channel (rest events)))))

#|
\noindent Example:
\begin{verbatim}
(get-byte 3 7)
--> 0
\end{verbatim}

\noindent This function converts an integer to bytes,
starting at the rightmost index. |#

(defun get-byte (byteIndex num)
   (ldb (byte 8 (* 8 byteIndex)) num))

#|
\noindent Example:
\begin{verbatim}
(insert-program-changes '((0 60 1000 1 127)))
--> ((0 1 0 1 255) (0 2 0 2 255) (0 3 0 3 255)
     (0 4 0 4 255) (0 5 0 5 255) (0 6 0 6 255)
     (0 7 0 7 255) (0 8 0 8 255) (0 9 0 9 255)
     (0 10 0 10 255) (0 11 0 11 255) (0 12 0 12 255)
     (0 13 0 13 255) (0 14 0 14 255) (0 15 0 15 255)
     (0 16 0 16 255) (0 60 1000 1 127))
\end{verbatim}

\noindent This function inserts MIDI track headers as
datapoints (signified by 255). |#

(defun insert-program-changes (events)
  (append
   (list
    (list 0 *channel-no-1* 0 1 255)
    (list 0 *channel-no-2* 0 2 255)
    (list 0 *channel-no-3* 0 3 255)
    (list 0 *channel-no-4* 0 4 255)
    (list 0 *channel-no-5* 0 5 255)
    (list 0 *channel-no-6* 0 6 255)
    (list 0 *channel-no-7* 0 7 255)
    (list 0 *channel-no-8* 0 8 255)
    (list 0 *channel-no-9* 0 9 255)
    (list 0 *channel-no-10* 0 10 255)
    (list 0 *channel-no-11* 0 11 255)
    (list 0 *channel-no-12* 0 12 255)
    (list 0 *channel-no-13* 0 13 255)
    (list 0 *channel-no-14* 0 14 255)
    (list 0 *channel-no-15* 0 15 255)
    (list 0 *channel-no-16* 0 16 255))
   events))

#|
\noindent Example:
\begin{verbatim}
(make-midi-note-msg '(0 60 1000 1 127) 144)
--> (144 60 127)
\end{verbatim}

\noindent This function creates MIDI note messages of
the type processed by the function
create-midi-track-data. It should be pointed out that
\#x90 is for a note-on and \#x80 for a note-off. |#

(defun make-midi-note-msg (event flag)
   (list
    (logior
     (1- (fourth event)) flag)
    (second event) (fifth event)))

#|
\noindent Example:
\begin{verbatim}
(make-midi-pc-msg '(0 16 0 16 255))
--> (207 15)
\end{verbatim}

\noindent This function creates MIDI PC messages of
the type processed by the function
create-midi-track-data. It should be pointed out that
\#xC0 is for a program change. |#

(defun make-midi-pc-msg (event)
   (list
    (logior (1- (fourth event)) #xC0)
    (1- (second event))))

#|
\noindent Example:
\begin{verbatim}
(make-var-len 1241)
--> (137 89)
\end{verbatim}

\noindent This function converts integers to a binary-
like representation. Adapted from www.borg.com/~jglatt
/tech/midifile.htm. |#

(defun make-var-len (value)
   (let ((buffer (list (logand #x7F value))))
      (loop while
        (not (zerop (setq value (ash value -7))))
        do
        (push
         (logior
          (logior #x80 (logand #x7F value))) buffer))
     buffer))

#|
\noindent Example:
\begin{verbatim}
(save-as-midi
 "midi-export-test.mid"
 '((0 36 1000 1 35) (0 48 1000 1 35) (500 60 500 7 40)
   (1000 63 500 7 45) (1500 67 500 7 50)
   (2000 72 500 7 55) (2500 75 500 7 60)
   (3000 79 500 12 65) (3500 84 500 12 70)
   (4000 79 500 12 75) (4500 75 500 12 80)
   (5000 72 500 12 84) (5500 67 500 12 84)))
--> (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/midi-export-test.mid")
\end{verbatim}

\noindent This function exports datapoints (dimensions
for ontime in milliseconds, MIDI note number, duration
in milliseconds, channel, and velocity) to a MIDI
file. It can clip the end of the file, so to avoid
this, use the function saveit. |#

(defun save-as-midi
       (outfilename events &optional
        (pathname
         (concatenate
          'string
          *MCStylistic-Oct2010-example-files-path*
          "/")))
  (when pathname
    (setq
     outfilename
     (concatenate 'string pathname outfilename))) 
  (let ((tracks
         (create-midi-tracks
          (insert-program-changes events))))
    (with-open-stream
        (file (create-midi-file outfilename))
      (push (create-MThd (length tracks)) tracks)
      (write-to-midi-file file tracks))
    outfilename))

#|
\noindent Example:
\begin{verbatim}
(saveit
 (concatenate
  'string
  *MCStylistic-Oct2010-example-files-path*
  "/midi-export-test.mid")
 '((0 36 1000 1 35) (0 48 1000 1 35) (500 60 500 7 40)
   (1000 63 500 7 45) (1500 67 500 7 50)
   (2000 72 500 7 55) (2500 75 500 7 60)
   (3000 79 500 12 65) (3500 84 500 12 70)
   (4000 79 500 12 75) (4500 75 500 12 80)
   (5000 72 500 12 84) (5500 67 500 12 84)))
--> (concatenate
     'string
     *MCStylistic-Oct2010-example-files-path*
     "/midi-export-test.mid")
\end{verbatim}

\noindent This function is very similar to the
function save-as-midi (exporting datapoints with
dimensions for ontime in milliseconds, MIDI note
number, duration in milliseconds, channel, and
velocity, to a MIDI file). The difference is that this
function does not clip the end of the file. |#

(defun saveit
       (outfilename events &optional
        (last-event (first (last events)))
        (new-time
         (if last-event
           (+
            (first last-event) (third last-event)
            500) (identity 500))))
  (let ((tracks
         (create-midi-tracks
          (insert-program-changes
           (append
            events
            (list (list new-time 1 500 1 1)))))))
    (if (probe-file outfilename)
      (delete-file outfilename))
    (with-open-stream
        (file (create-midi-file outfilename))
      (push (create-MThd (length tracks)) tracks)
      (write-to-midi-file file tracks))
    outfilename))

#|
\noindent Example:
\begin{verbatim}
(sort-by-deltatime '((24 (207 15)) (0 (207 15))))
--> ((0 (207 15)) (24 (207 15)))
\end{verbatim}

\noindent This function sorts a list of lists
ascending by the car of each list. |#
                                 
(defun sort-by-deltatime (midiEvents)
   (sort midiEvents #'< :key #'car))

#|
\noindent Example:
\begin{verbatim}
(split-bytes 7 4)
--> (0 0 0 7)
\end{verbatim}

\noindent This function splits a long integer into its
high byte and low byte. |#

(defun split-bytes (num count)
   (let ((bytes ()))
      (dotimes (i count bytes)
         (push (get-byte i num) bytes))))

#|
\noindent Example:
\begin{verbatim}
(setq
 outfilename
 (concatenate
  'string *MCStylistic-Oct2010-example-files-path*
  "/midi-export-test.mid"))
(write-to-midi-file
 (create-midi-file outfilename)
 '((77 84 104 100 0 0 0 6 0 1 0 17 0 48) 
   (77 84 114 107 0 0 0 4 0 255 47 0)
   (77 84 114 107 0 0 0 15 0 192 0 0 144 60 127 48 128
    60 127 0 255 47 0) 
   (77 84 114 107 0 0 0 7 0 193 1 0 255 47 0)
   (77 84 114 107 0 0 0 7 0 194 2 0 255 47 0)))
--> NIL
\end{verbatim}

\noindent This function will convert MIDI track events
to bytes and write them to a specified file. |#

(defun write-to-midi-file (file listOfChunks)
   (if (null listOfChunks) ()
      (progn
       (dolist (byte (first listOfChunks))
         (write-byte byte file))
       (write-to-midi-file
        file (rest listOfChunks)))))
