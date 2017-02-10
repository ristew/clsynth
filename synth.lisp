(defconstant +sample-rate+ 44100)
(defconstant +attack-time+ 0.5)
(defconstant +decay-time+ 0.3)
(defconstant +release-time+ 0.6)
(defconstant +sustain-volume+ 0.6)
(defconstant +chunk-size+ 1024)

(defvar *cycle* 0)

(defun note (hz)
  (/ +sample-rate+ (* 2 pi hz)))

(defun sample-hz (hz)
  (sin (/ *cycle* (note hz))))

(defun sample-harm (hz harm)
  (* (/ 1.0 harm) (sample-hz (* hz harm))))

(defun next-cycle ()
  (setq *cycle* (+ *cycle* 1)))

(defun square-wave-sample (hz vol)
  (let ((harms '(1 3 5 7 9 11))
    (applicator (lambda (harm)
            (* vol (sample-harm hz harm)))))
  (reduce #'+ (mapcar applicator harms))))

(defun attack (time)
  (/ time +attack-time+))

(defun decay (time)
  (+ 1.0 (* (- time +attack-time+) (/ (- +sustain-volume+ 1.0) +decay-time+))))

(defun release (passed)
  (+ +sustain-volume+ (* passed  (/ (- +sustain-volume+) +release-time+))))

(defun adsr-vol (time dur)
  (cond
  ((< time +attack-time+) (attack time))
  ((< time (+ +attack-time+ +decay-time+)) (decay time))
  ((> time (- dur +release-time+)) (release (- time (- dur +release-time+))))
  (t +sustain-volume+)))

(defun raw-note-data (hz vol dur)
  (let ((samples (floor
          (* (+ (max dur (+ +attack-time+ +decay-time+)) +release-time+)
           +sample-rate+)))
    (sample-list '()))
  (dotimes (sample samples sample-list)
    (progn
    (setq sample-list
        (cons (* (adsr-vol (/ sample +sample-rate+) (/ samples +sample-rate+))
             (square-wave-sample hz vol)) sample-list))
    (next-cycle)))))

(defun list-to-vec (ll)
  (make-array (list (length ll))))


(defun make-chunks (list chunk-size)
  (cond
  ((= (length list) 0) '())
  ((< (length list) chunk-size) (list (append list (make-list (- chunk-size (length list)) :initial-element 0.0))))
  (t (cons (subseq list 0 chunk-size) (make-chunks (subseq list chunk-size) chunk-size)))))
(ql:quickload :cl-portaudio)
(use-package :portaudio)

(defun play-note (hz vol dur)
  (portaudio:with-audio
  (portaudio:with-default-audio-stream (astream 1 1)
    (let*
      ((audio (raw-note-data hz vol dur))
       (audio-chunks (make-chunks audio +chunk-size+)))
    (loop
       for chunk in audio-chunks do (portaudio:write-stream astream (list-to-vec chunk)))))))

(print "playing A note")
(play-note 220 0.5 2.0)
(play-note 329 0.5 2.0)
(play-note 440 0.5 2.0)
(print "...done.")
