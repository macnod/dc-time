(in-package :dc-time)

(defvar *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))

(defun timestamp-string (&key
                           (universal-time (get-universal-time))
                           (timezone 0)
                           (format "%Y-%M-%DT%h:%m:%s"))
  "Returns the given time (or the current time, in universal time
format) formatted according to the FORMAT parameter, followed by an
optional value for STRING.  If STRING is provided, the function adds a
space to the result and then appends the string to that.  The FORMAT
string can contain any characters.  This function will replace the
format characters Y, M, D, h, m, and s, with numbers representing the
year,month, day, hour, minute, and second, respectively.  All the
numbers are 2 digits long, except for the year, which is 4 digits
long."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time timezone)
    (let* ((parts (ds (list :map
                               "%Y" (format nil "~d"     year)
                               "%M" (format nil "~2,'0d" month)
                               "%D" (format nil "~2,'0d" day)
                               "%h" (format nil "~2,'0d" hour)
                               "%m" (format nil "~2,'0d" minute)
                               "%s" (format nil "~2,'0d" second))))
           (elements (loop for i from 0 below (length format)
                           for v = (subseq format i (+ i 2))
                           for fs = (pick parts v)
                           collect (if fs (progn (incf i) fs) (subseq v 0 1)))))
      (format nil "~{~a~}" elements))))

(defun mark-time ()
  "Mark the start of a time span. Returns the mark."
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun elapsed-time (start-time)
  "Computes elapsed time between the START-TIME (the mark from MARK-TIME) and now.
Returns the time span in floating-point seconds."
  (- (/ (get-internal-real-time) internal-time-units-per-second)
     start-time))

(defun universal-time-to-unix-time (&optional universal-time)
  "Converts UNIVERSAL-TIME to unix time. If you don't provide a universal time,
this function returns the current unix time.

Unix time is the number of seconds elapsed since the epoch, January 1, 1970 at
00:00:00 UTC.

Universal time is the number of seconds elapsed since January 1, 1900 at
00:00:00 UTC
"
  (let ((universal-time (or universal-time (get-universal-time))))
    (- universal-time *unix-epoch*)))

(defun unix-time-to-universal-time (&optional unix-time)
  "Converts UNIX-TIME to universal time. If you don't provide UNIX-TIME,
this function returns the current universal time, as an integer.

Unix time is the number of seconds elapsed since the epoch, January 1, 1970 at
00:00:00 UTC.

Universal time is the number of seconds elapsed since January 1, 1900 at
00:00:00 UTC.
"
  (let ((unix-time (or unix-time (universal-time-to-unix-time))))
    (+ unix-time *unix-epoch*)))

(defun current-unix-time ()
  "Returns the current unix time, as an integer. Unix time is the number of
seconds elapsed since the epoch, January 1, 1970 at 00:00:00 UTC."
  (universal-time-to-unix-time (get-universal-time)))
