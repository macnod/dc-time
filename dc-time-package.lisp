(defpackage :dc-time
  (:use :cl :dc-ds)
  (:export
    *unix-epoch*
    timestamp-string
    mark-time
    elapsed-time
    universal-time-to-unix-time
    unix-time-to-universal-time
    current-unix-time))
