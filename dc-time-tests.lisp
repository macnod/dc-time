;; Tests for the dc-time package

(require :asdf)
(require :dc-ds)
(require :fiveam)
(require :uiop)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :dc-time)

(defpackage :dc-time-tests (:use :cl :fiveam :dc-time :dc-ds))

(in-package :dc-time-tests)

(def-suite dc-time-suite :description "FiveAM tests for the dc-time package")

(in-suite dc-time-suite)

(test *unix-epoch*
  (is (= *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))
    "*unix-epoch* matches 1970-01-01 00:00:00 UTC")
  (is (= *unix-epoch* 2208988800)
    "*unix-epoch* matches 2208988800 seconds since 1900-01-01 00:00:00 UTC"))

(test timestamp-string-basics
  (let ((now (get-universal-time)))
    (is (stringp (timestamp-string :universal-time now))
      "timestamp-string returns a string")
    (is (> (length (timestamp-string :universal-time now)) 10)
      "timestamp-string returns a non-trivial string")))

(test timestamp-string-format
  (let* ((fixed-time (encode-universal-time 30 59 23 15 6 2023 0))
         (result (timestamp-string :universal-time fixed-time)))
    (is (equal result "2023-06-15T23:59:30")
      "Default format produces expected string"))
  ;; Test literal characters
  (is (cl-ppcre:scan "Hello 1970"
        (timestamp-string :format "Hello %Y" :universal-time *unix-epoch*))
    "Literal characters in format are preserved"))

(test timestamp-string-padding
  (let ((fixed-time (encode-universal-time 5 4 3 2 1 2023 0)))
    (is (equal "2023-01-02T03:04:05"
          (timestamp-string :universal-time fixed-time))
      "Timestamp components are zero-padded")))

(test mark-time-elapsed-time
  (let ((start (mark-time)))
    (sleep 0.1)
    (let ((elapsed (elapsed-time start)))
      (is (> elapsed 0.05) "Elapsed time is greater than sleep time")
      (is (< elapsed 1.0) "Elapsed time is less than 1 second"))))

(test universal-time-to-unix-time
  ;; Test with known value
  (is (= 0 (universal-time-to-unix-time *unix-epoch*))
    "Unix time for *unix-epoch* is 0")
  ;; Test current time
  (let ((ut (get-universal-time))
        (unix (current-unix-time)))
    (is (= unix (universal-time-to-unix-time ut))
      "Current universal time converts to current unix time"))
  ;; Test non-zero time
  (let ((test-time (encode-universal-time 0 0 0 2 1 1970 0)))
    (is (= 86400 (universal-time-to-unix-time test-time))
      "Unix time for 1970-01-02 00:00:00 UTC is 86400")))

(test unix-time-to-universal-time
  ;; Test round trip
  (is (= *unix-epoch* (unix-time-to-universal-time 0))
    "Universal time for unix time 0 is *unix-epoch*")
  ;; Test current time round trip
  (let ((unix (current-unix-time)))
    (is (= (get-universal-time)
           (unix-time-to-universal-time unix)
           (unix-time-to-universal-time))
      "Current unix time converts back to current universal time"))
  ;; Test non-zero time
  (is (= (encode-universal-time 0 0 0 2 1 1970 0)
         (unix-time-to-universal-time 86400))
    "Universal time for unix time 86400 is 1970-01-02 00:00:00 UTC"))

(test current-unix-time
  (let ((unix (current-unix-time)))
    (is (integerp unix) "Current unix time is an integer")
    (is (> unix 1700000000) "Current unix time is after 2023")
    ;; Should be close to universal time conversion
    (let ((ut-diff (- (get-universal-time) *unix-epoch*)))
      (is (< (abs (- unix ut-diff)) 2)
        "Current unix time is close to universal time minus epoch"))))

(test time-conversions-roundtrip
  ;; universal -> unix -> universal
  (let ((ut (get-universal-time)))
    (is (= ut (unix-time-to-universal-time (universal-time-to-unix-time ut)))
      "Universal time round-trips through unix time"))
  ;; unix -> universal -> unix
  (let ((unix (current-unix-time)))
    (is (= unix (universal-time-to-unix-time (unix-time-to-universal-time unix)))
      "Unix time round-trips through universal time")))

(test timestamp-string-timezone
  (let* ((ut (get-universal-time))
         (result0 (timestamp-string :universal-time ut :timezone 0))
         (result1 (timestamp-string :universal-time ut :timezone -8)))
    ;; Just verify timezone param is accepted
    (is (stringp result0) "timestamp-string with timezone 0 returns string")
    (is (stringp result1) "timestamp-string with timezone -8 returns string")))

(test timestamp-string-edge-cases
  ;; Year 1900 edge case
  (let ((y2k1900 (encode-universal-time 0 0 0 1 1 1900 0)))
    (is (cl-ppcre:scan "1900" (timestamp-string :universal-time y2k1900))
      "Year 1900 is handled correctly"))
  ;; Max values
  (let ((max-time (encode-universal-time 59 59 23 31 12 9999 0)))
    (is (equal "9999-12-31T23:59:59"
               (timestamp-string :universal-time max-time))
      "Max date/time is handled correctly"))
  ;; Invalid format chars are preserved
  (is (= 3 (count #\X (timestamp-string :format "XXX%Y")))
    "Invalid format characters are preserved in output"))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
