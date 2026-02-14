;;; benchmark.lisp - Benchmarking harness

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(require :alexandria)
(require :cl-json)
(require :parsector/examples/json)

(defpackage #:parsector/stats/benchmark
  (:use #:cl #:parsector/examples/json)
  (:export #:benchmark))

(in-package #:parsector/stats/benchmark)



(defparameter +large-json-path+
  (merge-pathnames #P"stats/large.json"
                   (asdf:system-source-directory :parsector)))

(defun benchmark-stream-decoder (decoder)
  "Measure the time a decoder reads a jarge json stream payload many times."
  (with-open-file (stream +large-json-path+)
    (time
      (dotimes (n 100)
        (funcall decoder stream)
        (file-position stream 0)))))

(defun benchmark-string-decoder (decoder)
  "Measure the time a decoder reads a large json string payload many times."
  (let ((payload (alexandria:read-file-into-string +large-json-path+)))
    (time
      (dotimes (n 100) (funcall decoder payload)))))

(defun benchmark ()
  (asdf:oos 'asdf:load-op :parsector :force t)
  (asdf:oos 'asdf:load-op :parsector/examples/json :force t)

  (format t "===READING FROM STREAM~%")
  (format t "PARSECTOR:~%")
  (benchmark-stream-decoder #'decode-json)

  (format t "~%~%~%CL-JSON:~%")
  (benchmark-stream-decoder #'cl-json:decode-json)

  (format t "~%~%~%===READING FROM STRING===~%")
  (format t "PARSECTOR:~%")
  (benchmark-string-decoder #'decode-json-from-string)

  (format t "~%~%~%Cl-JSON:~%")
  (benchmark-string-decoder #'cl-json:decode-json-from-string))

(benchmark)
