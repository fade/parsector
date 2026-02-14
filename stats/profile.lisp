;;; profile.lisp - Profiling harness

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; BSD-3-Clause

(require :sb-sprof)
(require :parsector/examples/json)

(uiop:define-package #:parsector/stats/profile
  (:use #:cl #:parsector/examples/json))

(in-package #:parsector/stats/profile)



(defparameter +large-json-path+
  (merge-pathnames #P"stats/large.json"
                   (asdf:system-source-directory :parsector)))

(defun process-large-file ()
  (with-open-file (stream +large-json-path+)
    (decode-json stream))
  (values))

(sb-sprof:with-profiling (:max-samples 1000 :report :flat :loop t)
  (process-large-file))
