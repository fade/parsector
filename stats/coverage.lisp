;;; coverage.lisp - Generate coverage statistics

(require :sb-cover)

(defpackage #:parsector/stats/coverage
  (:use #:cl)
  (:export #:report))

(in-package #:parsector/stats/coverage)



(defun report (directory)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :parsector :force t)
  (asdf:oos 'asdf:load-op :parsector :force t)
  (asdf:test-system :parsector)
  (prog1
    (sb-cover:report directory )
    (declaim (optimize (sb-cover:store-coverage-data 0)))))

(report #P"/tmp/parsector-coverage/")
