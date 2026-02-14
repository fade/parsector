;;; test/all.lisp - Load and run all Parsector test suites

;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(uiop:define-package #:parsector/test/all
  (:use #:cl)
  (:import-from #:parsector/test/package)
  (:import-from #:parsector/test/json)
  (:import-from #:parsector/test/literals)
  (:import-from #:parsector/test/parsec)
  (:import-from #:parsector/test/m3u)
  (:import-from #:parsector/test/tiny-c))

(in-package #:parsector/test/all)
