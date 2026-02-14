;;; parsector.asd - Parsector library system definition

;;; Copyright 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; Copyright 2026 Brian O'Reilly <fade@deepsky.com>
;;; BSD-3-Clause

(asdf:defsystem #:parsector
  :class :package-inferred-system
  :description "Parser combinator library"
  :author ("Samuel Hunter" "Brian O'Reilly <fade@deepsky.com>")
  :license "BSD 3-Clause"
  :version "0.1.0"

  :homepage "https://github.com/fade/parsector"
  :source-control (:git "https://github.com/fade/parsector.git")
  :bug-tracker "https://github.com/fade/parsector/issues"

  :depends-on (#:parsector)
  :in-order-to ((asdf:test-op (asdf:test-op #:parsector/test/all))))
