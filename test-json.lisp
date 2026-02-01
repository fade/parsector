;;; test-json.lisp - Test suite for the Parsnip JSON example

(defpackage #:xyz.shunter.parsnip.test-json
  (:use #:cl
        #:xyz.shunter.parsnip
        #:xyz.shunter.parsnip.examples.json)
  (:local-nicknames (#:t #:parachute)))

(in-package #:xyz.shunter.parsnip.test-json)

(t:define-test json-literals
  (t:is eq :true (decode-json-from-string "true"))
  (t:is eq :false (decode-json-from-string "false"))
  (t:is eq :null (decode-json-from-string "null"))
  (t:is eq :true (decode-json-from-string " true ")))

(t:define-test json-numbers
  (t:is = 42 (decode-json-from-string "42"))
  (t:is = -123 (decode-json-from-string "-123"))
  (t:is = 3.14 (decode-json-from-string "3.14"))
  (t:is = -0.5 (decode-json-from-string "-0.5"))
  (t:is = 1.0e3 (decode-json-from-string "1.0e3"))
  (t:is = 1.0e+3 (decode-json-from-string "1.0e+3")))

(t:define-test json-strings
  (t:is string= "hello" (decode-json-from-string "\"hello\""))
  (t:is string= "hello world" (decode-json-from-string "\"hello world\""))
  (t:is string= "with\"quotes" (decode-json-from-string "\"with\\\\\"quotes\""))
  (t:is string= (string #\Newline) (decode-json-from-string "\"\\n\"")))

(t:define-test json-arrays
  (t:is equalp #() (decode-json-from-string "[]"))
  (t:is equalp #(1 2 3) (decode-json-from-string "[1, 2, 3]"))
  (t:is equalp #("a" :true 42) (decode-json-from-string "[\"a\", true, 42]")))

(t:define-test json-objects
  (t:is equalp '() (decode-json-from-string "{}"))
  (t:is equalp '(("a" . 1)) (decode-json-from-string "{\"a\": 1}"))
  (t:is equalp '(("a" . 1) ("b" . "c")) (decode-json-from-string "{\"a\": 1, \"b\": \"c\"}")))

(t:define-test json-nested-structures
  (t:is equalp '(("a" . #(1 2)) ("b" . :true))
        (decode-json-from-string "{\"a\": [1, 2], \"b\": true}"))
  (t:is equalp #((("a" . 1)))
        (decode-json-from-string "[{\"a\": 1}]")))
