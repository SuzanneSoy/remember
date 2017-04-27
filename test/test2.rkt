#lang racket
(require remember)
(remember-io-file "io2.rkt")
(begin-for-syntax
  (require rackunit)
  ;; Manually check for an error the first time this
  ;; file is compiled after emptying io2.rkt
  (check-true (remembered? 'bar '(1 2 3 xyz)))
  (remember-write! 'bar '(1 2 3 xyz)))