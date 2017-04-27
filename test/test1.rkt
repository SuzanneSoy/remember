#lang racket
(require remember
         rackunit
         (submod "../remember-implementation.hl.rkt" private))
(remember-input-file "input1.rkt")
(begin-for-syntax
  (require rackunit)
  (define secs (current-seconds))
  (remembered-add-written! 'foo `(1 2 3 secs))
  (check-false (remembered? 'foo `(1 2 3 secs)))
  (check-true (remembered-or-written? 'foo `(1 2 3 secs))))
;; check that no identifiers were imported from "input1.rkt".
(check-not-equal? + 'wrong)