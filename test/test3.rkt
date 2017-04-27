#lang racket
(require remember)
(remember-io-file "input3.rkt")
(begin-for-syntax
  (require rackunit
           racket/set)
  (check set=? (get-remembered 'foo3)
               (set '(1 2 3) '(1 2 3 5) '(1 2 3 4))))