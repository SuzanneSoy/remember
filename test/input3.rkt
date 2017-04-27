#lang racket
(require remember)
(remembered! foo3 (1 2 3))
(remembered! foo3 (1 2 3 4))
(remembered! foo3 (1 2 3 5))
(define + 'wrong)
(provide +)