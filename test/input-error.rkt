#lang racket
(require remember)
(remembered! foo-error (1 2 3))
(remembered! foo-error (1 2 3 4))
(remembered! foo-error (1 2 3 5))
(define + 'wrong)
(provide +)