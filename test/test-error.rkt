#lang racket

(require remember
         rackunit)
(remember-input-file "input-error.rkt")
(define-syntax (test-rem stx)
  (syntax-case stx ()
    [(_ val)
     (let ([v (syntax-e #'val)])
       (unless (remembered? 'err-category v)
         (remembered-error! 'err-category #'val)))
     #'(void)]))

(test-rem one)
(test-rem two)
(check-equal? (+ 1 2) 3)
(test-rem three)
(test-rem four)