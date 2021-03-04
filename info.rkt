#lang info
(define collection "remember")
(define deps '("base"
               "rackunit-lib"
               "compatibility-lib"
               "scribble-lib"
               "typed-racket-lib"
               "phc-toolkit"
               "hyper-literate"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-doc"
                     "scribble-enhanced"))
(define scribblings '(("scribblings/remember.scrbl" ())
                      ("remember-implementation.hl.rkt" () (omit-start))))
(define compile-omit-paths '("test/test-error.rkt"))
(define test-omit-paths '("test/test-error.rkt"))
(define pkg-desc (string-append "Compile-time memoize across compilations."
                                " Writes values to a file, so that they will"
				" be remembered during the next compilation."))
(define version "0.9")
(define pkg-authors '(|Suzanne Soy|))
