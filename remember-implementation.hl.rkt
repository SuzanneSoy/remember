#lang hyper-literate racket/base
@(require scribble-enhanced/doc)
@doc-lib-setup

@title[#:style manual-doc-style
       #:tag "remember"
       #:tag-prefix "(lib remember/remember-implementation.hl.rkt)"
       ]{Implementation of Remember}

@(chunks-toc-prefix
  '("(lib remember/remember-implementation.hl.rkt)"))

@(table-of-contents)

@section{@racket[remember]}

This module allows macros to remember some values across
compilations. Values are stored within the 
@tc[remembered-values] hash table, which associates a 
@racket[_category] (a symbol) with a set of values.

@chunk[<remembered-values>
       (begin-for-syntax
         (define remembered-values (make-hash)))]

A second set tracks values which were recently written, but
not initially added via @racket[remembered!] or 
@racket[remembered-add!].

@chunk[<remembered-values>
       (begin-for-syntax
         (define written-values (make-hash)))]

The user can specify input files from which remembered
values are loaded, and optionally an output file to which
new, not-yet-remembered values will be appended:

@CHUNK[<remember-file>
       (define-for-syntax remember-output-file-parameter
         (make-parameter #f (or? path-string? false?)))

       (define-syntax (remember-output-file stx)
         (syntax-case stx ()
           [(_ new-value)
            (string? (syntax-e #'new-value))
            (begin (remember-output-file-parameter (syntax-e #'new-value))
                   #'(void))]
           [(_)
            (quasisyntax/loc stx remember-output-file-parameter)]))
       
       (define-syntax (remember-input-file stx)
         (syntax-case stx ()
           [(_ name)
            (string? (syntax-e #'name))
            #'(require (only-in name))]))
       
       (define-syntax-rule (remember-io-file name)
         (begin (remember-input-file name)
                (remember-output-file name)))]

@CHUNK[<remember>
       (define-syntax-rule (remembered! category value)
         (begin-for-syntax
           (remembered-add! 'category 'value)))
       
       (define-for-syntax writable?
         (disjoin number?
                  string?
                  symbol?
                  char?
                  null?
                  (λ (v) (and (pair? v)
                              (writable? (car v))
                              (writable? (cdr v))))
                  (λ (v) (and (vector? v)
                              (andmap writable? (vector->list v))))))

       (define-for-syntax (remembered-add! category value)
         (unless (writable? value)
           (error "Value to remember does not seem to be safely writable:"
                  value))
         (unless (symbol? category)
           (error (format "The category was not a symbol, when remembering ~a:"
                          value)
                  category))
         (hash-update! remembered-values
                       category
                       (λ (s) (set-add s value))
                       set))

       (define-for-syntax (remembered-add-written! category value)
         (unless (writable? value)
           (error "Value to remember does not seem to be safely writable:"
                  value))
         (unless (symbol? category)
           (error (format "The category was not a symbol, when remembering ~a:"
                          value)
                  category))
         (hash-update! written-values
                       category
                       (λ (s) (set-add s value))
                       set))
       
       (define-for-syntax (remembered? category value)
         (unless (writable? value)
           (error "Value to remember does not seem to be safely writable:"
                  value))
         (set-member? (hash-ref remembered-values category set) value))
       
       (define-for-syntax (written? category value)
         (unless (writable? value)
           (error "Value to remember does not seem to be safely writable:"
                  value))
         (set-member? (hash-ref written-values category set) value))

       (define-for-syntax (remembered-or-written? category value)
         (or (remembered? category value)
             (written? category value)))
       
       (define-for-syntax (remember-write! category value)
         (unless (writable? value)
           (error "Value to remember does not seem to be safely writable:"
                  value))
         (unless (or (remembered? category value)
                     (written? category value))
           (when (remember-output-file-parameter)
             (with-output-file [port (remember-output-file-parameter)]
               #:exists 'append
               (writeln (list 'remembered! category value)
                        port)))
           (remembered-add-written! category value)))]

@chunk[<delayed-errors>
       (begin-for-syntax
         (define remember-errors-list '())
         (define remember-lifted-error #f))]

@chunk[<error>
       (define-for-syntax (remembered-error! category
                                             stx-value
                                             [stx-errs (list stx-value)])
         (set! remember-errors-list
               (cons (list category stx-value stx-errs) remember-errors-list))

         (unless (disable-remember-immediate-error)
           (if (not (syntax-local-lift-context))
               ;; Trigger the error right now
               (remember-all-hard-error)
               ;; Lift a delayed error, which will be triggered later on
               (lift-maybe-delayed-errors))))
       
       (define-for-syntax (remembered-add-error! category stx-value)
         (remembered-add! category (syntax-e stx-value))
         (remembered-error! category stx-value))]

@CHUNK[<remember-all-hard-error>
       ;; These two functions allow us to wait around 1000 levels of nested
       ;; macro-expansion before triggering the error.
       ;; If the error is triggered immediately when the lifted statements are
       ;; added at the end of the module, then it can get executed before macros
       ;; used in the righ-hand side of a (define …) are expanded, for example.
       ;; Since these macros may need to remember more values, it's better to
       ;; wait until they are all expanded.
       ;; The number 1000 above in #`(delay-remember-all-hard-error1 1000) is
       ;; arbitrary, but should be enough for most practical purposes, worst
       ;; case the file would require a few more compilations to settle.
       (define-syntax (delay-remember-all-hard-error1 stx)
         (syntax-case stx ()
           [(_ n)
            (number? (syntax-e #'n))
            (if (> (syntax-e #'n) 0)
                #`(let ()
                    (define blob
                      (delay-remember-all-hard-error2 #,(- (syntax-e #'n) 1)))
                    (void))
                (begin (syntax-local-lift-module-end-declaration
                        #`(remember-all-hard-error-macro))
                       #'(void)))]))

       (define-syntax (delay-remember-all-hard-error2 stx)
         (syntax-case stx ()
           [(_ n)
            (number? (syntax-e #'n))
            (begin
              (syntax-local-lift-module-end-declaration
               #'(delay-remember-all-hard-error1 n))
              #'n)]))
       
       (define-for-syntax (remember-all-hard-error)
         (define remember-errors-list-orig remember-errors-list)
         (set! remember-errors-list '())
         (unless (empty? remember-errors-list-orig)
           (raise-syntax-error
            'remember
            (format (~a "The values ~a were not remembered."
                        " Some of them may have been added to the"
                        " appropriate list automatically."
                        " Please recompile this file now.")
                    (string-join (remove-duplicates
                                  (reverse
                                   (stx-map (compose ~a syntax->datum)
                                            (map cadr
                                                 remember-errors-list-orig))))
                                 ", "))
            #f
            #f
            (remove-duplicates
             (append-map caddr remember-errors-list-orig)
             #:key (λ (e)
                     (cons (syntax->datum e)
                           (build-source-location-list e)))))))
       (define-syntax (remember-all-hard-error-macro stx)
         (remember-all-hard-error)
         #'(void))]

The @racket[disable-remember-immediate-error] parameter allows code to
temporarily prevent @racket[remembered-error!] from lifting a delayed error.
This can be useful for example when calling @racket[remembered-error!] from a
context where @racket[(syntax-local-lift-context)] is @racket[#false], e.g.
outside of the expansion of a macro, but within a @racket[begin-for-syntax]
block.

@chunk[<disable-remember-errors>
       (define-for-syntax disable-remember-immediate-error (make-parameter #f))]

The error is still put aside, so that if a delayed error was triggered by
another call to @racket[remembered-error!], the error will still be included
with the other delayed errors. If no delayed error is triggered during
macro-expansion, the error that was put aside will be ignored. To prevent
that, the user can call @racket[lift-maybe-delayed-errors] within a context
where lifts are possible.

@chunk[<lift-maybe-delayed-errors>
       (define-for-syntax (lift-maybe-delayed-errors)
         (if (syntax-transforming-module-expression?)
             ;; Lift a delayed error, attempting to allow several (1000) levels
             ;; of nested let blocks to expand before pulling the alarm signal.
             (unless remember-lifted-error
               (set! remember-lifted-error #t)
               (syntax-local-lift-module-end-declaration
                #`(delay-remember-all-hard-error1 1000)))
             ;; Lift a delayed error, which will be triggered after the current
             ;; expansion pass (i.e. before the contents of any let form is
             ;; expanded).
             (syntax-local-lift-expression
              #`(remember-all-hard-error-macro))))]


@CHUNK[<get-remembered>
       (define-for-syntax (get-remembered category)
         (hash-ref remembered-values category set))]

@chunk[<provide>
       (begin-for-syntax
         (provide get-remembered
                  remembered-add!
                  remembered?
                  remembered-or-written?
                  remember-write!
                  remembered-error!
                  remember-output-file-parameter
                  disable-remember-immediate-error
                  lift-maybe-delayed-errors))
       (provide remember-input-file
                remember-output-file
                remember-io-file
                remembered!)

       (module+ private
         (begin-for-syntax
           (provide remembered-add-written!)))]

@; TODO: circumvents bug https://github.com/racket/scribble/issues/44
@(require racket/require)
@chunk[<*>
       (require mzlib/etc
                ;; TODO: circumvent https://github.com/racket/scribble/issues/44
                racket/require
                (subtract-in phc-toolkit/untyped syntax/stx)
                syntax/stx
                (for-syntax racket/base
                            racket/function
                            racket/bool
                            racket/set
                            racket/list
                            mzlib/etc
                            ;;TODO: https://github.com/racket/scribble/issues/44
                            (subtract-in phc-toolkit/untyped
                                         syntax/stx)
                            syntax/stx
                            syntax/srcloc
                            racket/string
                            racket/format))
       <provide>
       <remembered-values>
       <remember-file>
       <remember>
       <get-remembered>
       <delayed-errors>
       <disable-remember-errors>
       <lift-maybe-delayed-errors>
       <remember-all-hard-error>
       <error>]