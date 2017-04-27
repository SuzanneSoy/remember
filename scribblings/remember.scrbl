#lang scribble/manual
@require[@for-label[remember
                    racket/base]]

@title{Remember: storage for macros which is persistant across compilations}
@author{Georges Dupéron}

@defmodule[remember]

This library is implemented using literate programming. The
implementation details are presented in 
@other-doc['(lib "remember/remember-implementation.hl.rkt")].

This module allows macros to remember some values across
compilations. Values are grouped by @racket[_category], so
that multiple macros can use this facility without
interfering with each other. The @racket[_category] is
simply a symbol given when remembering the value.

The list of all remembered values for a given 
@racket[_category] is returned by @racket[get-remembered],
and it is possible to check if a single value has been
remembered using @racket[remembered?].

Values are loaded from files using 
@racket[remember-input-file] and @racket[remember-io-file].
An output file can be set with 
@racket[remember-output-file] and 
@racket[remember-io-file].

When an output file has been declared, new values passed to
@racket[remember-write!] are marked as 
@racket[remembered-or-written?] and appended to that file
(more precisely, the expression 
@racket[(remembered! _category _value)] is appended to the
file, followed by a newline).

When initially created by the user, the output file should
contain the code below, which will be followed by the
automatically-generated 
@racket[(remembered! _category _value)] statements:

@codeblock[#:keep-lang-line? #t]|{
            #lang racket
            (require remember)}|

The @racket[remembered!] macro indicates an
already-remembered value, and is typically used inside input
files. The @racket[for-syntax] function 
@racket[remembered-add!] can also be used instead, to mark a
value as @racket[remembered?] without adding it to any file
(this can be useful for values which should implicitly be
remembered).

@defproc[#:kind "for-syntax procedure"
         (get-remembered [category symbol?]) list?]{
 Returns a list of all values that have been remembered for
 the given @racket[category] (i.e. all values passed as the
 second argument to @racket[remembered-add!], 
 @racket[remember-write!] or @racket[remembered!], with the given
 category as the first argument).}

@defproc[#:kind "for-syntax procedure"
         (remembered-add! [category symbol?] [value any/c]) void?]{
 Marks the given @racket[value] as remembered in the given 
 @racket[category]. If the same value is remembered twice
 for the same category, the second occurrence is ignored
 (i.e. values are stored in a distinct @racket[set] for each
 category).
 
 This @racket[for-syntax] procedure is called by the 
 @racket[remembered!] macro, but can also be executed on its
 own.}

@defproc[#:kind "for-syntax procedure"
         (remembered? [category symbol?] [value any/c]) boolean?]{
 Checks whether the given @racket[value] has already been
 added to the set of remembered values for the given 
 @racket[category].}

@defproc[#:kind "for-syntax procedure"
         (remembered-or-written? [category symbol?] [value any/c]) boolean?]{
 Checks whether the given @racket[value] has already been
 added to the set of remembered values for the given 
 @racket[category], or if it was freshly written to a file
 during the current expansion.}

@defproc[#:kind "for-syntax procedure"
         (remember-write! [category symbol?] [value any/c]) void?]{
 Adds the given @racket[value] to the current 
 @racket[remember-output-file] for the given category. More
 precisely, the expression 
 @racket[(remembered! category value)] is appended to the
 file, followed by a newline.
 
 If the value is already @racket[remembered-or-written?],
 then the file is left unchanged, i.e. two or more calls to
 @racket[remember-write!] with the same @racket[category]
 and @racket[value] will only append an expression to the
 file the first time.
 
 The value is also added to the set of 
 @racket[remembered-or-written?] values, so that subsequent
 calls to @racket[remembered-or-written?] return 
 @racket[#t] for that category and value. Calls to 
 @racket[remembered?] will be unaffected, and will still
 return @racket[#f]. If some declarations are created by a
 library based on the @racket[get-remembered] set, it is
 therefore possible to check whether a value was already
 present, or if it was added by a subsequent 
 @racket[remember-write!].}

@defproc[#:kind "for-syntax procedure"
         (remembered-error! [category symbol] [stx-value syntax?]) void?]{
 Produces a delayed error indicating that this value has
 not been remembered, but was added to the output file.
 
 This procedure just triggers the error, and is not
 concerned with actually adding the value to the output
 file.
 
 The error is added in a lifted declaration which is
 inserted at the end of the current module, using 
 @racket[syntax-local-lift-module-end-declaration]. It
 should therefore be triggered only when the compilation
 reaches the end of the file, if no other error was raised
 before.
 
 This allows as many @racket[remembered-error!] errors as
 possible to be accumulated; all of these are then shown
 when the file is fully expanded. The goal is to be able to
 add all values to the output file in a single run, instead
 of aborting after each value which is not remembered. This
 would otherwise require recompiling the program once for
 each value which is not initially remembered.

 TODO: it would be nice to factor out the delayed error
 mechanism into a separate package, so that multiple
 libraries can add errors, and all of them get reported,
 without one preventing the others from executing. This
 function would likely keep the same signature, and just
 delegate to the delayed-error library.}

@defparam[disable-remember-immediate-error disable? boolean? #:value #f]{
 The @racket[disable-remember-immediate-error] parameter allows code to
 temporarily prevent @racket[remembered-error!] from lifting a delayed error.
 This can be useful for example when calling @racket[remembered-error!] from a
 context where @racket[(syntax-local-lift-context)] is @racket[#false], e.g.
 outside of the expansion of a macro, but within a @racket[begin-for-syntax]
 block.

 The error is still put aside, so that if a delayed error was triggered by
 another call to @racket[remembered-error!], the error will still be included
 with the other delayed errors. If no delayed error is triggered during
 macro-expansion, the error that was put aside will be ignored. To prevent
 this from happening, call @racket[lift-maybe-delayed-errors] within a context
 where lifts are possible.}

@defproc[(lift-maybe-delayed-errors) void?]{
 Uses @racket[syntax-local-lift-module-end-declaration] or
 @racket[syntax-local-lift-expression], depending on the context, to lift an
 expression which will trigger delayed errors, if any. If no delayed errors
 have been recorded by @racket[remembered-error!] when the lifted form is
 executed, then nothing will happen and expansion will proceed.

 Note that when @racket[(syntax-transforming-module-expression?)] returns
 @racket[#false], @racket[syntax-local-lift-expression] is used. The lifted
 form is then run as part of the current expansion pass, before the contents of
 any @racket[let] forms are expanded. This means that calls to
 @racket[remembered-error!] must not happen within the expansion of nested
 @racket[let] forms (with respect to the @racket[let] form being expanded (if
 any) when @racket[lift-maybe-delayed-errors] is called), as they would add
 delayed errors too late, i.e. after the lifted form got executed.}

@defform[(remember-input-file name)
         #:grammar ([name string?])]{
 The file is loaded with @racket[require], but no
 identifier is imported from that module. Instead, 
 @racket[remembered?] relies on its internal mutable 
 @racket[for-syntax] hash table which stores remembered
 values associated to their category.
 
 @racket[remembered-values]. Values are added to the hash
 via the @racket[remembered!] macro. The @racket[name] file
 should therefore @racket[require] the 
 @racketmodname[remember] library, and contain a number of
 calls to @racket[remembered!], each adding a new value to
 the mutable hash.}

@deftogether[
 (@defform*[((remember-output-file)
             (remember-output-file name))
           #:grammar ([name (or/c string? false?)])]
  @defproc*[#:kind "for-syntax parameter"
            #:link-target? #f 
            ([(remember-output-file) (or/c string? false?)]
             [(remember-output-file [name (or/c string? false?)]) void?])]
   )]{
 Indicates that new values added via 
 @racket[remember-write!] should be appended to the file 
 @racket[name]. More precisely, the expression 
 @racket[(remembered! _category _value)] is appended to the
 file, followed by a newline.
 
 Note that if the @racket[_value] given to 
 @racket[remember-write!] is already registered in an input
 file with @racket[remembered!] for the same category, it
 will not be appended to the output file.
 
 For now there can only be one @racket[output] file at the
 same time, any call to @racket[remember-output-file]
 overrides the setting from previous calls. Future versions
 of this library may offer the possibility to specify an
 output file per @racket[_category].

 The special value @racket[#f] indicates that there is no
 output file, in which case @racket[remember-write!] simply
 marks the @racket[value] as 
 @racket[remembered-or-written?] for that category, without
 altering any file.
 
 This identifier exists both as a macro and a for-syntax
 parameter. When called without any argument, it expands to
 (for the macro) or returns (for the for-syntax parameter)
 the last value set using either the macro or by passing an
 argument to the for-syntax parameter.}

@defparam[remember-output-file-parameter output-file
          (or/c path-string? false?)
          #:value #f]{
 This for-syntax parameter that new values added via @racket[remember-write!]
 should be appended to the file whose name is stored within the parameter.

 The @racket[remember-output-file] macro simply sets this parameter.}

@defform[(remember-io-file name)
         #:grammar ([name string?])]{
 Indicates that calls to @racket[remembered!] in this file
 should be taken into account, and that new values added
 with @racket[remember-write!] should be appended to this
 file.
 
 It is equivalent to:
 @racketblock[(remember-input-file name)
              (remember-output-file name)]}

@defform[(remembered! category value)
         #:grammar ([category identifier?])]{
 Marks the given @racket[value] as remembered in the given 
 @racket[category]. If the same value is remembered twice
 for the same category, the second occurrence is ignored
 (i.e. values are stored in a distinct @racket[set] for each
 category).
 
 Calls to this macro are usually present in an input file
 loaded with @racket[remember-input-file] or 
 @racket[remember-io-file], but can also be inserted in the
 main file or any other file loaded with @racket[require].}