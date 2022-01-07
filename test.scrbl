#lang scribble/manual

@;====================================================================================================

@(require
  scribble/core
  scribble/eval
  racket racket/list
  (only-in racket/block block)
  "test.rkt"
  (for-label "test.rkt" racket racket/list (only-in racket/block block))
  (for-syntax racket))

@(define-syntax-rule (Interaction x ...)
  (interaction #:eval (make-base-eval #:lang '(begin (require racket "test.rkt"))) x ...))

@(define lb linebreak)
@(define nb nonbreaking)
@; ignore is a syntax such as to prevent arguments to be evaluated.
@(define-syntax-rule (ignore x ...) (void))
@; Below syntaxes are used such as to allow keyword arguments
@; without explicitly mentioning them in the definitions.
@(define-syntax-rule (nbsl x ...) (nb (seclink    x ...)))
@(define-syntax-rule (nbsr x ...) (nb (secref     x ...)))
@(define-syntax-rule (nbhl x ...) (nb (hyperlink  x ...)))
@(define-syntax-rule (nber x ...) (nb (elemref    x ...)))
@(define-syntax-rule (nbrl x ...) (nb (racketlink x ...)))
@(define-syntax-rule (nbr  x ...) (nb (racket     x ...)))
@(define (tt . content) (element 'tt (apply list content)))
@(define(minus) (tt "-"))
@(define(-?) (element "roman" ?-))
@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))
@(define (expt-1) @↑{@(minus)1})
@(define ↑ superscript)
@(define ↓ subscript)
@(define-syntax-rule (Tabular ((e ...) ...) . rest) (tabular (list (list e ...) ...) . rest))
@(define (roman . x) (element 'roman x))
@(define (nbtt x) (nb (ttblack x)))
@(define test-arg   (make-parameter "yet to be constructed"))
@(define test-thunk (make-parameter "yet to be constructed"))
@(define test-block (make-parameter "yet to be constructed"))

@(define-syntax-rule (Tabular-with-linebreaks ((e ...) ... (le ...)) . rest)
  (Tabular (((list e (lb) (hspace 1)) ...) ... (le ...)) . rest))

@(define (make-color-style color)
  (define prop:color (color-property color))
  (define color-style (style #f (list prop:color)))
  (lambda elems (element 'roman (element color-style elems))))

@(define (make-ttcolor-style color)
  (define prop:color (color-property color))
  (define color-style (style #f (list prop:color)))
  (lambda elems (element 'tt (element color-style elems))))

@(define red       (make-color-style   "red"))
@(define green     (make-color-style   "green"))
@(define black     (make-color-style   "black"))
@(define ttblack   (make-ttcolor-style "black"))
@(define ttred     (make-ttcolor-style "red"))
@(define ttgreen   (make-ttcolor-style "green"))
@(define optional "optional, evaluated, default: ")
@(define opt-proc "optional, default: ")

@(define (excn)
         (seclink "exn-model" #:doc '(lib "scribblings/reference/reference.scrbl") "exception"))

@(define (excns)
         (seclink "exn-model" #:doc '(lib "scribblings/reference/reference.scrbl") "exceptions"))

@title[#:version ""]{A tool for testing}
@author{Jacob J. A. Koot}
@(defmodule test/test #:packages ())
@;@(defmodule "test.rkt" #:packages ())

@section{Test and companions}

A tool for @seclink["test-bang" #:doc '(lib "scribblings/style/style.scrbl")]{testing}:
comparison of computed results with expected ones.

@(Tabular
(("•" @roman{Checks computed values, @seclink["values-model"
             #:doc '(lib "scribblings/reference/reference.scrbl")]{multiple return values} included.})
 ("•" @roman{@seclink["parameter-model"
                      #:doc '(lib "scribblings/reference/reference.scrbl")]{Parameterizes}
             the @nbrl[current-output-port]{current output}
             and @nbrl[current-error-port]{error port} to
             @seclink["stringport" #:doc '(lib "scribblings/reference/reference.scrbl")]{string ports}
             and checks the outputs.})
 ("•" @roman{Catches and checks
             @(excns),
             syntax errors included.})
 ("•" @roman{Each test has its own optional time limit.})
 ("•" @roman{A test inherits @seclink["parameter-model"
             #:doc '(lib "scribblings/reference/reference.scrbl")]{parameters}
             but does not affect those of the calling program.})
 ("•" @roman{Uses a @seclink["custodian-model"
             #:doc '(lib "scribblings/reference/reference.scrbl")]{custodian}
             to kill @seclink["Creating_Threads"
             #:doc '(lib "scribblings/reference/reference.scrbl")]{threads}
             made by a test, to close
             @seclink["i/o" #:doc '(lib "scribblings/guide/guide.scrbl")]{ports}
             opened by a test, etc.})
 ("•" @roman{Double sided @seclink["prompt-model"
             #:doc '(lib "scribblings/reference/reference.scrbl")]{continuation barrier}
             around each test.}))
#:sep (hspace 1) #:row-properties (make-list 7 'top))

@defform[#:kind @seclink["macros" #:doc '(lib "scribblings/guide/guide.scrbl")]{macro}
 (test #,(roman " ")   name            #,(roman "required, evaluated")
           (expr ...)      #,(roman "required, not evaluated")
           expected-values #,(roman "required, evaluated")
  #:output expected-output #,(roman optional (nbr #f))
  #:error  expected-error  #,(roman optional (nbr #f))
  #:exn    exn-expected?   #,(roman optional (elemref "default-exn" "see below"))
  #:time   time-limit      #,(roman optional (nbr (test-time)))
  #:cg?    cg?             #,(roman optional (nbr #f))
  #:check  check-procedure #,(roman optional (nbr test-check)))
  #:contracts
  ((name            #,(tt (hspace 11) (nbr any/c)))
   (expr            #,(tt (hspace 11) (nbr any) (roman ", but not yet evaluated")))
   (expected-values #,(tt             (nbr (or/c (listof any/c) #f))))
   (expected-output #,(tt             (nbr (or/c string? (listof string?) #f))))
   (expected-error  #,(tt (hspace  1) (nbr (or/c string? (listof string?) #f))))
   (exn-expected?   #,(tt (hspace  2) (nbr any/c)))
   (time-limit      #,(tt (hspace  5) (nbr (or/c #f (>/c 0)))))
   (cg?             #,(tt (hspace 12) (nbr any/c)))
   (check-procedure #,(tt             (roman (nbr (procedure-arity-includes/c 14))
                                             " for details see " (nbr test-check)))))]{

@(test-arg   (nbr (expr #,(tt "..."))))
@(test-block (nbr (block expr #,(tt "..."))))
@(test-thunk (nbr (λ () #,(test-block))))

@(lb)The @nbr[name], @(test-arg) and @nbr[expected-values] must be the first three arguments,
in this order. The optional keyword arguments can follow in arbitrary order.
@(test-arg) is the test proper and is not yet evaluated. All other arguments are evaluated
in the order as they appear in the @nbr[test]-call and
are checked to satisfy their contracts.
@nb{The test is} saved for procedure @nbr[test-report], which will evaluate the @nbr[expr]s.
@nb{The @nbr[test]-form returns @seclink["void" #:doc '(lib "scribblings/reference/reference.scrbl")
                                         (tt "#<void>")]}.

@Tabular-with-linebreaks[
 (((nbr name) "Gives a name to the test.")
  (@(test-arg)
   @roman{Forms to be checked. Not yet evaluated.
          They are wrapped in a thunk @(test-thunk)
          within the @seclink["id-model" #:doc '(lib "scribblings/reference/reference.scrbl")]{scope}
          in which the @nbr[test]-form is located,
          not necessarily at top level.
          The thunk will be called as a
          @seclink["threads" #:doc '(lib "scribblings/reference/reference.scrbl")]{thread}
          started by procedure @(racket test-report).})
  ((nbr expected-values)
   @roman{List of the expected values of @(test-block), possibly empty.@(lb)
          @nbr[#f] is short for @(nbr (list (void))),
          indicating that the single value
          @seclink["void" #:doc '(lib "scribblings/reference/reference.scrbl")
                   (tt "#<void>")] is expected.
          When @nbr[test-check] is given for the @nbr[check-procedure],
          computed and expected values are compared with each other with procedure
          @nbr[test-compare?].})
  ((list (nbr expected-output) (lb) (nbr expected-error))
   @roman{Output expected on @(racket current-output-port) cq @racket[current-error-port].
          @nb{These arguments} are @nbrl[normalize-whitespace]{normalized for white@(-?)space}.
          The computed output and error
          will be normalized too before comparison with the expected output and error.})
  ((nbr exn-expected?) (elemtag "default-exn" 
   @roman{@nbr[#f] indicates that no @(excn) is expected.
          Every other value indicates that an @(excn) is expected.
          If the @nbrl[normalize-whitespace]{normalized} @racket[expected-error] is a non-empty string
          the default is @nbr[#t], else @nbr[#f].
          Normally you don't need this argument, but is is included for tests that produce output
          to the @racket[current-error-port] without raising an @(excn),
          in which case @nbr[#f] is the appropriate value for this argument.
          When the test is expected to raise an @(excn) without producing non-white error output,
          the appropriate value is @nbr[#t].}))
  ((nbr time-limit)
   @roman{Imposes a real-time limit on the test in milliseconds.
          @(nbr +inf.0) is taken as @(nbr #f), which means no time limit.
          Exceeding the @nbr[time-limit] raises an error,
          which can be an @nbr[expected-error].
          See parameter @(nbr test-time).})
  ((nbr cg?)
   @roman{Coerced to @nbr[(and time-limit cg? #t)]. If true,
          @nbrl[collect-garbage]{garbage} is collected before the test is executed
          in order to reduce time spent on garbage collection within the @nbr[time-limit].})
  ((nbr check-procedure)
   @roman{Procedure that is assumed to check the results of a test.
          @nb{The default is} @nbr[test-check].
          @nb{The @nbr[check-procedure]} is called by @nbr[test-report] after running a test.
          Section ‘@secref["Make check procedure"]’
          shows two examples of how to prepare your own @nbr[check-procedure].}))
 #:sep (hspace 2)
 #:row-properties '(top top top top top top top)]

When tests are @nbrl[test-enable "enabled"] the test is saved for procedure @(nbr test-report).
The test is not yet executed.
The @(nbr expr)s are not yet evaluated.
@(test-arg) must be readable as a datum, though.
A syntactic error in @(test-arg) is caught during expansion
and is expanded to code that raises the error during execution of the test.
The error will be catched by procedure @nbr[test-report] and the associated message
will be passed to the @nbr[check-procedure],
which can compare it with the expected error message, if present.

When tests are @nbrl[test-enable "disabled"] the test is not recorded
and the arguments are not evaluated but nevertheless must pass the expansion phase.
@(test-arg) may have syntactic errors, though, but must be readable as a datum. 

@elemtag["output-string-port"] @elemtag["error-string-port"]

When eventually the tests are run by procedure @nbr[test-report],
for each test a thunk of the form @(test-thunk) is called.
This thunk is wrapped such that @(excns) are caught and
the @racket[current-output-port] and the @racket[current-error-port] are
@seclink["parameters" #:doc '(lib "scribblings/reference/reference.scrbl") "parameterized"]
to two internal
@seclink["stringport" #:doc '(lib "scribblings/reference/reference.scrbl") "output-strings"], say
@elemref["output-string-port"]{@italic{output-string-port}} and
@elemref["error-string-port"]{@italic{error-string-port}}.
The results are checked by the procedure given for argument @nbr[check-procedure],
whose default is @nbr[test-check].
The test is run under a
@seclink["custodian-model"
         #:doc '(lib "scribblings/reference/reference.scrbl")]{custodian} which is
@nbrl[custodian-shutdown-all]{shut down}
after the test has finished or has been terminated in any other way. 

When the evaluation of an @nbr[expr] raises an @(excn)
that is not caught within @(test-block) itself,
the @nbr[exn-message] is @nbrl[display]{displayed} on the
@elemref["error-string-port"]{@italic{error-string-port}}
in@(-?)de@(-?)pend@(-?)ent@(-?)ly of
@seclink["parameterize" #:doc '(lib "scribblings/guide/guide.scrbl") "parameterization"]
of the @nbr[current-error-port] within @(test-arg).
The displayed @nbr[exn-message] does not include @nbrl[srcloc]{source information},
which makes it easier to provide a matching @nbr[expected-error]
(see section @secref["Predict error messages"]).
When procedure @nbr[test-check] reports a failure,
source information of the test is shown, though,
but not in the @nbr[exn-message].
Every @nbrl[raise]{raised} value other than an @nbrl[exn?]{exception} is treated like an @(excn)
and displayed on the @elemref["error-string-port"]{@italic{error-string-port}}.

There is a double sided continuation-barrier around the dynamic extent of a test.
@(test-thunk) is wrapped such that an exit from its dynamic content other than to the continuation
of the block is caught and reported as an error with the @nbrl[exn-message]{message}:@(lb)@(hspace 3)
@(nb (red (italic (tt "continuation application: jump out of dynamic extent of test"))))@(lb)
An attempt to enter a continuation into the block from outside its dynamic extent
is caught and reported as error with the @nbrl[exn-message]{message}:@(lb)@(hspace 3)
@(nb (red (italic (tt "continuation application: attempt to cross a continuation barrier"))))

Examples: all of the following tests pass:

@Interaction[
(test 1 (1) '(1))
(test 2 () #f)
(test 3 ((write (list 1 2 3))) #f #:output "(1 2 3)")
(code:comment " ")
(code:comment #,(black "Treatment of whitespace."))
(code:comment " ")
(test 4 ("  a  b  ") '("a\nb"))
(test 5 ((write   "   ")) #f #:output "\" \"")
(test 6 ((display "   ")) #f #:output #f)
(test 7 ((error "an error")) '() #:error (list " an" "error "))
(code:comment " ")
(code:comment  #,(black "Multiple values and void."))
(code:comment " ")
(test 8 ((values 1 2 3)) '(1 2 3))
(test 9 ((values)) '())
(test 10 () #f)
(test 11 ((display "  ")) #f #:output #f)
(code:comment " ")
(code:comment #,(black
  "A test can refer to a variable yet to be defined, but the variable must be defined"))
(code:comment #,(black "before the test can be executed by procedure " (nbr test-report)
                       ", of course."))
(code:comment " ")
(test 12 ((+ a b)) '(3))
(define a 1)
(define b 2)
(code:comment " ")
(test 13 ((factorial 5)) '(120))
(define (factorial n)
 (if (zero? n) 1 (* n (factorial (sub1 n)))))
(code:comment " ")
(code:comment #,(black (nbr test) " is not required to be at top-level."))
(code:comment " ")
(letrec ((! (λ (n) (if (zero? n) 1 (* n (! (sub1 n)))))))
        (test 14 ((apply values (map ! (range 5)))) '(1 1 2 6 24)))
(code:comment " ")
(code:comment #,(black "It is possible to define macros within a test."))
(code:comment " ")
(test 15 ((define-syntax-rule
           (my-let id val   body ...)
           (let  ((id val)) body ...))
          (my-let a 10 (add1 a)))
         '(11))
(code:comment " ")
(code:comment #,(black "Data containing cycles in their graph structure are compared correctly."))
(code:comment #,(black "They do not give rise to an infinite loop."))
(code:comment #,(black "Below the strings in computed and expected values differ in their"))
(code:comment #,(black "whitespace, but nevertheless are accepted as matching each other."))
(code:comment " ")
(define (make-list-example str)
 (let ((a (mcons 1 str))) (set-mcar! a (box a)) a))
(let ((p (make-list-example  "abc"))
      (q (make-list-example " abc ")))
 (test 16 (p) (list q))
 (printf "~s~n~s~n" p q))
(code:comment " ")
(define (make-vector-example str1 str2)
 (let ((a (vector str1 0 str2))) (vector-set! a 1 a) a))
(let ((p (make-vector-example " a b " " c d "))
      (q (make-vector-example "a   b" "c   d")))
 (test 17 (p) (list q))
 (printf "~s~n~s~n" p q))
(code:comment " ")
(code:comment #,(black  "Catching a syntax error:"))
(code:comment " ")
(test 18 (λ) '() #:error "λ: bad syntax")
(code:comment " ")
(test 19 ((#:foo)) '()
 #:error "#%datum: keyword misused as an expression")
(code:comment " ")
(test 20 ((define-syntax-rule (a x) x) (a)) '()
 #:error "a: use does not match pattern: (a x)")
(code:comment " ")
(code:comment #,(black "Raised non-exceptions are caught as well"))
(code:comment " ")
(test 21 ((raise '(this is caught))) '()
         #:error "(this is caught)")
(code:comment " ")
(test 22 ((raise #\space)) '() #:error #f #:exn #t)
(code:comment " ")
(code:comment #,(black "The following three tests remain within their dynamic extent."))
(code:comment " ")
(test 23 ((example)) #f #:output "billy minny billy")
(code:comment " ")
(define (example)
 (define continuation 'whatever)
 (let/cc cc (set! continuation cc))
 (printf "~s " 'billy)
 (when continuation
  (let ((cc continuation))
   (set! continuation #f) (code:comment #,(black "Avoid infinite loop."))
   (printf "~s " 'minny)
   (cc))))
(code:comment " ")
(test 24 ((let/cc cc (apply cc '(this is ok))
                     (error "we do not get here")))
        '(this is ok))
(code:comment " ")
(let ((n 10) (cont #f))
 (define (set-cont) (let/cc cc (set! cont cc))
                    (printf "~s~n" n))
 (test 25 ((set-cont)
           (when (> n 0) (set! n (sub1 n)) (cont))
           (printf "end"))
       #f #:output "10 9 8 7 6 5 4 3 2 1 0 end"))
(code:comment " ")
(code:comment #,(black "Attempts to cross the double sided continuation barrier."))
(code:comment " ")
(let/cc cc
 (test 26 ((cc 123)) '()
  #:error (list "continuation application:"
                "jump out of dynamic extent of test")))
(code:comment #,(elemtag "test 27" " "))
(test 27 ((let/cc cc (set! continuation cc))) #f)
(test 28 ((continuation)) '()
 #:error (list "continuation application:"
               "attempt to cross a continuation barrier"))
(code:line (define continuation #f) (code:comment #,(black "Will point into "
                                                           (elemref "test 27" "test 27" "."))))
(code:comment " ")
(code:comment #,(black "The " (nbr current-error-port)
                       " receives both explicit output"))
(code:comment #,(black "and output produced by " (excns) "."))
(code:comment " ")
(test 29 ((display "foo  ")
          (eprintf "baz ")
          (values 1 2 3)) '(1 2 3)
         #:output "foo"
         #:error  "baz"
         #:exn    #f)
(code:comment " ")
(test 30 ((display "foo ")
          (eprintf "baz ")
          (error   "bar ")) '()
         #:output  "foo"
         #:error   "baz bar")
(code:comment " ")
(test 31 ((display "Not raising an error." (current-error-port)))
         #f
         #:error "Not raising an error."
         #:exn #f)
(code:comment " ")
(code:comment #,(black "A test does not affect "
                       (seclink "parameters"
                                #:doc '(lib "scribblings/reference/reference.scrbl")
                                "parameters")
                       " of the calling program."))
(code:comment " ")
(define p (make-parameter 0))
(test 32 ((p 1) (p)) '(1))
(test 33 (      (p)) '(0))
(code:comment " ")
(code:comment #,(elemtag "test 34" (black "Cleanup a thread initiated by the test.")))
(code:comment " ")
(test 34
 ((set! thd (thread (λ () (let loop ((n 0)) (loop (add1 n))))))
  (sleep 0.01)) #f)
(define thd #f)
(code:comment " ")
(code:comment #,(black "Check that the thread ininitated by "
                       (elemref "test 34" "test 34")
                       " has been killed."))
(code:comment " ")
(test 35 ((thread-dead? thd)) '(#t))
(code:comment " ")
(code:comment #,(elemtag "test 36" (black "Timer:")))
(code:comment " ")
(define counter 0)
(code:line (define time-limit 5) (code:comment #,(black "milliseconds")))
(test 36
 ((let loop () (set! counter (add1 counter)) (loop))) '()
 #:time time-limit #:cg? #t
 #:error (format
          "test-time: limit of ~s milliseconds exceeded"
          time-limit))
(code:comment #,(black "The test has not yet been executed. "
                       "Hence the " (tt "counter") " still is zero."))
counter
(code:comment " ")
(code:comment #,(black "Sofar tests have been gathered but not yet executed."))
(code:comment #,(black "Procedure " (nbr test-report) " does the tests gathered by macro "
                       (nbr test) "."))
(code:comment " ")
(test-report)
(code:comment " ")
(code:comment #,(black "The counter of "
                       (elemref "test 36" "test 36")
                       ". Available now the test has been executed."))
counter]}

@defproc[
(test* (name any/c)
       (thunk (-> any))
       (expected-values (or/c (listof any/c) #f))
       (#:output expected-output (or/c string? (listof string?)) #f)
       (#:error  expected-error  (or/c string? (listof string?)) #f)
       (#:exn    exn-expected?   any/c (and expected-error #t))
       (#:time   time-limit      (or/c #f (>/c 0)) (test-time))
       (#:cg?    cg?             any/c #f)
       (#:check  check-procedure (procedure-arity-includes/c 14) test-check)) void?]{

Procedure version of macro @nbr[test]. Before being passed to the procedure,
the arguments are evaluated in the order as they appear in the call,
argument @nbr[thunk] included.
Therefore syntax errors in this argument are not delayed to execution time.
Procedure @nbr[test-report] cannot show the source location of tests made with @nbr[test*].
In all other respects same as:

@(inset (racketblock0
(test name
      ((thunk))
      expected-values
     [#:output expected-output
      #:error  expected-error
      #:exn    exn-expected?
      #:time   time-limit
      #:cg?    cg?
      #:check  check-procedure])))}

@defproc[(test-report) void?]{

Runs all @nbrl[test "tests"] in the order they were gathered
and reports details about all failing tests on the
@racket[current-output-port]. The list of gathered tests is cleared.

@Interaction[
(module test racket
 (require test/test)
 (test 'ok1   (1)             '(1))
 (test 'fail1 (1)             '(2))
 (test 'ok2   ((write 'foo))  #f     #:output "foo")
 (test 'fail2 ((write 'foo))  '(foo) #:output "baz")
 (test 'ok3   ((raise 'bar))  '()    #:output #f    #:error "bar")
 (test 'fail3 ((error "bar")) #f     #:output "bar" #:error "oof")
 (test 'ok4   ((raise 123))   '()                   #:error "123")
 (test 'fail4 ((raise ""))    '()    #:exn    #f    #:error #f)
 (test 'ok5   ((eprintf "1")) #f     #:exn    #f    #:error "1")
 (test 'fail5 ((eprintf "1")) #f                    #:error "1")
 (test-report))
(require 'test)]}

@defproc[(test-check
          (name            #,(tt (hspace 11) (nbr any/c)))
          (exprs           #,(tt (hspace 10) (nbr (listof any/c))))
          (expected-values #,(tt             (nbr (listof any/c))))
          (computed-values #,(tt             (nbr (listof any/c))))
          (expected-output #,(tt             (nbr (or/c #f non-empty-string?))))
          (computed-output #,(tt             (nbr (or/c #f non-empty-string?))))
          (expected-error  #,(tt (hspace  1) (nbr (or/c #f non-empty-string?))))
          (computed-error  #,(tt (hspace  1) (nbr (or/c #f non-empty-string?))))
          (exn-expected?   #,(tt (hspace  2) (nbr boolean?)))
          (exn-raised?     #,(tt (hspace  4) (nbr boolean?)))
          (report-port     #,(tt (hspace  4) (nbr (and/c output-port? string-port?))))
          (source          #,(tt (hspace  9) (nbr (or/c path? 'unknown))))
          (line            #,(tt (hspace 11) (nbr (or/c exact-positive-integer? #f))))
          (column          #,(tt (hspace  9) (nbr (or/c exact-nonnegative-integer? #f)))))
         boolean?]{

This is the default for argument @(italic (tt "check-procedure")) of macro @nbr[test].
Procedure @nbr[test-report] processes the recorded tests.
For each test it runs a
@seclink["threads" #:doc '(lib "scribblings/reference/reference.scrbl")]{thread}
with the associated thunk and subsequently calls
the @(italic (tt "check-procedure")) with the results.

The @nbr[name], @nbr[exprs], @nbr[expected-values], @nbr[expected-output] and @racket[expected-error]
are those given to and as @nbrl[normalize-whitespace]{normalized}
for white@(-?)space by macro @nbr[test].

The @nbr[computed-output] and @nbr[computed-error] are strings collected from the
@elemref["output-string-port"]{@italic{output-string-port}} and the 
@elemref["error-string-port"]{@italic{error-string-port}}.
They are @nbrl[normalize-whitespace]{normalized}
for white@(-?)space in the same way as the @nbr[expected-output] and the @nbr[expected-error]
are normalized by macro @nbr[test].

The @nbr[source], @nbr[line] and @nbr[column] specify the location of the test.
@ignore{For a test made with procedure @nbr[test*]
  these arguments are @nbr['unknown], @nbr[#f] and @nbr[#f].}

The @racket[report-port] is a fresh
@seclink["stringport" #:doc '(lib "scribblings/reference/reference.scrbl") "output-string"].
If the test passes, procedure @nbr[test-check] returns @nbr[#t], else
it writes details of unexpected results on the @racket[report-port] and returns @nbr[#f].
Procedure @nbr[test-report] gathers the reports of the failing tests from the
@racket[report-port] and prints them after all tests have been executed.
Equality of @nbr[computed-values] and @nbr[expected-values]
is tested with procedure @nbr[test-compare?].

@Interaction[
(define
 (tstchck name exprs
          expected-values computed-values
          expected-output computed-output
          expected-error  computed-error
          exn-expected? exn-raised?
          report-port source line column)
 (fprintf report-port " ~nname  : ~s~n" name)
 (for-each (λ (x) (fprintf report-port "expr  : ~s~n" x)) exprs)
 (for-each (λ (x) (fprintf report-port "expect: ~s~n" x)) expected-values)
 (test-check name exprs
             expected-values computed-values
             expected-output computed-output
             expected-error  computed-error
             exn-expected? exn-raised?
             report-port source line column))
(code:line)
(define-syntax-rule
 (printing-test x ...)
 (test x ... #:check tstchck))
(code:line)
(printing-test 'a ((add1 3)) '(4))
(printing-test 'b ((sub1 3)) '(2))
(printing-test 'c (1) '(2))
(printing-test 'd
 ((define a 1)
  (define b 2)
  (define c 3)
  (values a b c))
 '(1 2 3))
(code:line)
(test-report)
]}

@defparam*[test-enable on/off any/c boolean? #:value #t]{

If @nbr[on/off] is not @nbr[#f] it is coerced to @nbr[#t].
Initially tests are enabled.@(lb)
When tests are disabled gathering of tests is ignored until enabled again.}

@defparam[test-time time-limit (or/c #f (>/c 0)) #:value #f]{

Specifies the default value for the @nbr[time-limit] argument of macro @nbr[test].
The initial value is @nbr[#f], indicating that no time limit is imposed.
A positive real number specifies a time limit in milliseconds.
@nbr[+inf.0] is coerced to @nbr[#f].
If a test exceeds its time limit it is halted by raising an @(excn),
which is caught and will be reported by @nbr[test-report] when not expected
or not the same as expected.
Output on the internal @elemref["output-string-port"]{@italic{output-string-port}} is discarded and
output on the internal @elemref["error-string-port"]{@italic{error-string-port}} is replaced by:

@inset[@(green (list (tt "test-time: limit of " (nbr time-limit) " milliseconds exceeded")))]

@Interaction[
(define counter 0)
(parameterize ((test-time 5))
 (test 'loop
  (( printf "this will be discarded")
   (eprintf "this will be replaced")
   (let loop () (set! counter (add1 counter)) (loop)))
  '()
  #:cg? #t
  #:error (format "test-time: limit of ~s milliseconds exceeded"
                  (test-time))))
(test-report)
(code:comment " ")
(code:comment #,(black "The " (tt "counter") " shows how many iterations the "
                       (tt "loop") " has made"))
(code:comment #,(black  "within the " (nbr 5) " milliseconds the test was allowed to run."))
(code:comment " ")
counter]}

@defproc[(test-clear) void?]{
Clears the list of tests gathered by macro @nbr[test].}

@defproc[(test-list) (listof
                      (list/c
                       (cons/c 'name            any/c)
                       (cons/c 'exprs           (listof any/c))
                       (cons/c 'thunk           (-> any))
                       (cons/c 'source          (or/c path? 'unknown))
                       (cons/c 'line            exact-positive-integer?)
                       (cons/c 'column          exact-nonnegative-integer?)
                       (cons/c 'expected-values (listof any/c))
                       (cons/c 'expected-output (or/c non-empty-string? #f))
                       (cons/c 'expected-error  (or/c non-empty-string? #f))
                       (cons/c 'exn-expected?   boolean?)
                       (cons/c 'time-limit      (or/c #f (>/c 0)))
                       (cons/c 'cg?             boolean?)
                       (cons/c 'check-procedure (procedure-arity-includes/c 14))))]{

Returns the gathered tests as a list of
@seclink["dicts" #:doc '(lib "scribblings/reference/reference.scrbl")]{association lists}
with symbols for the keys:

@Tabular[
(("Symbol" "Associated value")
 ((nbtt "name") "The name of the test.")
 ((nbtt "exprs") "The list of unevaluated exprs.")
 ((nbtt "thunk") @roman{Thunk @(test-thunk).})
 ((nbtt "source") @roman{Source of the test.})
 ((nbtt "line") "Line in this source.")
 ((nbtt "column") "Column in this line.")
 ((nbtt "expected-values") "The list of expected values.")
 ((nbtt "expected-output") @roman{The expected output to the @(racket current-output-port)
                                  or @(nbr #f).})
 ((nbtt "expected-error") @roman{The expected output to the @(nbr current-error-port)
                                 or @(nbr #f).})
 ((nbtt "exn-expected?") (list "Indicates whether or not an " (excn) " is expected."))
 ((nbtt "time-limit") "Time-limit in milliseconds imposed on the test.")
 ((nbtt "cg?") @roman{When @nbr[#t] garbage is collected before the test is executed.})
 ((nbtt "check-procedure") @roman{Procedure that is supposed to check the results of the test.}))
 #:sep (hspace 2)
 #:column-properties '(left left)
 #:row-properties `((top top-border bottom-border) ,@(make-list 12 'top) (top bottom-border))]

The thunk yields the values to be checked against the expected values.
It is not protected and has no continuation barriers.
It may produce output and may raise an @(excn). For example:

@Interaction[
(define-values (a b c) (values 1 2 3))
(test "test 1" ((values a b c)) '(1 2 3))
(test "test 2" ((display "test 2")) #f #:output "test 2")
(let/cc cc (test "test 3" ((cc "test 3")) '()
            #:error (list "continuation application:"
                          "jump out of dynamic extent of test")))
(test "test 4" ((error "error")) '() #:error "error")
(define saved-list-of-tests (test-list))
(pretty-display (car saved-list-of-tests))
(test-report)
(define-values (thunk-1 thunk-2 thunk-3 thunk-4)
 (apply values
  (map (compose cdr (curry assoc 'thunk))
   saved-list-of-tests)))
(code:line (thunk-1) (code:comment #,(black "the thunk of test 1 returns 3 values:")))
(code:line (thunk-2) (code:comment #,(black "the thunk of test 2 displays \"test 2\"")))
(code:line (thunk-3) (code:comment #,(black "the thunk of test 3 returns \"test 3\"")))
(code:line (thunk-4) (code:comment #,(black "the thunk of test 4 raises an error:")))]}

@defform[
(test-immediately name (expr ...) expected-values #,(black "etc."))]{

Like @racket[test], but immediately executes the test.
The test is not put into the @nbr[test-list].
Tests that may already have been gathered by macro @nbr[test] remain in the @nbr[test-list].}

@defproc[(normalize-whitespace (arg (or/c #f string? (listof string?)))) (or/c #f non-empty-string?)]{

Normalizes white@(-?)space in a string or list of strings.
If the @nbr[arg] is @nbr[#f] it is returned as is.
If a list of strings is given, they are concatenated with a separating space between every
pair of adjacent elements. The resulting string is normalized as follows.
If the @nbr[arg] is a string, heading and trailing white@(-?)space is removed and every other
sequence of contiguous white@(-?)space is replaced by one single space.
The resulting string is returned, except when it is empty, in which case @nbr[#f] is returned.

Examples:

@Interaction[
(normalize-whitespace '())
(normalize-whitespace "")
(normalize-whitespace (make-list 10 "  "))
(normalize-whitespace "  a  b  c  ")
(normalize-whitespace '(" a " " b " " c "))
(code:comment #,(black "Null is not whitespace."))
(normalize-whitespace "   \u0   ")]}

@defproc[(test-compare? (value-1 any/c) (value-2 any/c)) boolean?]{

Like @nbr[equal?], but strings with the same
@nbrl[normalize-whitespace]{normalized} form are considered to be equal to each other as well.
This applies recursively within pairs, vectors and boxes, both mutable and immutable ones.
Circular constructs do no harm.

@Interaction[
(define a (mcons  "1"  '())) (set-mcdr! a a)
(define b (mcons " 1 " '())) (set-mcdr! b b)
(code:line (equal?        a b) (code:comment #,(red "false")))
(code:line (test-compare? a b) (code:comment #,(green "true")))]}

@section{Nested tests}

Procedure @nbr[test-report] clears the list of tests after saving it in a local variable
but before performing the tests.
Nested tests are gathered in a new list.
Macro @nbr[test] can test itself.

@Interaction[
(test 0 ((test-list)) '(()))
(code:comment #,(black (nbr test-list) " is cleared before the test will be executed."))
(code:line)
(code:comment #,(black "Nested test, three levels deep."))
(code:line)
(test 1
 ((test 2
   ((test '3a ('3a) '(3a))
    (test '3b ('3b) '(3b))
    (test-report)) #f #:output "Nr of tests: 2 All is well.")
  (test-report))   #f #:output "Nr of tests: 1 All is well.")
(test-report)]

Nested tests in other arguments than @(test-arg) neither cause problems,
but may be confusing and probably have no practical use.
They do no harm, though.
They are inserted in the same @nbr[test-list] as the enclosing @nbr[test]-form.
Similar remarks apply to nested calls to procedure @nbr[test-report].
Do you understand the following puzzle?

@Interaction[
(test (begin (test 1 (1) '(1)) 4)
      (4)
      (begin (test 2 (2) '(2))
             (printf "~s~n" (map car (test-list)))
             (test-report)
             (test 3 (3) '(3))
             '(4)))
(map car (test-list))
(test-report)]

@section[#:tag "Predict error messages"]{Predict error messages}

It may be easy to predict that an error will be raised,
but it may be difficult to predict the exact message.
In that case macro @nbr[test] can be helpfull, for example:

@Interaction[
(define cop (current-output-port))
(define-syntax-rule (get-error-message n expr)
                    (test n (expr) '() #:exn #t #:check my-check))
(define (my-check . args)
 (fprintf cop "~a: ~a~n"
  (list-ref args 0) (code:comment "name")
  (list-ref args 7) (code:comment #,(list "computed error" (lb) (hspace 2)))
  ))
(code:comment "Let's find some error messages:")
           (get-error-message 1 (add1 'a))
           (get-error-message 2 λ)
(let/ec ec (get-error-message 3 (ec)))
(let/cc cc (get-error-message 4 (cc)))
(code:comment "Suppress output of the report.")
(parameterize ((current-output-port (open-output-nowhere)))
 (test-report))]

The messages of examples 3 and 4 differ,
because the tests are not called from within the dynamic extent
of the @(nbr let/ec) cq @(nbr let/cc) form.
They are called by procedure @nbr[test-report].
The thunk of example 3 is called outside the continuation barrier imposed by @(nbr let/ec).
@nb{In example 4} @nbr[let/cc] does not impose a barrier, but procedure @nbr[test-report] does.

@section[#:tag "Make check procedure"]{Make check procedure}

One can adapt tests by supplying ones own @(italic (tt "check-procedure")) to macro @nbr[test].
Such a @(italic (tt "check-procedure")) must accept arguments as described for procedure
@nbr[test-check] and should return @nbr[#f] for a failing test,
any arbitrary other value for a passing test. The @(italic (tt "check-procedure")) can use the
@tt[(italic "report-port")]
to communicate details of a failing test to procedure @nbr[test-report].
Below two examples testing numeric results. First a test on exact integer numbers:

@Interaction[
(define-syntax-rule (test-exact name computed expected)
 (let ((expect expected))
  (unless (exact-integer? expect)
   (raise-argument-error 'test-exact "exact integer" expect))
  (test name (computed) (list expect)
   #:check exact-check-procedure)))
(code:comment " ")
(define (exact-check-procedure
         name
         exprs
         expected-values computed-values
         expected-output computed-output
         expected-error  computed-error
         exn-expected?   exn-raised?
         report-port
         source line column)
 (define nr-of-values (length computed-values))
 (define computed
  (and (= nr-of-values 1)
       (car computed-values)))
 (define (failure-header)
  (printf " ~ntest ~a fails~n" name)
  (printf " source: ~s, line: ~s, column: ~s~n" source line column))
 (define expected (car expected-values))
 (parameterize ((current-output-port report-port))
  (cond
   (exn-raised?
    (failure-header)
    (printf " exception raised:~n")
    (printf " ~a~n" computed-error)
    #f)
   ((and (exact-integer? computed)
         (= computed expected)))
   ((not (= nr-of-values 1))
    (failure-header)
    (printf " expected 1 value, computed ~s values:~n" nr-of-values)
    (for ((v (in-list computed-values))) (printf " ~s~n" v))
    #f)    
   (else
    (failure-header)
    (printf " computed: ~s~n" computed)
    (printf " expected: ~s~n" expected)
    #f))))
(code:comment " ")
(test-exact 0         (factorial 0)     1)
(test-exact 1         (factorial 1)     1)
(test-exact 2         (factorial 2)     2)
(test-exact 5         (factorial 5)   120)
(test-exact '‹wrong›  (factorial 5.0) 120)
(test-exact '‹error›  (factorial 'a)    0)
(test-exact '‹values› (values 1 2 3)    0)
(code:comment " ")
(define (factorial n)
 (if (zero? n) 1 (* n (factorial (sub1 n)))))
(code:comment " ")
(test-report)]

Test on real numbers, possibly inexact.
@(tt "delta") is the allowed absolute error.

@Interaction[
(define-syntax-rule (test-inexact name computed expected delta)
 (let ((expect expected) (δ delta))
  (unless (real? expect)
   (raise-argument-error 'test-inexact "real number" expect))
  (unless (and (real? δ) (not (negative? δ)) (not (nan? δ)))
   (raise-argument-error 'test-inexact "nonnegative real number" δ))
  (test name (computed) (list expect)
   #:check (λ args (apply inexact-check-procedure δ args)))))
(code:comment " ")
(code:comment #,(black "Notice the additional argument " (tt "delta")))
(code:comment " ")
(define (inexact-check-procedure
         delta
         name
         exprs
         expected-values computed-values
         expected-output computed-output
         expected-error  computed-error
         exn-expected?   exn-raised?
         report-port
         source line column)
 (define (failure-header)
  (printf " ~ntest ~a fails~n" name)
  (printf " source: ~s, line: ~s, column: ~s~n" source line column))
 (parameterize ((current-output-port report-port)) 
  (cond
   (exn-raised?
    (failure-header)
    (printf " exception raised:~n")
    (printf " ~a~n" computed-error)
    #f)
   (else
    (define nr-of-values (length computed-values))
    (define computed 
     (and (= nr-of-values 1)
      (car computed-values)))
    (define expected (car expected-values))
    (cond
     ((not (= nr-of-values 1))
      (failure-header)
      (printf " expected 1 value, computed ~s values:~n" nr-of-values)
      (for ((v (in-list computed-values))) (printf " ~s~n" v))
      #f)
     ((not (real? computed))
      (failure-header)
      (printf " computed value is not a real number~n")
      (printf " computed: ~s~n" computed)
      #f)
     ((<= (abs (- computed expected)) delta))
     (else
      (failure-header)
      (define (rprintf fmt r)
       (printf fmt (~r r #:notation 'exponential
                         #:precision (list '= 20))))
      (rprintf  " computed:         ~a~n" computed)
      (define (print-range op+ op-)
       (rprintf " expected: between ~a~n" (op+ expected delta))
       (rprintf "               and ~a~n" (op- expected delta)))
      (if (negative? expected)
       (print-range + -)
       (print-range - +))
      #f))))))
(code:comment " ")
(test-inexact "sin( 30°)"             (sin (/ pi 6)) 1/2  1e-10)
(test-inexact "asin(1/2)"             (asin 1/2) (/ pi 6) 1e-10)
(test-inexact "sin(180°)"             (sin    pi   ) 0    1e-10)
(test-inexact "cos( 90°)"             (cos (/ pi 2)) 0    1e-10)
(test-inexact "cos( 60°)"             (cos (/ pi 3)) 1/2  1e-10)
(test-inexact "‹inaccurate sin(30°)›" (sin (/ pi 6)) 1/2  (expt 10 -17))
(test-inexact "‹raises error›"        (atan 0+1i)    0    +inf.0)
(test-inexact "not a real"            'a             0    +inf.0)
(test-inexact "multiple value"        (values 1 2 3) 0    +inf.0)
(code:comment " ")
(test-report)]

@(larger (larger (bold "The end.")))