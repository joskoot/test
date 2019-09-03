#lang racket

; Use test.scrbl for the preparation of documentation.

;====================================================================================================

(require
 (only-in racket/block block)
 (only-in syntax/macro-testing convert-syntax-error))

(provide
 test
 test*
 test-report
 test-enable
 test-clear
 test-list
 test-check
 test-time
 test-immediately
 normalize-whitespace
 test-compare?)

;====================================================================================================

(define-syntax (test stx)
 (syntax-case stx ()
  ((_ name (form ...) expected-values . rest)
   (with-syntax
    ((source (datum->syntax stx (syntax-source stx)))
     (line   (datum->syntax stx (syntax-line   stx)))
     (column (datum->syntax stx (syntax-column stx))))
  #'(when (test-enable)
     (let ((local-name name)) ; Evaluate the name once only.
      (test-put!
       local-name
       '(form ...)
       (procedure-rename
        (λ () (convert-syntax-error (block form ...)))
        (string->symbol (format "test-thunk-~a" local-name)))
       (or expected-values ‹‹void››)
       'source 'line 'column
       . rest))))))) ; rest: keyword arguments, evaluated once in order as they appear in the form.

(define (test* name thunk expected-values
         #:output (expected-output #f)
         #:error  (expected-error  #f)
         #:exn    (exn-expected?   expected-error)
         #:time   (time-limit      (test-time))
         #:check  (check-proc      test-check)
         #:cg?    (cg?             #f))
 (test-put! name (list (list (or (object-name thunk) 'unknown))) thunk (or expected-values ‹‹void››)
         'unknown #f #f ; no source info.
         #:output expected-output
         #:error  expected-error
         #:exn    exn-expected?
         #:time   time-limit
         #:check  check-proc
         #:cg?    (and cg? time-limit #t)))

;====================================================================================================

(define (test-report)
 (define nr-of-tests (length tests))
 (cond
  ((zero? nr-of-tests) (printf "No tests recorded.~n"))
  (else
   (define details-port (open-output-string))
   (define gathered-tests (reverse tests))
   (test-clear)
   (define nr-of-failures
    (for/sum ((test (in-list gathered-tests)))
     (if (test-apply test details-port) 0 1)))
   (printf "Nr of tests: ~s~n" nr-of-tests)
   (case nr-of-failures
    ((0) (printf "All is well.~n"))
    ((1) (printf "One test failed.~n")
     (pretty-display (get-output-string details-port)))
    (else (printf "~s tests failed.~n" nr-of-failures)
     (pretty-display (get-output-string details-port)))))))

;====================================================================================================

(define (test-put! name forms thunk expected-values source line column
         #:output (expected-output #f)
         #:error  (expected-error  #f)
         #:exn    (exn-expected?   expected-error)
         #:time   (time-limit      (test-time))
         #:check  (check-proc      test-check)
         #:cg?    (cg?             #f))
 (unless
  (or (list?  expected-values)
      (false? expected-values))
  (raise-argument-error 'test
   "expected-values: (or/c list? #f)" expected-values))
 (unless
  (or (string?        expected-output)
      (false?         expected-output)
      (listof-string? expected-output))
  (raise-argument-error 'test
   "expected-output: (or/c string? (listof string?) #f)" expected-output))
 (unless
  (or (string?        expected-error)
      (false?         expected-error)
      (listof-string? expected-error))
  (raise-argument-error 'test
   "expected-error: (or/c string? (listof string?) #f)" expected-error))
 (unless
  (or (eq? check-proc test-check)
      ((procedure-arity-includes/c 14) check-proc))
  (raise-argument-error 'test
   "procedure check: (and/c procedure? (procedure-arity-includes/c 14)" check-proc))
 (unless (or (not time-limit) (and (real? time-limit) (positive? time-limit)))
  (raise-argument-error 'test
   "time-limit: (or/c #f (>/c 0))" time-limit))
 (set! tests
  (cons (list name forms thunk
             (if (path? source) source 'unknown)
             (if (path? source) line #f)
             (if (path? source) column #f)
              expected-values 
             (normalize-whitespace expected-output)
             (normalize-whitespace expected-error)
             (and exn-expected? #t)
             (if (equal? time-limit +inf.0) #f time-limit)
             (and time-limit cg? #t)
             check-proc)
        tests)))

;====================================================================================================

(define (test-check name forms
                      expected-values computed-values
                      expected-output computed-output
                      expected-error  computed-error
                      exn-expected?   exn-raised?
                      details-port
                      source line column)
 
 (define correct-values? (test-compare? computed-values expected-values))
 
 (define correct-output?
  (cond
   ((not expected-output) (not computed-output))
   ((and (string? expected-output) (string? computed-output))
    (test-compare? computed-output expected-output))
   (else #f)))
 
 (define correct-error-output?
  (cond
   ((not expected-error) (not computed-error))
   ((and (string? expected-error) (string? computed-error))
    (equal? computed-error expected-error))
   (else #f)))

 (define correct-exn? (not (xor exn-raised? exn-expected?)))

 (parameterize ((current-output-port details-port))
 
  (cond
   ((and correct-values? correct-output? correct-error-output? correct-exn?))
   (else
    (printf " ~nTest ~a failed.~n" name)
    (printf " source: ~a, line: ~a, column: ~a~n"
     (if (path? source) source "unknown") line column)
    (case (length forms)
     ((0) (printf " No forms given.~n"))
     ((1) (printf " Expression: ~s~n" (car forms)))
     (else (printf " Expressions:~n")
      (for ((form (in-list forms))) (printf "  ~s~n" form))))
    (unless correct-values?
     (case (length expected-values)
      ((0) (printf " Expected values: none.~n"))
      ((1) (printf " Expected value: ~s~n" (car expected-values)))
      (else (printf " Expected values:~n")
       (for ((val (in-list expected-values))) (printf "  ~s~n" val))))
     (case (length computed-values)
      ((0) (printf " Computed values: none.~n"))
      ((1) (printf " Computed value: ~s~n" (car computed-values)))
      (else (printf " Computed-values:~n")
       (for ((val (in-list computed-values))) (printf "  ~s~n" val)))))
    (unless correct-output?
     (cond
      ((equal? expected-output "") (printf " Expected output: none.~n"))
      (expected-output (printf " Expected output: ~a~n" expected-output))
      (else (printf " Expected output: none.~n")))
     (cond
      ((not computed-output) (printf " Computed output: none.~n"))
      (else (printf " Computed output: ~a~n" computed-output))))
    (unless correct-error-output?
     (cond
      (expected-error (printf " Expected error: ~a~n" expected-error))
      (else (printf " Expected error: none.~n")))
     (cond
      ((not computed-error) (printf " Computed error: none.~n"))
      (else (printf " Computed error: ~a~n" computed-error))))
    (unless correct-exn?
     (cond
      (exn-expected? (printf " Exception expected but not raised.~n"))
      (else (printf " Unexpected exception.~n"))))
    #f))))

;====================================================================================================

(define (test-apply test output-port)
 (define-values (name
                 forms
                 thunk
                 source line column
                 expected-values
                 expected-output
                 expected-error
                 exn-expected?
                 time-limit cg?
                 check-proc) (apply values test))
 (define computed-values '())
 (define computed-output #f)
 (define computed-error #f)
 (define exn-raised? #f)
 ; Run the test in a thread in order to preserve all parameters upon return from the test.
 ; Also allows the timer to abort it.
 (define (compute)
  (parameterize-break #t
   (wrap (λ () (call-with-continuation-barrier thunk)))))
 (define thd #f)
 (define sema-ready (make-semaphore 0))
 (define sema-one-only (make-semaphore 1))
 (define sema-thd-running (make-semaphore 0))
 (define sema-wait-running (make-semaphore 0))
 (let/ec ec
  (define (break-handler exn)
   (kill-thread thd)
   (set! computed-output #f)
   (set! computed-error "break")
   (set! exn-raised? #t)
   (ec))
  (with-handlers ((exn:break? break-handler))
   (when cg? (collect-garbage))
   (cond
    ((not time-limit)
     (set! thd
      (thread
       (λ ()
        (set!-values (computed-values computed-output computed-error exn-raised?) (compute)))))
     (thread-wait thd))
    (else
     (define (epilogue)
      (set! computed-values '())
      (set! computed-output "")
      (set! computed-error (format "test-time: limit of ~s milliseconds exceeded" time-limit))
      (set! exn-raised? #t))
     ; thd-wait waits time-limit milliseconds, then breaks thd.
     ; thd does the computation and then kills thd-wait.
     ; semaphores are used for synchronization.
     ; sema-ready ; posted when one of the threads has finished and thas killed the other one.
     ; sema-one-only : make sure only one thread is killing the othere one.
     ; sema-thd-running : make sure that thd is running before thd-wait starts its job proper.
     ; sema-wait-running : make sure that thd-wait is running before thd starts its job proper.
     (define thd-wait
      (thread
       (λ ()
        (semaphore-post sema-wait-running)
        (semaphore-wait/enable-break sema-thd-running)
        (sleep (/ time-limit 1000)) ; milliseconds to seconds
        (when (thread-running? thd)
         (semaphore-wait/enable-break sema-one-only)
         (kill-thread thd) (epilogue) (semaphore-post sema-ready)))))
     (set! thd
      (thread
       (λ ()
        (semaphore-post sema-thd-running)
        (semaphore-wait/enable-break sema-wait-running)
        (define-values (temp-values temp-output temp-error temp-exn) (compute))
        (semaphore-wait/enable-break sema-one-only)
        (kill-thread thd-wait)
        (set!-values (computed-values computed-output computed-error exn-raised?)
         (values temp-values temp-output temp-error temp-exn))
        (semaphore-post sema-ready))))
     (semaphore-wait/enable-break sema-ready)))))
 (check-proc name forms
             expected-values computed-values
             expected-output (normalize-whitespace computed-output)
             expected-error  (normalize-whitespace computed-error)
             exn-expected?   exn-raised?
             output-port source line column))

;====================================================================================================

(define (wrap thunk)
 (define output-string (open-output-string 'test-output))
 (define error-string (open-output-string 'test-error))
 (define exn-encountered? #f)
 (define in-extent? #t)
 (define custodian (make-custodian))
 (let/ec ec
  (parameterize ((current-custodian custodian))
   (define (exit list-of-values)
    (set! in-extent? #f)
    (ec list-of-values
        (get-output-string-or-false output-string)
        (get-output-string-or-false error-string)
        exn-encountered?))
   (define (main)
    (define (exn-handler exn)
     (set! exn-encountered? #t)
     (display (exn-message exn) error-string)
     (exit '()))
    (define (not-exn-handler not-exn)
     (display not-exn error-string)
     (set! exn-encountered? #t)
     (exit '()))
    (define (break-handler exn)
     (display "break" error-string)
     (set! exn-encountered? #t)
     (exit '()))
    (parameterize ((current-output-port output-string)
                   (current-error-port error-string))
     (with-handlers ((exn:fail? exn-handler) (not-exn? not-exn-handler) (exn:break? break-handler))
      (exit
       (call-with-continuation-barrier
        (procedure-rename
         (λ () (call-with-values thunk list)) 'test-thunk))))))
   (define (epilogue)
    (custodian-shutdown-all custodian)
    (when in-extent?
     (display "continuation application: jump out of dynamic extent of test" error-string)
     (set! exn-encountered? #t)
     (exit '())))
   (dynamic-wind values main epilogue))))

;====================================================================================================

(define-syntax (test-immediately stx)
 (syntax-case stx ()
  ((_ arg ...)
   (datum->syntax stx
  #`(block
     (define save-tests tests)
     (test-clear)
   #,(syntax/loc stx #'(test arg ...))
     (test-report)
     (restore save-tests))
  stx stx))))

(define (restore save-tests) (set! tests save-tests))

;====================================================================================================

(define (test-time-guard x)
 (cond
  ((or (not x) (equal? x +inf.0)) #f)
  ((and (real? x) (positive? x)) x)
  (else (raise-argument-error 'test-time "#f or positive real number" x))))

(define tests '()) ; List of tests recorded by syntax test.
(define (test-enable-guard x) (and x #t))
(define ‹void› (void))
(define ‹‹void›› (list ‹void›))
(define listof-string? (listof string?))
(define (not-exn? x) (not (exn? x)))
(define (associate test-data) (map cons arg-names test-data))
(define (test-clear) (set! tests '()))
(define (test-list) (map associate (reverse tests)))
(define test-time  (make-parameter #f test-time-guard))
(define test-enable (make-parameter #t test-enable-guard))

(define arg-names
'(name
  forms
  thunk
  source
  line
  column
  expected-values
  expected-output
  expected-error
  exn-expected?
  time-limit
  cg?
  check-proc))

(define (get-output-string-or-false output-string)
 (define str (string-normalize-spaces (get-output-string output-string)))
 (and (> (string-length str) 0) str))

(define (normalize-whitespace x)
 (and x
  (let ((str (string-normalize-spaces (if (listof-string? x) (string-join x) x))))
   (and (not (equal? str "")) str))))

;====================================================================================================

(define (test-compare? a b) (or (equal? a b) (compare-help 0 a (hasheq) b (hasheq))))

(define (compare-help n a hash-a b hash-b)
 (cond
  ((equal? a b))
  (else
   (define key-a (hash-ref hash-a a -1))
   (define key-b (hash-ref hash-b b -2))
   (cond
    ((= key-a key-b))
    ((< key-b key-a 0)
     (cond
      ((string? a)
       (and (string? b)
        (equal?
         (string-normalize-spaces a)
         (string-normalize-spaces b))))
      ((list? a)
       (cond
        ((and (list? b) (= (length a) (length b)))
         (define n+1 (add1 n))
         (define ha (hash-set hash-a a n))
         (define hb (hash-set hash-b b n))
         (for/and ((a (in-list a)) (b (in-list b)))
          (compare-help n+1 a ha b hb)))
        (else #f)))
      ((pair? a)
       (cond
        ((pair? b)
         (define ha (hash-set hash-a a n))
         (define hb (hash-set hash-b b n))
         (define n+1 (add1 n))
         (and
          (compare-help n+1 (car a) ha (car b) hb)
          (compare-help n+1 (cdr a) ha (cdr b) hb)))
        (else #f)))
      ((mpair? a)
       (cond
        ((mpair? b)
         (define ha (hash-set hash-a a n))
         (define hb (hash-set hash-b b n))
         (define n+1 (add1 n))
         (and
          (compare-help n+1 (mcar a) ha (mcar b) hb)
          (compare-help n+1 (mcdr a) ha (mcdr b) hb)))
        (else #f)))
      ((box? a)
       (and (box? b)
        (compare-help (add1 n) (unbox a) (hash-set hash-a a n) (unbox b) (hash-set hash-b b n))))
      ((vector? a)
       (cond
        ((and (vector? b) (= (vector-length a) (vector-length b)))
         (define n+1 (add1 n))
         (define ha (hash-set hash-a a n))
         (define hb (hash-set hash-b b n))
         (for/and ((a (in-vector a)) (b (in-vector b)))
          (compare-help n+1 a ha b hb)))
        (else #f)))
      (else #f)))
    (else #f)))))

;====================================================================================================
; The end.
