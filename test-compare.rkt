#lang racket

(provide test-compare?)

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
