#lang racket
(require "test.rkt")
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
(test-report)