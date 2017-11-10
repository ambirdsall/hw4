
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([f (lambda (l result)
                (if (<= l high)
                    (f (+ l stride) (append result (list l)))
                    result))])
    (f low null)))

(define (string-append-map strings suffix)
  (map (lambda (s) (string-append s suffix)) strings))

(define (list-nth-mod nums n)
  (cond
    [(> 0 n) (error "list-nth-mod: negative number")]
    [(null? nums) (error "list-nth-mod: empty list")]
    [else (car (list-tail nums (remainder n (length nums))))]))
