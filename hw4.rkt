
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

(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (let ([next-chunk (stream)])
        (cons (car next-chunk) (stream-for-n-steps (cdr next-chunk) (- n 1))))))

(define (start-funny-number-stream n)
  (cons (if (= (modulo n 5) 0)
            (- n)
            n)
        (lambda ()
          (start-funny-number-stream (add1 n)))))

(define funny-number-stream (lambda () (start-funny-number-stream 1)))
