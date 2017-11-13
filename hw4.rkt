
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([f (λ (l result)
                (if (<= l high)
                    (f (+ l stride) (append result (list l)))
                    result))])
    (f low null)))

(define (string-append-map strings suffix)
  (map (λ (s) (string-append s suffix)) strings))

(define (list-nth-mod nums n)
  (cond
   [(> 0 n) (error "list-nth-mod: negative number")]
   [(null? nums) (error "list-nth-mod: empty list")]
   [else (car (list-tail nums (remainder n (length nums))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next-chunk (s)])
        (cons (car next-chunk) (stream-for-n-steps (cdr next-chunk) (- n 1))))))

(define (start-funny-number-stream n)
  (cons (if (= (modulo n 5) 0)
            (- n)
            n)
        (λ ()
          (start-funny-number-stream (add1 n)))))

(define funny-number-stream (λ () (start-funny-number-stream 1)))

(define dan-then-dog
  (λ ()
    (let ([dog-then-dan (lambda () (cons "dog.jpg" dan-then-dog))])
      (cons "dan.jpg" dog-then-dan))))

(define (stream-add-zero s)
  (let ([result (s)])
    (λ () (cons (cons 0 (car result)) (stream-add-zero (cdr result))))))

(define (cycle-lists xs ys)
  (letrec ([f (λ (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (λ () (f (+ n 1)))))])
    (λ () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (λ (i) (cond
                      [(>= i (vector-length vec)) #f]
                      [(pair? (vector-ref vec i))
                       (if (equal? v (car (vector-ref vec i)))
                           (vector-ref vec i)
                           (f (+ 1 i)))]
                      [else (f (+ 1 i))]))])
    (f 0)))

(define ((cached-assoc xs n) v)
  (let* ([cache (make-vector n #f)]
         [l (length xs)]
         [next-cache-idx 0]
         [update-next-cache-idx! (λ () (let ([candidate (+ 1 next-cache-idx)])
                                         (if (= candidate l)
                                             (set! next-cache-idx 0)
                                             (set! next-cache-idx candidate))))])
    (let ([cache-result (vector-assoc v cache)])
      (if cache-result
          cache-result
          (begin
            (let ([result (assoc v xs)])
              (if result
                  (begin (vector-set! cache next-cache-idx result)
                         (update-next-cache-idx!)
                         result)
                  #f)))))))
