;;
;; Traditional map filter accumulate using lists
;;
(define (accumulate fn seed src)
  (cond
    ((null? src) seed)
    (else (fn (car src) (accumulate fn seed (cdr src))))))

(define (my-filter pred? src)
  (cond
    ((null? src) '())
    ((pred? (car src))
    (cons (car src) (my-filter pred? (cdr src))))
    (else (my-filter pred? (cdr src)))))

(define (my-map fn src)
  (cond
    ((null? src) '())
    (else (cons (fn (car src)) (my-map fn (cdr src))))))

(define (enumerate-interval from to)
  (cond
    ((> from to) '())
    (else (cons from (enumerate-interval (+ 1 from) to)))))

;;
;; streams - constructor and selectors
;;
(define (delay x)
  (lambda () x))

(define (force x) (x))

(define (cons-stream x y)
  (cons x (delay y)))

(define (head-of-stream s)
  (car s))

(define (rest-of-stream s)
  (force (cdr s)))

(define (empty-stream? s)
  (eq? 'empty-stream s))

;;
;; stream operations
;;
(define (stream-each fn s)
  (cond
    ((empty-stream? s) 'done)
    (else
      (fn (head-of-stream s))
      (stream-each fn (rest-of-stream s)))))

(define (display-stream s)
  (display "Stream-{ ")
  (stream-each (lambda (item) (display item) (display " ")) s)
  (display "}"))

(define (accumulate-stream fn seed src)
  (cond
    ((empty-stream? src) seed)
    (else
      (fn
        (head-of-stream src) 
        (accumulate-stream fn seed (rest-of-stream src))))))

(define (filter-stream pred? src)
  (cond
    ((empty-stream? src) 'empty-stream)
    ((pred? (head-of-stream src))
      (cons-stream
        (head-of-stream src)
        (filter-stream pred?
                       (rest-of-stream src))))
    (else (filter-stream pred? (rest-of-stream src)))))

(define (skip-stream count src)
  (cond 
    ((= 0 count) 'empty-stream)
    (else (skip-stream (- count 1) (rest-of-stream src)))))

(define (take-stream count src)
  (cond
    ((= 0 count) 'empty-stream)
    (else (cons-stream (head-of-stream src) (take-stream (- count 1) (rest-of-stream src))))))

(define (map-stream fn src)
  (cond
    ((empty-stream? src) 'empty-stream)
    (else (cons-stream (fn (head-of-stream src)) (map-stream fn (rest-of-stream src))))))

(define (interval-stream from to)
  (cond
    ((> from to) 'empty-stream)
    (else (cons-stream from (interval-stream (+ 1 from) to)))))

(define (fibonacci-stream-generator a b)
  (cons-stream a (fibonacci-stream-generator b (+ a b))))

(define fibonacci-stream (fibonacci-stream-generator 0 1))

(display-stream (interval-stream 1 5))
(display (accumulate-stream + 0 (interval-stream 1 5)))
(display-stream (filter-stream odd? (interval-stream 1 100)))
(display-stream (take-stream 5 (skip-stream 3 fibonacci-stream)))

