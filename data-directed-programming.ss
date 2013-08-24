;;
;; types
;;
(define (attach-type type contents)
  (cons type contents))

(define (type thing)
  (car thing))

(define (contents thing)
  (cdr thing))

;;
;; operator dispatch
;;
;; Modelled as a list of key value pairs where the key is the composite of the
;; operator and the type it operates on
(define (make-key type operator)
  (cons type operator))

(define (key-eq? key1 key2)
  (and (eq? (car key1) (car key2)) (eq? (cdr key1) (cdr key2))))

(define (make-entry key value)
  (cons key value))

(define (key entry)
  (car entry))

(define (value entry)
  (cdr entry))

(define (put-operator type opname operator operators)
  (cons (make-entry (make-key type opname) operator) operators))

(define (get-operator type operator ops)
  (cond
    ((null? ops) (error "no operator for type" type operator))
    ((key-eq? (key (car ops)) (make-key type operator)) (value (car ops)))
    (else (get-operator type operator (cdr ops)))))

(define (operate operator operators x y)
  (if (not (eq? (type x) (type y)))
      (error "types must match")
      ((get-operator (type x) operator operators) (contents x) (contents y))))

;;
;; generic operators
;;
(define (add x y operators)
  (operate 'add operators x y))

(define (sub x y operators)
  (operate 'sub operators x y))

(define (mul x y operators)
  (operate 'mul operators x y))

(define (div x y operators)
  (operate 'div operators x y))

;;
;; ordinary numbers
;;
(define (make-ordinary num)
  (attach-type 'ordinary num))

(define operators
  (put-operator 
    'ordinary 'div /
    (put-operator 
      'ordinary 'mul *
      (put-operator 
        'ordinary 'sub -
        (put-operator 'ordinary 'add + '())))))

(display (add (make-ordinary 2) (make-ordinary 3) operators))
(display (mul (make-ordinary 2) (make-ordinary 3) operators))
(display (sub (make-ordinary 2) (make-ordinary 3) operators))
(display (div (make-ordinary 2) (make-ordinary 3) operators))
