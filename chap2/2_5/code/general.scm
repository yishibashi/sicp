
; get/put/global-array


(include "./scheme-number.scm")
(include "./integer.scm")
(include "./rational.scm")
(include "./real.scm")
(include "./complex.scm")
(include "./polynomial.scm")



(define global-array '())
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put-table table key1 key2 item)
  (if (not (hash-has-key? table key1))
      (hash-set! table key1 (make-hash))
      true)
  (hash-set! (hash-ref table key1) key2 item))
(define (get-table table key1 key2)
  (define (not-found . msg)
    ;  (display msg (current-error-port))
    ;  (display "\n")
    false)
  (if (hash-has-key? table key1)
      (if (hash-has-key? (hash-ref table key1) key2)
          (hash-ref (hash-ref table key1) key2)
          (not-found "Bad key -- KEY2" key2))
      (not-found "Bad key -- KEY1" key1)))
(define *op-table* (make-hash))
(define (put op type item)
  (put-table *op-table* op type item))
(define (get op type)
  (get-table *op-table* op type))
(define *coercion-table* (make-hash))
(define (put-coercion type1 type2 item)
  (put-table *coercion-table* type1 type2 item))
(define (get-coercion type1 type2)
  (get-table *coercion-table* type1 type2))

; e.t.c

  ; numerical
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (square x) (* x x))

  ; print
  (define (print x)
    (display x)
    (newline))



;tag/content

(define (attach-tag tag contents)
  (cons tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents dataum)
  (if (pair? dataum)
      (cdr dataum)
      (error "Bad tagged dataum: CONTENTS" dataum)))



; apply-generic & e.t.c
(define type-tower '(complex real rational integer))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (higher? type1 type2)
                    (apply-generic op a1 (raise a2))
                    (apply-generic op (raise a1) a2)))
              (error "No method for these types" (list op type-tags)))))))
; MEMO: drop の実装が汚いのであとで直す. (if文)

(define (drop x)
  (if (not (eq? (type-tag x) 'integer))
      (let ((y (project x)))
        (if (equ? x (raise y))
            (drop y)
            x))
      x))

; MEMO: dropの実装は取り敢えずできたので後は apply-generic. (drop を付け足しても動かない)
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (drop (apply proc (map contents args)))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (higher? type1 type2)
;                    (let ((a (apply-generic op a1 (raise a2))))
;                      (drop a))
;                    (let ((b (apply-generic op (raise a1) a2)))
;                      (drop b))))
;              (error "No method for these types" (list op type-tags)))))))


(define (higher? t1 t2) ; check t1 > t2 ?
  (define (iter t1 t2 type-tower)
    (cond ((eq? t1 t2) #f)
          ((eq? t1 (car type-tower)) #t)
          ((eq? t2 (car type-tower)) #f)
          (else (iter t1 t2 (cdr type-tower)))))
  (iter t1 t2 type-tower))



; generic arithmetic

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))




(define i (make-integer 3))
(define r (make-rational 3 2))
(define rl (make-real 2.222))
(define c (make-complex-from-real-imag 3 2))
(print "print 'i', 'r', 'rl', and 'c'") (newline)
(print i)
(print r)
(print rl)
(print c)
