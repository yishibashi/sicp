

; integer

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer (lambda (x) (tag x)))
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put 'raise '(integer) (lambda (x) ((get 'make 'rational) x 1)))
  'done)

  (install-integer-package)

 (define (make-integer x )
   ((get 'make 'integer) x))
