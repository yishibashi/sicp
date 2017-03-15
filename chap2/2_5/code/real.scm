

; real-number

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real (lambda (x) (tag (* 1.0 x))))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
  (define (project->rational x)
    (let ((n (numerator (inexact->exact x)))
          (d (denominator (inexact->exact x))))
    ((get 'make 'rational) n d)))
  (put 'project '(real)
       (lambda (x) (project->rational x)))
  'done)


(install-real-package)

(define (make-real x )
  ((get 'make 'real) x))
