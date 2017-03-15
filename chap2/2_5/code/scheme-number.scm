
; scheme-number

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  ; ex 2.79
  (define (equ-scm? x y) (= x y))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equ-scm? x y)))
  ; ex 2.80
  (define (=zero-scm? x) (= x 0))
  (put '=zero? '(scheme-number)
       (lambda (x) (=zero-scm? x)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
