#lang racket

; print
(define (print x)
  (display x)
  (newline))

;get put
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

;
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (square x) (* x x))


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
;(define (raise x) (apply-generic 'raise x))
(define (raise x) ((get 'raise (type-tag x)) x))


;(define (project x) (apply-generic 'project x))
;(define (project x) ((get 'project (type-tag x)) x))
(define (project x)
  (let ((proc (get 'project (type-tag x))))
       (if proc
           (proc (contents x))
           #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ADD PACKAGES ! ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scheme-number
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
;; （タグつきの）通常の数値を作成する
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(install-scheme-number-package)

;; rational
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ; ex 2.79
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  ; ex 2.80
  (define (=zero-rat? x)
    (= (numer x) 0))
  (put '=zero? '(rational)
       (lambda (x) (=zero-rat? x)))
  ; ex 2.83
  (define (raise->real x) ; rational -> real
    ((get 'make 'real) (* (/ (numer x) (denom x )) 1.0)))
  (put 'raise 'rational
       (lambda (x) (raise->real x)))
  ; ex 2.85
  (define (project->int x)
    (let ((n (numer x))
          (d (denom x)))
      ((get 'make 'integer) (inexact->exact (floor (/ n d))))))
  (put 'project 'rational
       (lambda (x) (project->int x)))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(install-rational-package)

;; complex
;rectangular
(define (install-rectangular-package)
  ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;polar
(define (install-polar-package)
  ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)
(install-polar-package)
; complex
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; <-- add (exercise 2.77)
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ; add -->
  (define (add-complex z1 z2)
    (print z1)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  ; <- add for exercise 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ; add -->
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; ex 2.79
  (define (equ-comp? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex)
       (lambda (x y) (equ-comp? x y)))
  ; ex 2.80
  (define (=zero-comp? x)
    (and (= (imag-part x) 0)
         (= (real-part x) 0)))
  (put '=zero? '(complex)
       (lambda (x) (=zero-comp? x)))
  ; または、　magnitude == 0 をチェック
  ; ex 2.85
  (define (project->real x)
      ((get 'make 'real) (real-part x)))
  (put 'project 'complex
       (lambda (x) (project->real x)))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(install-complex-package)

;; integer
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
  (put 'raise 'integer (lambda (x) ((get 'make 'rational) x 1)))
  'done)
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
  (put 'raise 'real
       (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
  (define (project->rational x)
    (let ((n (numerator (inexact->exact x)))
          (d (denominator (inexact->exact x))))
    ((get 'make 'rational) n d)))
  (put 'project 'real
       (lambda (x) (project->rational x)))
  'done)

(install-integer-package)
(define (make-integer x )
  ((get 'make 'integer) x))

(install-real-package)
(define (make-real x )
  ((get 'make 'real) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;tag/content
(define (attach-tag tag contents)
  (if (eq? tag 'scheme-number)
      contents
      (cons tag contents)))

(define (type-tag datum)
  ;(print datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged dataum: CONTENTS" datum))))


(define (higher? t1 t2) ; check t1 > t2 ?
  (define (iter t1 t2 type-tower)
    (cond ((eq? t1 t2) #f)
          ((eq? t1 (car type-tower)) #t)
          ((eq? t2 (car type-tower)) #f)
          (else (iter t1 t2 (cdr type-tower)))))
  (iter t1 t2 type-tower))


(define type-tower '(complex real rational integer))

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
;                    (apply-generic op a1 (raise a2))
;                    (apply-generic op (raise a1) a2)))
;              (error "No method for these types" (list op type-tags)))))))

; MEMO: drop の実装が汚いのであとで直す. (if文)
(define (drop x)
  (if (not (eq? (type-tag x) 'integer))
      (let ((y (project x)))
        ;(print x)
        ;(print y)
        (if (equ? x (raise y))
            (drop y)
            x))
      x))


; MEMO: dropの実装は取り敢えずできたので後は apply-generic. (drop を付け足しても動かない)
(define (apply-generic op . args)
  ;(display op)
  ;(print args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (display proc)
      (display args)
      (print op)
      (if proc
          (if (or (eq? op 'project) (eq? op 'raise) (eq? op 'equ?))
              (apply proc (map contents args))
              (drop (apply proc (map contents args))))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (higher? type1 type2)
                    (apply-generic op a1 (raise a2))
                    (apply-generic op (raise a1) a2)))
              (error "No method for these types" (list op type-tags)))))))

(define i (make-integer 3))
(define r (make-rational 3 2))
(define rl (make-real 2.222))
(define c (make-complex-from-real-imag 3 2))
;(print "print 'i', 'r', 'rl', and 'c'")
;(print i)
;(print r)
;(print rl)
;(print c)
;(print "print raised 'i', 'r', 'rl', and 'c'")
;(print (raise i))
;(print (raise r))
;(print (raise rl))
;(equ? (make-rational 1 1) (make-rational 1 1))
;(print "print add, sub, mul & div")
;(print (add i r))
(print (sub i rl))
(print (mul c r))
(print (div r i))
(print "print project 'i', 'r', 'rl', and 'c'")
(print (project r))
(print (project rl))
(print (project c))
(print "drop")
(print (drop (make-complex-from-real-imag 3 1)))
(print (drop (make-complex-from-mag-ang 3 0)))
(print (drop (make-real 1.5)))
(print (drop (make-real 1.0)))
(print (add (make-real 1.0) (make-complex-from-real-imag 3 0)))

(print "DROOOOOP")
(print (make-complex-from-real-imag 3 0))

; ex 2.86
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(print "HOGE")
(define z1 (make-complex-from-real-imag (make-rational 3 2) (make-rational 1 2)))
(define z2 (make-complex-from-mag-ang (make-real 2.3) (make-rational 2 3)))
(print z1)
(print z2)
;(add z1 z1)





; 2.5.3 Symbolic Algebra
(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;add
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD_POLY" (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  ;mul
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  ; 2.87
  ;(put '=zero? 'polynomial
  ;     (lambda (t) (eq? 0 (coeff t))))
  ;  (define (=zero?-poly p)
  ;    (let ((ts (term-list p)))
  ;      (cond ((null? t) #t)
  ;            (
  ; 2.88
  (define (minus1 p)
    (make-poly (variable p) '((0 -1))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1
                                      (mul-poly (minus1 p2) p2)))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


; Test
(define px (make-polynomial 'x '((1 1))))
;(print px)

(define py (make-polynomial 'y '((1 1))))
;(print py)

(define p1 (make-polynomial 'x '((2 1) (1 2) (0 1))))
;(print p1)

(define p2 (make-polynomial 'x '((2 1) (1 4) (0 2))))
;(print p2)

(define m1 (make-polynomial 'x '((0 -1))))
;(print m1)

;(print (add p1 p2))
;(print (mul p1 p2))
;(print (sub p2 p1))

(define ip1 (make-polynomial 'x '((1 (real . 1.111)) (0 (integer . 1)))))
(define ip2 (make-polynomial 'x '((1 (rational 3 . 2)) (0 (integer . 3)))))
;(print (add ip1 ip2))
;(print (mul ip1 ip2))


;(define p3 (make-polynomial 'x '((2 (polynomial y (1 1))) (1 (polynomial y (1 1))) (0 1))))
;(print p3)
;(add p1 p3)
