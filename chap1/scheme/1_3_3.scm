
(define (close-enough? x y) (< (abs (- x y)) 0.00001))
(define (average x y) (/ (+ x y) 2.0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(print "---------------------------------")
(print "---------------------------------")
(print "(search cos -1 2)")
(print (search cos -1 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
           ((and (negative? b-value) (positive? a-value))
            (search f b a))
           (else
            (error "Value are not of opposite sign" a b)))))
(print "---------------------------------")
(print "(half-interval-method sin 2 4)")
(print (half-interval-method sin 2 4))

(print "---------------------------------")
(print "(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)
")


(print (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2) (< (abs (- v1 v2))tolerance))

  (define (try guess)
    (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess))

(print "---------------------------------")
(print "(fixed-point cos -1.0)")
(print (fixed-point cos -1.0))
;; interpreter error

(define (sqrt x)
  (fixed-point (lambda (x) (/ x y))
               1.0))
; loop

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(print "---------------------------------")
(print "(sqrt 4)")
(print (sqrt 4)) ;interpreter error

(print "---------------------------------")
(print "(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)")

(print (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))
; >> 1.2587315962971173

(define (golden-ratio y)
  (fixed-point (lambda (y) (average y (+ 1 (/ 1 y)))) 1.0))

(print "---------------------------------")
(print "(golden-ratio 1.0)")
(print (golden-ratio 1.0))
(print (golden-ratio -2.0))
; >> 1.6180311591702674

(define tolerance 0.00001)
(define (mod-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display  guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(print "---------------------------------")
(print "(mod-fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)")
(mod-fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
(print "---------------------------------")
(print "(mod-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1)")
(print (mod-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1))
(define (cont-frac n d k)
  (define (calc i)
    (if (< k i)
          0.0
          (/ (n i) (+ (d i) (calc (+ 1.0 i))))))
  (calc 1))

(print "(/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                                13))")

(print (/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                13)))

(print "---------------------------------")
(print "---------------------------------")

(define (cont-frac-r n d k)
  (define(c i a)
    (if (= i 0)
        a
        (c 
         (- i 1)
         (/ (n i) (+ (d i) a)))))
  (c k 0))

(print (/ 1 (cont-frac-r (lambda (i) 1.0)
                (lambda (i) 1.0)
                13)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (cont-frac-l n d k)
;  (define (next n d ))



(define (get_e k)
  (define (getD i)
    (cond ((= i 1) 1.0)
          ((= i 2) 2.0)
          ((= (remainder (+ i 1) 3) 0) (* 2.0 (/ (+ i 1) 3)))
          (#t 1.0)))
  (+ 2 (cont-frac (lambda (i) 1.0)
             getD
             k)))
(print "---------------------------------")
(print "(get_e 35)")
(print (get_e 35))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                                  x
                                  (- (* x x))))
                  (lambda (i) (- (* 2 i) 1))
                  k))

(print "---------------------------------")
(print "(tan-cf 3.21 10)")
(print (tan-cf 3.21 10))

