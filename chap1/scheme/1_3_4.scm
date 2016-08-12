; fixed-point

(define tolerance 0.00001)
(define (fixed-point f first-guess) ;; USE gosh
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess))

; newtons-method

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; newton's method sqrt

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(print "Newton's method sqrt")
(print (sqrt 4))



(print "\nEXERCISAE\n")

;; Exercise 1.40

(define (cubic a b c)
  (newtons-method (lambda (x) (+ (* x x x) (* a x x) (* b x) c)) 2.0))

(print "\nExercise 1.40")
(print (cubic 3 3 1))


;; Exercise 1.41

(define (double func)
    (lambda (x) (func (func x))))

(define (inc x)
    (+ x 1))

; (print ((double inc) 3)) => 5

(print "\nExercise 1.41")
(print (((double (double double)) inc) 5)) ; => 21

;; Exercise 1.42

(define (compose f g)
    (lambda (x) (f (g x))))

(print "\nExercise 1.42")
(print ((compose square inc) 6))


;; Exercise 1.43

(define (repeated f n)
  (define (iter result n)
    (if (= n 1)
        result
        (iter (compose f result) (- n 1))))
  (iter f n))

(print "\nExercise 1.43")
(print ((repeated square 2) 5))
;; Exercise 1.44

(define (smooth f)
    (let  ((dx 0.001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(print "\nExercise 1.44")
(print " smooth")
(print ((smooth sin) 0.5))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(print " n-fold-smooth")
(print ((n-fold-smooth sin 5) 0.5))

;; Exercise 1.45
(print "\nExercise 1.45")


(define (average x y) (/ (+ x y) 2))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (product x a)
  (define (iter v a) 
    (if (= a 0)
        v
        (iter (* v x) (- a 1))))
  (iter 1 a))


(define (root x n)
  (fixed-point ((repeated average-dump 2) (lambda (y) (/ x (product y (- n 1))))) 1.0))

;(print (root 3 3))
;(print (root 3 4))
(print (root 5 5))
;(print (root 3 6))
;(print (root 3 7))
;(print (root 3 10))
;(print (root 3 16))

