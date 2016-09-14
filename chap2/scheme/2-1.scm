;; ========== Chapter 2 Building Abstractions with Data ========================
;; =============================================================================





;===============================================================================
;========== 2.1.1 Example: Arithmetic Operations for Rational Numbers ==========
;===============================================================================



; MAKE RATIONAL NUMBER
(define (make-rat n d) (cons n d)) ; returns the rational number d/n

; GET NUMERATOR
(define (numer x) (car x)) ; returns the numerator of the rational number <x> => d

;GET DENOMINATOR
(define (denom x) (cdr x)) ; returns the denominator of the rational number <x> => n

;PRINT RATIONAL NUMBER
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;===============================================================================
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

;(print (car z))
;(print (cdr z))

;(print (car (cdr z)))
;(print (cdr (car z)))
;===============================================================================

;;ARITHMETICAL PROCEDURE

;ADD
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
;SUBTRACTION
(define (sub-rat x y)
 (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
;MULTIPLY
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
;DIVISION
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))



;===============================================================================

(define one-half (make-rat 1 2))
;(print-rat one-half)

(define one-third (make-rat 1 3))

;(print-rat (add-rat one-half one-third))

;(print-rat (sub-rat one-half one-third))

;(print-rat (mul-rat one-half one-third))

;(print-rat (div-rat one-half one-third))

;(print-rat (add-rat one-third one-third))
;^^^ our rational number implimentation does not
;    reduce rational numbers to lowerst terems

;==============================================================================

;DEFINE mod-make-rat

;to make mod-make-rat, use gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;(print-rat (add-rat one-third one-third))
;(newline)
;^^^ get rational number as desired!!

;========== EXERCISE 2.1 =======================================================

(print "EXERCISE 2.1")


(define (abs x)
  (if (> 0 x)
      (- x)
      x))

(define (make-rat n d)
  (let ((g  (abs (gcd n d))))
   (if (< (* n d) 0)
     (cons (/ (abs n) (- g)) (/ (abs d) g))
     (cons (/ (abs n)  g) (/ (abs d) g))
     )
   )
  )


;===============================================================================
;========== 2.1.2 Abstraction Barrier ==========================================
;===============================================================================


;========== EXERCISE 2.2 =======================================================
(print "EXERCISE 2.2")


;; define Point

(define (make-point px py)
  (cons px py))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))



;; define Line

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))



;; get mid-point

(define (midpoint-segment line)
  (cons (/ (+ (car (start-segment line)) (car (end-segment line))) 2.0)
        (/ (+ (cdr (start-segment line)) (cdr (end-segment line))) 2.0)))


      (define a (make-point 0 0))
        (print-point a)

        (define b (make-point 1 1))
        (print-point b)

        (define c (make-point -1 -1))
        (print-point c)


        (define a2b (make-segment a b))

        (print-point (start-segment a2b))
        (print-point (end-segment a2b))
        (print-point (midpoint-segment a2b))



;========== EXERCISE 2.3 =======================================================

(print "EXERCISE 2.3")

;x軸,y軸に平行な場合

;a_4                      a_3
;+-------------------------+
;|                         |
;|                         |
;|        THIS IS          |
;|       RECTANGLE         |
;|                         |
;|                         |
;+-------------------------+
;a_1                       a_2

(define (abs x)
  (if (> 0 x)
      (- x)
      x))

(define (rectangle a1 a3)
  (cons a1 a3))

(define (get-perimeter rectangle)
  (* 2 (+ (abs (- (x-point (cdr rectangle)) (x-point (car rectangle))))
          (abs (- (y-point (cdr rectangle)) (y-point (car rectangle)))))))

(define (get-area rectangle)
  (* (abs (- (x-point (cdr rectangle)) (x-point (car rectangle))))
     (abs (- (y-point (cdr rectangle)) (y-point (car rectangle))))))


(define a_1 (make-point 0 0))
;(print-point a_1)
(define a_3 (make-point 3 3))
;(print-point a_3)

;(define rec (rectangle a_1 a_3))
;(print (get-perimeter rec))
;(print (get-area rec))



;一般の場合 は省略


;===============================================================================
;========== 2.1.3 What is Meant by Data ========================================
;===============================================================================


;========== EXERCISE 2.4 =======================================================
(print "EXERCISE 2.4")

(define (cons2.4 x y)
  (lambda (m) (m x y)))

(define (car2.4 z)
  (z (lambda (p q) p)))

(print (car2.4 (cons2.4 "x" "y")))

(define (cdr2.4 z)
  (z (lambda (p q) q)))

(print (cdr2.4 (cons2.4 "x" "y")))


;========== EXERCISE 2.5 =======================================================
(print "EXERCISE 2.5")
(define (product x a)
  (define (iter v a)
    (if (= a 0)
        v
        (iter (* v x) (- a 1))))
  (iter 1 a))


(define (pair-cons a b)
  (* (product 2 a)
     (product 3 b)))

(define (pair-car z)
  (define (iter n)
    (if (not (= (remainder z (product 2 n)) 0))
      (- n 1)
      (iter (+ n 1))))
  (iter 0))


(define (pair-cdr z)
  (define (iter n)
    (if (not (= (remainder z (product 3 n)) 0))
      (- n 1)
      (iter (+ n 1))))
  (iter 0))



;========== EXERCISE 2.6 =======================================================
(print "EXERCISE 2.6")

(define zero (lambda (f) (lambda (x) x)))

(define (add1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


  (define one
    (lambda (f)
      (lambda (x)
        (f x)
        )
      )
    )

  (define two
    (lambda (f)
      (lambda (x)
        (f (f x)
           )
        )
      )
    )


;===============================================================================
;========== 2.1.4 Extended Exercise: Interval Arithmetic =======================
;===============================================================================

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;========== EXERCISE 2.7 =======================================================
(print "EXERCISE 2.7")

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;========== EXERCISE 2.8 =======================================================
(print "EXERCISE 2.8")
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;========== EXERCISE 2.9 =======================================================
(print "EXERCISE 2.9")

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;```
;  $x := (a, b)\\y := (c, d)$
;
;  $x+y = (a+c, b+d),\\x-y = (a-d, b-c) $
;
;
;  \begin{eqnarray*}
;  width(x+y) = \frac{(b+d)-(a+c)}{2}\\
;             = \frac{(b-a)+(d-c)}{2}\\
;             = \frac{b-a}{2} + \frac{d-c}{2}\\
;             = width(x) + width(y)
;  \end{eqnarray*}
;  \\
;
;  \begin{eqnarray*}
;  width(x-y) = \frac{(b-c)-(a-d)}{2}\\
;             = \frac{(b-a)+(d-c)}{2}\\
;             = \frac{b-a}{2} + \frac{d-c}{2}\\
;             = width(x) + width(y)
;  \end{eqnarray*}
;```

;========== EXERCISE 2.10 ======================================================
(print "EXERCISE 2.10")

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      ;(error "Divide by an interval that spans zero.")
      ()
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;========== EXERCISE 2.11 ======================================================
(print "EXERCISE 2.11")



;========== EXERCISE 2.12 ======================================================
(print "EXERCISE 2.12")

(define (make-center-width c v)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))



(define (percent c p)
  (* c (/ p 100)))

(define (make-center-percent c p)
  (make-interval (- c (percent c p)) (+ c (percent c p))))

;========== EXERCISE 2.13 ======================================================
(print "EXERCISE 2.13")

;========== EXERCISE 2.14 ======================================================
(print "EXERCISE 2.14")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
   (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(print "A : interval (10 +- 1% ) ")
(define A (make-center-percent 10 1))
(print "B : interval (15 +- 5% ) ")
(define B (make-center-percent 15 5))

(print "par1 A/A :")
(print (lower-bound (par1 A A)))
(print (upper-bound (par1 A A)))
(print "par2 A/A :")
(print (lower-bound (par2 A A)))
(print (upper-bound (par2 A A)))


(print "par1 A/B :")
(print (lower-bound (par1 A B)))
(print (upper-bound (par1 A B)))
(print "par2 A/B :")
(print (lower-bound (par2 A B)))
(print (upper-bound (par2 A B)))


;========== EXERCISE 2.15 ======================================================
(print "EXERCISE 2.15")

;========== EXERCISE 2.16 ======================================================
(print "EXERCISE 2.16")
