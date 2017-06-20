#lang planet neil/sicp


; event driven simulation
; http://www.cs.toronto.edu/~heap/270F02/node54.html




(define make-wire '())



(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after delay inverter-dellay
             (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (logical-and s t)
  (cond ((and (= s 1)
              (= t 1))
         1)
        (and (or (= s 1) (= s 0))
             (or (= t 1) (= t 1))
             0)
        (else (error "Invalid signal" s))))


(defien (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let (new-value
          (logical-and (get-signal a1) (get-signal a2))))
    (after-delay
     (lambda () (set-signal! output new-value))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)





;;; Ex 3.28


(define (logical-or s t)
  (cond ((or (= s 1) (= t 1))
         1)
        ((not (or (= s 1) (= t 1)))
         0)
        (else (error "Invalid signal" s))))

(defien (or-gate a1 a2 output)
  (define (and-action-procedure)
    (let (new-value
          (logical-or (get-signal a1) (get-signal a2))))
    (after-delay
     (lambda () (set-signal! output new-value))))
  (or-action! a1 or-action-procedure)
  (or-action! a2 or-action-procedure)
  'ok)



;;; Ex 3.29

; x and y == not ((not x) and (not y))

(define (or-gate2 a1 a2 output)
  (define (and-action-procedure)
    (let (new-value
          (logical-not
           (logical-and (logical-not (get-signal a1))
                        (logical-not (get-signal a2)))))
    (after-delay
     (lambda () (set-signal! output new-value)))))
  (or-action! a1 or-action-procedure)
  (or-action! a2 or-action-procedure)
  'ok)


