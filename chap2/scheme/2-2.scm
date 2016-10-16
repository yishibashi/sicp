
;===============================================================================
;========== 2.2 Hierarchical Data andthe Closure Property ======================
;===============================================================================

(define nil '())

;===============================================================================
;========== 2.1.1 Example: Arithmetic Operations for Rational Numbers ==========
;===============================================================================

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

; procedure implements.
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; iterative style
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define odds (list 1 3 5 7))

; implemented using a recursive style. 
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))



;========== EXERCISE 2.17 ======================================================

; solution 1

(define (last-pair items)
  (list-ref items (- (length items) 1)))

(last-pair odds)
(last-pair squares)


; solution 2

(define (last-pair2 items)
  (if (null? (cdr items))
      (car items)
      (last-pair2 (cdr items))))

(last-pair2 odds)
(last-pair2 squares)

;========== EXERCISE 2.18 ======================================================

(define (reverse items)
  (if (null? (cdr items))
      (list (car items))
      (append (reverse (cdr items)) (list (car items)))))

(reverse odds)





;;; TEST =======================================================================
(print "odds : (1 3 5 7)")
(print "squares : (1 4 9 16 25)")

(print "----- Exercise 2.17 -----")

(print "last-pair odds")
(print (last-pair odds))
(print "last-pair squares")
(print (last-pair squares))

(print "last-pairi2 odds")
(print (last-pair2 odds))
(print "last-pair2 squares")
(print (last-pair2 squares))


(print "----- Exercise 2.18 -----")

(print (reverse odds))
(print (reverse squares))



(print "----- Exercise 2.19 -----")
(print "----- Exercise 2.20 -----")
(print "----- Exercise 2.21 -----")
(print "----- Exercise 2.22 -----")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))

(print "(square-list (1 2 3 3 4 5))")
(print (square-list (list 1 2 3 4 5)))

