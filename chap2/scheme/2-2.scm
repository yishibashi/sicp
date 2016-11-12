
;===============================================================================
;========== 2.2 Hierarchical Data andthe Closure Property ======================
;===============================================================================

;(define nil '())

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







(append nil (list 1))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; Section 1.2.2

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;(count-change 100) => 292

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(cc 100 us-coins)

; my solution
(define (same-parity x . xs)
  (let ([p (mod x 2)])
    (define (sp xs)
      (if (not (null? (cdr xs)))
          (if (= p (mod (car xs) 2))
              (cons (car xs ) (sp (cdr xs)))
              (sp (cdr xs)))
          (if (= p (mod (car xs) 2))
                 (list (car xs))
                 nil)))
    (cons x (sp xs))))

(define (same-parity x . xs)
  (define (sp? y)
    (= (mod (- x y) 2) 0))
  (define (sp xs)
    (if (null? xs)
      xs
      (if (sp? (car xs))
               (cons (car xs) (sp (cdr xs)))
               (sp (cdr xs)))))
  (cons x (sp xs)))

(same-parity 1 2 3 4 5 6 7 8 9 10 11 12)

(same-parity 2 3 4 5 6 7)

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))


(scale-list (list 1 2 3 4 5) 10)

(define (my_map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (my_map proc (cdr items)))))

(my_map abs (list  -10 2.4 -11 19))

(my_map (lambda (x) (* x x)) (list 1 2 3 4))

; a new definition of *scale-list* in terms of map:

(define (scale-list items factor)
  (my_map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))

(square-list (list 1 2 3 4 5))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer ; cons -> append
                    (list (square (car things))))))) ; (...) -> (list (...))
  (iter items nil))

(square-list (list 1 2 3 4 5))

;Bad solution

(define (for-each procedure items)
  (procedure (car items))
  (if (null? (cdr items))
      nil
      (for-each procedure (cdr items))))

(for-each (lambda (x) (print x)) (list 1 2 3 4 5))

(for-each (lambda (x) (* x x x)) (list 1 2 3 4 5))

(for-each (lambda (x) (print (* x x x))) (list 1 2 3 4 5))

(for-each (lambda (x) (print x)) nil) ;

(define (for-each procedure items)
  (cond ((null? items) nil)
        (else  (procedure (car items))
               (for-each procedure (cdr items)))))

; car / cdr を使うには、まず、items が nil かどうかをチェックしよう

(for-each (lambda (x) (print x)) (list 1 2 3 4 5))

(for-each (lambda (x) (print x)) nil) ;

(define x (cons (list 1 2) (list 3 4)))
x

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(length x)

(count-leaves x)

(define y (list x x))
y

(length y)

(count-leaves (list x x))

(define x (list 1 (list 2 (list 3 4))))
x

(car x)

(cdr x)

(car (cdr x))

(cdr (cdr x))

(define y (list 1 2 3 4))

(car y)

(cdr y)

(car (cdr y))

(cdr (cdr y))

; 1.

(list 1 3 (list 5 7) 9)

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

; 2.

(list (list 7))

(car (car (list (list 7))))

; 3.

(define prob3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
prob3

(car (cdr  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr prob3))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

; append :: [a] -> [a] -> [a]
(append x y) ; => (1 2 3 4 5 6)

;cons :: a -> [a] -> [a]
(cons x y) ; => ((1 2 3) 4 5 6)

;list :: a -> a -> [a]
(list x y) ; => ((1 2 3) (4 5 6))

;(define (reverse2 items)
;  (if (not (pair? items))
;      items
;      (if (null? items)
;          items
;          (append (reverse2 (cdr items)) (list (car items))))))

(define (deep_reverse items)
  (cond ((null? items) nil)
        ((pair? (car items)) (append (deep_reverse (cdr items))
                                     (list (deep_reverse (car items)))))
        (else (append (deep_reverse (cdr items))
                      (list (car items))))))

(define (d-r l)
  (cond ((null? l) l)
        ((pair? (car l)) (push (d-r (car l)) (d-r (cdr l))))
        (else (push (car l) (d-r (cdr l))))))

(define (push a l)
  (if (null? l)
      (cons a l)
      (cons (car l) (push a (cdr l)))))

(deep_reverse(list (list 1 2) (list 3 4)))

(d-r (list (list 1 2) (list 3 4)))

(define x (list (list 1 2 (list 1 2)) (list 3 4)))
x

(deep_reverse x)

(d-r x)

(define (fringe items)
  (cond ((null? items) nil)
        ((pair? (car items)) (append (fringe (car items)) (fringe (cdr items))))
        (else (append (list (car items)) (fringe (cdr items))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)

(fringe (list x x))

(fringe (list (list x 5) x))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))


(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
      (car (cdr branch)))

(define mobile1 (make-mobile (make-branch 1 5) (make-branch 1 5)))
(define mobile2 (make-mobile (make-branch 1 15) (make-branch 3 5)))

(define mobile3  (make-mobile (make-branch 2 mobile1)
                              (make-branch 1 mobile2)))

(define mobile4  (make-mobile (make-branch 2 mobile1)
                              (make-branch 2 mobile2)))

; b.

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        structure
        (total-weight structure))))


(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(total-weight mobile1)

(total-weight mobile2)

(total-weight mobile3)

; c.

;(define (balanced? mobile)
;  (let [(left-weight (if (pair? (branch-structure (left-branch mobile)))
;                        (total-weight (branch-structure (left-branch mobile)))
;                        (branch-structure (left-branch mobile))))
;        (right-weight (if (pair? (branch-structure (right-branch mobile)))
;                          (total-weight (branch-structure (right-branch mobile)))
;                          (branch-structure (right-branch mobile))))]
;
;      (= (* left-weight ; left weight
;            (branch-length (left-branch mobile))) ; left length
;         (* right-weight ; right weight
;            (branch-length (right-branch mobile)))))) ; right length


;(define (balanced? mobile)
;
;  (and (= (* (total-weight (left-branch mobile)))
;          (* (total-weight (right-branch mobile))))
;       (balanced? (left-branch mobile))
;       (balanced? (right-branch moible))))


(define (balanced? struct)
  (if (pair? struct)
      (and (= (* (branch-weight (left-branch struct)) (branch-length (left-branch struct)))
              (* (branch-weight (right-branch struct)) (branch-length (right-branch struct))))
           (balanced? (branch-structure (right-branch struct)))
           (balanced? (branch-structure (left-branch struct))))
      #t))


(balanced? mobile1)

(balanced? mobile2)

(balanced? mobile3)

(balanced? mobile4)

; d.

(define (make-mobile_2 left right) (cons left right))

(define (make-branch_2 length structure)
  (cons length structure))


(define mobile1_2 (make-mobile_2 (make-branch_2 1 5) (make-branch_2 1 5)))

(define mobile2_2 (make-mobile_2 (make-branch_2 1 15) (make-branch_2 3 5)))

(define mobile3_2  (make-mobile_2 (make-branch_2 2 mobile1_2)
                              (make-branch_2 1 mobile2_2)))


(print mobile1_2)
(print mobile2_2)
(print mobile3_2)

(print mobile1)
(print mobile2)
(print mobile3)

(define (left-branch mobile)
  (car mobile))


(define (right-branch mobile)
  (cdr mobile))


(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))


(left-branch mobile2_2)

(right-branch mobile2_2)

(branch-length (left-branch mobile2_2))

(branch-structure (left-branch mobile2_2))

(total-weight mobile3_2)

(balanced? mobile1_2)

(balanced? mobile3_2)

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; Another way to implement ***scale-tree***

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
                    (scale-tree sub-tree factor)
                    (* sub-tree factor)))
         tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))


(square-tree
     (list 1
           (list 2 (list 3 4) 5)
           (list 6 7)))

(square-tree (list 1 2 3 4))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
  tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
     (list 1
           (list 2 (list 3 4) 5)
           (list 6 7)))

(square-tree (list 1 2 3 4))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (print rest)
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

(subsets (list 1 2 3 4))
