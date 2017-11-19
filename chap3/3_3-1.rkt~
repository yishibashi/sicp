#lang planet neil/sicp

(define (append! x y)
    (set-cdr! (last-pair x) y)
    x)


(define (last-pair x)
    (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
     (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
;w


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 共有とアイデンティティ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (list 'a 'b))
(define z1 (cons x x)) ; car と cdr によって x が共有できるようになっている。
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w1 (cons 'a (cons 'b nil)))
(define z4 (cons w1 (cdr w1)))


(define a (cons 'a nil))
(define b (cons a a))
(define z7 (cons b b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-pairs x)
    (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define (in e l)
  (cond ((null? l) #f)
        ((eq? e (car l)) #t)
        (else (in e (cdr l)))))


; ex 3.17
(define (m-count-pairs p)
  (let ((paths '()))
    (define (count p)
      (cond ((null? p) 0)
            ((not (pair? p)) 0)
            ((in p paths) (+ (count (car p)) (count (cdr p))))
            (else (set! paths (cons p paths))
                  ;(write paths)
                  (+ 1 (count (car p)) (count (cdr p))))))
    (count p)))

;(m-count-pairs z4)
;(m-count-pairs z7)

;;;;;

; ex 3.18
(define (loop? l)
  (define (check l2)
    (cond ((null? l2) #f)
          ((eq? l l2) #t)
          (else (check (cdr l2)))))
  (check (cdr l)))



(define loop1 (make-cycle (list 1 2 3 4 5)))

;(loop? loop1) ; #t
;(loop? (list 1 2 3 4 5)) ; #f

(define loop2 (cons 1 (cons 2 (make-cycle (list 3 4 5 6)))))
; (loop? loop2) ; <= loop.


; ex 3.19

(define (loop?? l)
  (define (check l2 l3)
    (cond ((null? l2) #f)
          ((eq? l2 l3) #t)
          (else (check (cdr l2) (cddr l3)))))
  (cond ((not (pair? l)) #f)
        ((null? (cddr l)) #f)
        (else (check (cdr l) (cddr l)))))


;(loop?? loop2) ; #t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; QUEUE


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
         (else (set-front-ptr! queue (cdr (front-ptr queue)))
               queue)))

(define q (make-queue))
;(insert-queue! q 'a)
;(insert-queue! q 'b)
;(delete-queue! q)
;(insert-queue! q 'c)
;(insert-queue! q 'd)
;(delete-queue! q)

;; ex3.21
(define (print-queue queue)
  (car queue))


;(print-queue q)



;; ex3.22

(define (make-queue-dispatch)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (print)
      (display front-ptr))
    (define (insert i)
      (let ((new-pair (cons i '())))
        (cond ((empty?) (set! front-ptr new-pair)
                        (set! rear-ptr new-pair))
              (else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)))))
    (define (delete)
      (cond ((empty?) (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (dispatch m . args)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'front) (front))
            ((eq? m 'insert!) (insert (car args)))
            ((eq? m 'print) (print))
            ((eq? m 'delete!) (delete))))
    dispatch))
            

;(define qq (make-queue-dispatch))
;(qq 'insert! '1)
;(qq 'insert! '2)
;(qq 'print)
;(newline)
;(qq 'delete!)
;(qq 'print)

;; ex3.23

(define (make-deque)
  (cons '() '()))

(define (make-item val)
  (cons '() (cons val '())))

(define (prev-ptr item)
  (car item))
(define (next-ptr item)
  (cddr item))

(define (value item)
  (cadr item))

;selecter
(define (front-deque dq)
  (car dq))
(define (rear-deque dq)
  (cdr dq))

;pred
(define (empty-deque? dq)
  (null? (front-deque dq)))

;mu
;(define (front-insert-deque dq item)
;  (set-car! item (front-deque dq))
;  (set-car! dq (prev-ptr item)))



