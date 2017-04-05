#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

  ;;;;;;;;;
 ;; 3.1 ;;
;;;;;;;;;

;;; 3.1.1 

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
              balance)
       "Insufficient funds"))

;; set
; (set! <name> <new-value>)
; <name>: Symbol.
; <new-value> Expression.

;; begin
; (begin <exp_1> <exp_2> ... <exp_k>)
; evaluate exp_i (i = 1 ...k) & return evaluation of <exp_k>

; 上のwithdraw は意図したとおりに動くが、変数balance がグローバルで定義されている
; ので、どんな手続からでもアクセスして値を調べたり変更できてしまう。

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
      "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)




;;;;;;;;;;;;;;;;;;;;


;; Exercise 3.1

(define (make-accumulator i)
  (let ((counter i))
    (lambda (x)
      (set! counter (+ counter x))
      counter)))

;; Exercise 3.2

(define (make-monitored mf)
  (let ((count 0))
    (lambda x
      (cond ((eq? (car x) 'how-many-calls?) count)
            (else (begin (set! count (+ 1 count))
                         (apply mf x)))))))
    
;; Exercise 3.3

;(define (make-account-with-psw balance password)
;  (let ((psw password))
;    (define (withdraw amount)
;      (if (>= balance amount)
;          (begin (set! balance (- balance amount))
;                 balance)
;          "Insufficient funds"))
;    (define (deposit amount)
;      (set! balance (+ balance amount))
;      balance)
;    
;    (define (dispatch p m)
;      (if (eq? password p)
;          (cond ((eq? m 'withdraw) withdraw)
;                ((eq? m 'deposit) deposit)
;                (else (error "Unknown request: MAKE-ACCOUNT" m)))
;          "Incorrect password"))
;    dispatch))


;;
    
(define (make-account-with-psw balance password)
  (let ((psw password)
        (count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define (call-the-cops) "CALL THE COPS")
    
    (define (dispatch p m)
      (if (eq? password p)
          (begin (set! count 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (begin (set! count (+ count 1))
                 (lambda (x)
                   (if (not (< count 7))
                       (call-the-cops)
                       "Incorrect password")))))
    dispatch))


;;; 3.1.2

;(define random-init 12321)

;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x)))

;(define (rand-update x)
;  (set! random-init x)
;  (set! x (remainder (+ (* 8121 x) 28411) 134456))
;  x)


;(define rand
;  (let ((x 0))
;    (lambda ()
;      (set! x (* 1.0 (random 134456)))
;      x)))


; using rand-update 
;(define (estimate-pi trials)
;  (sqrt (/ 6 (random-gcd-test trials random-init))))
;(define (random-gcd-test trials initial-x)
;  (define (iter trials-remaining trials-passed x)
;    (let ((x1 (rand-update x)))
;      (let ((x2 (rand-update x1)))
;        (cond ((= trials-remaining 0)
;               (/ trials-passed trials))
;              ((= (gcd x1 x2) 1)
;               (iter (- trials-remaining 1)
;                     (+ trials-passed 1) x2))
;              (else
;               (iter (- trials-remaining 1)
;                     trials-passed
;                     x2))))))
;  (iter trials 0 initial-x))


; using rand 
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))


;; Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* 1.0 (* (monte-carlo trials P)
            (* (abs (- x2 x1))
               (abs (- y2 y1))))))

(define pc
  (lambda ()
    (let ((x (+ -1
                (* 2 (random))))
          (y (+ -1
                (random))))
      (< (+ (* x x)
            (* y y))
         1.0))))



;; Exercise 3.6

(define rand
  (let ((x 12321))
    
    (define (rand-update x)
      (remainder (+ (* 8121 x) 28411) 134456))

    (define reset
      (lambda (n)
        (begin
          (set! x n)
          "RANDOM RESET")))

    (define (generate)
      (begin (set! x (rand-update x))
             x))
             

    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Error: command not found" m))))
    dispatch))



;;
(define RAND_MAX 2147483647)
(define (rand-update x)
  (remainder (+ (* 1103515245 x) 12345) RAND_MAX))
(define (make-rand seed)
  (define (get-rand)
    (begin (set! seed (rand-update seed))
           seed))
  (define (randint range)
    (remainder (get-rand) range))
  (define (random range)
    (* (/ (get-rand) RAND_MAX) range 1.0))
  (define (dispatch proc)
    (cond ((eq? proc 'get-rand)
           get-rand)
          ((eq? proc 'randint)
           randint)
          ((eq? proc 'random)
           random)
          (else (error "Unknown request: MAKE-RAND"
                       proc))))
  dispatch)
;;



;;; 3.1.3

; from 1.2.1 
;(define (factorial n)
;  (define (iter product counter)
;    (if (> counter n)
;        product
;        (iter (* counter product) (+ counter 1))))
;  (iter 1 1))

; imperative way.
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


;; Exercise 3.7

(define (eq-psw? p psw)
  (cond ((null? psw) #f)
        ((= (car psw) p) #t)
        (else (eq-psw? p (cdr psw)))))


(define (make-acc balance password)
  (let ((psws (list password))
        (count 0))
    ;check password
    (define (eq-psw? passwords p)
      (cond ((null? passwords) #f)
            ((eq? (car passwords) p) #t)
            (else (eq-psw? (cdr passwords) p))))
    ;withdraw
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    ;deposit
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    ;add new password
    (define (add-new-psw new)
      (set! psws (cons new psws)))
    
    ;
    ;call the cops
    (define (call-the-cops) "CALL THE COPS")
    ;dispatch
    (define (dispatch p m)
      (if (eq-psw? psws p)
          (begin (set! count 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       ((eq? m 'password?) (lambda (p) (eq-psw? psws p)))
                       ((eq? m 'add-psw) add-new-psw)
                       (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (begin (set! count (+ count 1))
                 (lambda (x)
                   (if (not (< count 7))
                       (call-the-cops)
                       "Incorrect password")))))
    dispatch))




(define (make-joint acc psw new-psw)
  (if (acc psw 'password?)
      (begin ((acc psw 'add-psw) new-psw)
             acc)
      (error "Password is'nt correct")))


;; Exercise 3.8

(define f
  (let ((m 1))
    (lambda (n)
      (if (= n 0)
          (set! m 0)
          (set! m (* m n)))
      m)))

;(+ (f 1) (f 0))
;(+ (f 0) (f 1))





  ;;;;;;;;;
 ;; 3.2 ;;
;;;;;;;;;

;;; 3.2.1

;;; 3.2.2




;; Exercise 3.9

; recursion
(define (r-factorial n)
  (if (= n 1)
      1
      (* n (r-factorial (- n 1)))))

; iteration
(define (i-factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))














