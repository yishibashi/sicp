#lang planet neil/sicp
(require racket/stream)
; 3.5 STREAMS

; 3.5.1 Streams Are Delayed Lists

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-stream s))
(define (display-line x) (newline) (display x))

; (cons-stream a b) == (cons a (delay b))
(define (stream-car stream) (caar stream))
(define (stream-cdr stream) (force (cdr stream)))

; (delay exp)  == (lambda () exp))
(define (force delayed-object) (delayed-object))


(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? true)
               result)
        result))))

; Ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) ; 引数のstreamの内（とりあえず）先頭のものがnullかチェックしている。
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams)) ; argstreams内の各streamの先頭要素を取って,それに procをapply
       (apply stream-map ; argstreamsの各streamのcdrを取って再帰
              (cons proc (map strea-cdr argstreams))))))
; apply : (apply procedure obj, ..., list) -> (precedure obj, ..., list)

; Ex 3.51
; (define x ...) した時に 0 だけ表示.
; 1, ..., 5
; 1, ..., 7 (memo化したら 6,7)
; ?????????????

; Ex 3.52

; つらい



; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ;
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ;

; 3.5.2 Infinite Streams

; Even though we manipulating stream as conplete entities(or sequence), we compute only
; as much of the streams as we need to access.

; We can use streams to represent sequences that are infinitely long.

; example) stream of (infinite) positive integers.
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; (define s (integers-starting-from 5))
; (stream-car s) -> 5
; (stream-cdr s) -> (integers-starting-from 6)

; This makes sense. This is an infinitely long stream, but in any given
; time we can examine only a finite protion of it.

; Using 'integers' we can define other infinite streams.
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
; (stream-ref no-sevens 100) -> 117

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define (fibs (fibgen 0 1)))


(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

; Maybe this is not 'genuine' sieve of eratosthenese.
; https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
; http://d.hatena.ne.jp/kazu-yamamoto/20100624/1277348961
(define (primes (sieve (integers-starting-from 2))))

; Henderson diagram

; unconser: cons a b -- unconser --> a, b 
; a is used to construc a divisibility filter.



; Defining streams implicitly


; ex 3.53
; 1, 2, 4, 8, 16

; ex 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))


; ex 3.55


; ex 3.56
(define (partial-sums S)
  (cons-stream (car s)
               (cons-stream (+ (car S) (cadr S))
                            (cdr S))))


; ex 3.57
; ex 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))


; ex 3.59
(define (integrate-series s c)
  (define (sub-series s2 i)
	(cons-stream c
		(cons-stream (* (/ 1 (car i))
				(car s2))
				(sub-series (cdr s2) (cdr i)))))
  (sub-series s integers))



; ex 3.60
(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-stream (scale-stream (stream-car s1) s2)
                           (mul-series (stream-cdr s1) (stream-cdr s2)))))
; ex 3.61
(define (invert-unit-series s)
  (cons-stream 1.0
	       (stream-map - (mul-series (cdr X) (invert-unit-series s)))))

(define (invert-unit-series s)
    (define x
        (cons-stream 1
                     (scale-stream -1
                        (mul-stream (stream-cdr s) x))))
    x
)


; ex 3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "THE DENOMINATOR SERIES MUST BEGINS WITH A NONZERO CONSTANT TERM")
    (mul-series s1 (invert-unit-series s2))))


(define (div-series s1 s2)
  (cond ((= (stream-car s2) 0)
	 (error "ZERO DIVISION ERROR"))
	((= (stream-car s2) 1)
	 (mul-series s1 (invert-unit-series s2)))
	(else
	  (let ((inv (/ 1 (car s2)))
		(vni (car s2)))
	    (scale-stream vni
		(mul-series s1 
			    (invert-unit-series (scale-stream inv s2))))))))




(define tan-series (div-series sin-series cos-series))


; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
; 3.5.3 

(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guess
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
		  guesses)))
  guesses)

(display-stream (sqrt-stream 2))


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(display-stream pi-stream)


;-------------------

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelarated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display-stream 
  (accelarated-sequence euler-transform pi-stream))


; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

; ex 3.63
(define (sqrt-stream x)
  (cons-stream 1.0
	       (stream-map 
		 (lambda (guess)
		   (sqrt-improve guess x))
		 (sqrt-stream x))))
	; (define (sqrt-improve guess x)
	;   (average guess (/ x guess)))
	; (define (sqrt-stream x)
	;   (define guesses
	;     (cons-stream
	;       1.0
	;       (stream-map (lambda (guess) (sqrt-improve guess x))
	; 		  guesses)))
	;   guesses)

; 以前に勉強した環境モデルを考えればよい（？）

; ex 3.64

(define (stream-limit s t)
  (let ((fst (stream-car s))
	(scd (stream-car (sctream-cdr s))))
    (if (< (abs (- fst scd)) t)
      scd
      (stream-limit (stream-cdr s) t))))

; ex 3.65

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1)))))

(define ln2
  (partial-sums (ln2-summands 1)))



; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
		 (stream-append (stream-cdr s1) s2))))
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
		 (interleave s2 (stream-cdr s1)))))

; ex 3.66

; ex 3.67
; yishibashi

(define (pairs-all s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
	(stream-map (lambda (x) (list (stream-car s) x))
			(stream-cdr t))
	(stream-map (lambda (x) (list (sream-car t) x))
		    (stream-cdr s)))
      (pairs-all (stream-cdr s) (stream-cdr t)))))
; 本文に書かれていたpairsに足りないのは(i, j) (i \in S, j \in T s.t. i > j)
; なので (stream-map (lambda (x) (list (stream-car t) x)) (stream-cdr s))) を追加すればいい


; ex 3.68
; そもそも cons-stream が無いので評価が終わらない。

; ex 3.69

; ex 3.70

(define (merge-weighted ps1 ps2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	  (let* ((s1car (stream-car s1))
		 (s2car (stream-car s2))
	         (s1w (weight s1car))
		 (s2w (weight s2car)))
	    (cond ((< s1w s2w)
		   (cons-stream
		     s1car
		     (merge-weighted (stream-cdr s1) s2)))
		  ((> s1w s2w)
		   (cons-stream
		     s2car
		     (merge-weighted s1 (stream-cdr s2))))
		  (else
		    (cons-stream
		      s1car
		      (cons-stream
			s2car
			(merge-weighted (stream-cdr s1)
					(stream-cdr s2))))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (merge-weighted
	(stream-map (lambda (x) (list (stream-car s) x))
		    (stream-cdr t))
	(stream-map (lambda (x) (list (stream-car t) x))
		    (stream-cdr s))
	weight)
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))

; ex 3.71
; yishibashi
; machigai
(define (cubic-weight pair)
  (let ((a (car pair))
	(b (cadr pair)))
    (+ (* a a a)
       (* b b b))))

(define cubic-numbers
  (weighted-pairs integers integers cubic-weight))

(define (ramanujan-numbers cs cw)
  (define (iter s val)
    (let ((fst (stream-car s)))
	(if (= (cw fst) val)
	  (cons fst (iter (stream-cdr s) val))
	  '())))
  (let* ((fst (stream-car cs))
	 (val (cw fst)))
    (cons-stream
      (iter cs val)
      (ramanujan-number 
	(stream-filter (lambda (x) (< val x)) cs)
	cw))))


; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ;
; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ ;
; Stream as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)



; ex 3.73

(define (RC R C dt)
  (define (proc i v)
    (add-stream (scale-stream i R)
                (integral (scale-stream i (/ 1 C)) v dt)))
  proc)

; ex 3.74

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))
(define zero-crossings
        (make-zero-crossings sense-data 0))


;;;

(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

; ex 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings
        (stream-cdr input-stream) (stream-car input-stream) avpt))))


; ex 3.78

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))




