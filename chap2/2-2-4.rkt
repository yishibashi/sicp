#lang racket

(require sicp-pict)

(define wave1
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))

(paint wave1)


(define wave2 (beside wave1 (flip-vert wave1)))
(paint wave2)

(define wave4 (below wave2 wave2))
(paint wave4)


(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4-2 (flipped-pairs wave1))
(paint wave4-2)


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@      @@@@@      ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@     @@  @@     ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    @@   @@    ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@ @@@@@@@@@@ ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@         @@  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;;;;;
(paint (up-split wave1 4))
(paint (corner-split einstein 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 4))



; higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
      (bottom (beside (bl painter) (br painter))))
    (below bottom top))))



(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(paint (flipped-pairs2 einstein))


(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit2 einstein 4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@        ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@       ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    5 ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@   ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@    ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (split r l)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split r l) painter (- n 1))))
          (r painter (l smaller smaller))))))

;;;;;;;
(define right-split2 (split beside below))
(define up-split2 (split  below beside))

(paint (right-split2 wave1 3))
(paint (up-split2 einstein 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@        ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@       ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    6 ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@   ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@    ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (list (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (list (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (list (* s (xcor-vect v))
        (* s (ycor-vect v))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@                 ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@                ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@               ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@            ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@             ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (get-origin f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

;;

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 f)
  (car f))

(define (edge1-frame2 f)
  (cadr f))

(define (edge2-frame2 f)
  (cddr f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@                 ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@                ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@       8       ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@            ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@             ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))



(define wave
  (segments->painter
   (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
         (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
         (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
         (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
         (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
         (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
         (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
         (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
         (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
         (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
         (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
         (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
         (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
         (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
         (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
         (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0)))))

(paint wave)