#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(paint einstein)

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
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@    @@@@@@@      ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@    @@          ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    @@@@@@@    ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@       @@   ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@    @@@@@@@  ;;;;;;;;;;
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
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@    @@@@@@@      ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@    @@          ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    @@@@@@@    ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@  @@   @@   ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@    @@@@@@@  ;;;;;;;;;;
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
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@    @@@@@@@@     ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@    @@    @@    ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@         @@    ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@      @@    ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@       @@    ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
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
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@    @@@@@@@@     ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@    @@    @@    ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    @@@@@@@@   ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@  @@    @@  ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@    @@@@@@@@ ;;;;;;;;;;
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@       @@@@@    @@@@@@@@     ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@      @@  @@    @@    @@    ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@   @@    @@@@@@@@   ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@       @@@@@@@@@@        @@  ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @       @@          @@ ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; a.
(define (paint-frame frame)
  (let ((o (origin-frame frame))
        (e1 (edge1-frame frame))
        (e2 (edge2-frame frame)))
    (segments->painter
     (list (make-segment o (add-vect o e1))
           (make-segment o (add-vect o e2))
           (make-segment (add-vect o e1) e2)
           (make-segment (add-vect o e2) e1)))))
;;
(define f1 (make-frame (list 0.0 0.0)
                       (list 0.0 0.5)
                       (list 0.25 0.0)))
;;                     
(paint (paint-frame f1))


; b.

(define draw-X
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) make-vect(0.9 0.9))
         (make-segment (make-vect 0.9 0.0) make-vect(0.0 0.9)))))

; c.

; d.


(define my-wave
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Transforming and combining painters

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2 new-origin))))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


   
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (point-right frame)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@     @@@@@@@   @@@@@@@@      ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@     @@        @@    @@     ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@@@@@@   @@    @@    ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@             @@   @@    @@   ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @  @@@@@@@   @@@@@@@@  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

                

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@     @@@@@@@       @@        ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@     @@          @@@@       ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@@@@@@       @@      ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@             @@       @@     ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @  @@@@@@@     @@@@@@  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (point-right frame)))))


(define (below1 p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-upper
           (transform-painter split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-lower
           (transform-painter (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-upper frame)
        (paint-lower frame)))))


(define (below2 p1 p2)
  (rotate90 (beside (rotate270 p1) (rotate270 p2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Levels of language for robust design



                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; @@@@@@@@  @@    @@     @@@@@@@     @@@@@@@   @@@@@@@       ;;;;;;
;;;;;;;;;;;;;;  @@         @@  @@      @    @@     @@        @    @@      ;;;;;;;
;;;;;;;;;;;;;   @@@@@@      @@@@          @@@      @@@@@@@      @@@      ;;;;;;;;
;;;;;;;;;;;;    @@         @@  @@       @@@             @@    @@@       ;;;;;;;;;
;;;;;;;;;;;     @@@@@@@@  @@    @@  @  @@@@@@@  @  @@@@@@@   @@@@@@@   ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; a.
; b.
; c.

