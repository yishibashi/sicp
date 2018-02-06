#lang racket

(define x 10)

(eval '(cons (set! x (+ x 1)) (set! x (* x 2))))