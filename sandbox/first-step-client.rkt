#lang racket


(require "first-step-defs.rkt")

(define-input-pin 'btnA 2)   ; digital input, debounced by default
(define-input-pin 'btnB 3)

(define-output-pin 'p1 8)
(define-output-pin 'p2 9)

;(states `(EVEN_ON   ODD_ON   RETURN_ODD  RETURN_EVEN))

;(initial-state 'EVEN_ON)



(display-sketch)



