#lang racket

(require "first-step-defs.rkt")

(define-input-pin 'btnA 2)   ; digital input, debounced by default
(define-input-pin 'btnB 3)

(define-output-pin 'p1 8)
(define-output-pin 'p2 9)

(states `(EVEN_ON   ODD_ON   RETURN_ODD  RETURN_EVEN))

(initial-state 'EVEN_ON)

(states-outputs '( (EVEN_ON p2)
                   (ODD_ON p1)
                   (RETURN_ODD )
                   (RETURN_EVEN ) ))

(transitions '( (EVEN_ON btnA ODD_ON)
                (ODD_ON btnA EVEN_ON)
                (EVEN_ON btnB RETURN_EVEN)
                (ODD_ON btnB RETURN_ODD)
                (RETURN_EVEN btnB EVEN_ON)
                (RETURN_ODD btnB ODD_ON)))

#|

void loop() {
  bool btnA_pressed =  debouncer_btnA.fell();
  bool btnB_pressed =  debouncer_btnB.fell();


 ....


  if (currentState == EVEN_ON && btnA_pressed) { currentState = ODD_ON; }
  else if (currentState == ODD_ON && btnA_pressed) { currentState = EVEN_ON; }
  ...


|#


(display-sketch)



