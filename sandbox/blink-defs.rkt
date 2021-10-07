#lang at-exp racket

(provide input-pin display-sketch)



(define-struct sketch (input-pin) #:mutable)

(define GS (make-sketch 13))

(define (stringify . stuff)
  (apply string-append (map (λ(s) (~a s)) stuff)))


(define (display-sketch [sk GS])
  (display
  @stringify{
/*
  Blink
*/

void setup() {
  pinMode(@sketch-input-pin[sk], OUTPUT);
}

void loop() {
  digitalWrite(@sketch-input-pin[sk], HIGH);
  delay(1000);
  digitalWrite(@sketch-input-pin[sk], LOW);
  delay(1000);
}
 }))


(define (input-pin i)
  (set-sketch-input-pin! GS i))
