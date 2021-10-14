#lang at-exp racket

(provide display-sketch define-input-pin define-output-pin)

; An InputPair is (list Symbol Number)   i.e.  '(Symbol Number)

; (sketch [Listof InputPair]
(define-struct sketch (inputs ) #:mutable #:transparent)

(define-struct sketch-output (outputs) #:mutable #:transparent)






(define GS (make-sketch '() ))
(define OS (make-sketch-output '() ))



; Symbol Number Sketch -> Void
(define (define-input-pin sym num [sk GS])
  (define cur-inputs (sketch-inputs sk))
   ;  List curInputs = sk.inputs;
  
  (set-sketch-inputs! sk (cons `(,sym ,num) cur-inputs))
   ; sk.inputs =  new Pair(sym, num) + curInputs;
  )

;Symbol Number Sketch -> Void
(define (define-output-pin sym num [sk OS])
  (define cur-outputs (sketch-output-outputs sk))
   ;  List curOutputs = sk.Outputs;
  
  (set-sketch-output-outputs! sk (cons `(,sym ,num) cur-outputs))
   ; sk.inputs =  new Pair(sym, num) + curOutputs;
  )



; (listof Any) -> String
(define (stringify . stuff)
  (apply string-append (map (λ(s) (~a s)) stuff)))



; (listof InputPair) -> String
;; creates the global variable for the inputs
;; example output: const int btnA = 2
;;                 Bounce debouncer_btnA = Bounce();
(define (build-inputs-declarations loi)
  ;   for (InputPair ip : loi) {
  (define strs
    (for/list ([ip  loi])
      ;(string-append "const int " (symbol->string (first ip)) " = " (number->string (second ip)) ";")))
      (string-append (format "const int ~a = ~a;" (first ip) (second ip))
                     "\n" (format "Bounce debouncer_~a = Bounce();" (symbol->string(first ip))))))

      
  (string-join strs "\n"))


; (listof InputPair) -> String
;; sets up the inputs for the setup function
;; example output: pinMode(btnA, INPUT)
(define (build-inputs-setup loi)
  ;   for (InputPair ip : loi) {
  (define strs
    (for/list ([ip  loi])
      ;(string-append "pinMode( " (symbol->string (first ip)) ", INPUT);")))
      (format "pinMode(~a, INPUT);" (first ip))))

      
  (string-join strs "\n"))


; (listof OutputPair) -> String
;; creates the global variable for the outputs
;; example output: (const int p2 =9);
(define (build-outputs-declarations loi)
  ;   for (OutputPair ip : loi) {
  (define strs
    (for/list ([ip  loi])
      ;(string-append "const int " (symbol->string (first ip)) " = " (number->string (second ip)) ";")))
      (format "const int ~a = ~a;" (first ip) (second ip))))

      
  (string-join strs "\n"))


; (listof OutputPair) -> String
;; sets up the outputs for the setup function
;; example output: pinMode(p1, OUTPUT)
(define (build-outputs-setup loi)
  ;   for (OutputPair ip : loi) {
  (define strs
    (for/list ([ip  loi])
       ;(string-append "pinMode( " (symbol->string (first ip)) ", OUTPUT);")))
      (format "pinMode(~a, OUTPUT);" (first ip))))

      
  (string-join strs "\n"))


#|
;;(list of States) -> String?
;; creates an arrayList of states
(define (build-state-array los)
  (define strs
    (for/lsit ([ip loi])
      |#

(define (display-sketch [sk GS])
  (display
  @stringify{
#include <Bounce2.h>

@build-inputs-declarations[(sketch-inputs sk)]
@build-outputs-declarations[(sketch-output-outputs OS)]

enum State{
   EVEN_ON,
   ODD_ON,
   RETURN_ODD,
   RETURN_EVEN
   };

State currentState = EVEN_ON;
             
void setup() {

@build-inputs-setup[(sketch-inputs sk)]
@build-outputs-setup[(sketch-output-outputs OS)]
  
  ...
}

void loop() {
  
  delay(1000);
}
 }))










;; "testing"....
#|
(define-input-pin 'btnA 2)   ; digital input, debounced by default
(define-input-pin 'btnB 3)
(define-output-pin 'p1 8)
(define-output-pin 'p2 9)

|#



