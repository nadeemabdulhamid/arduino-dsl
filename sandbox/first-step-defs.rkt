#lang at-exp racket

(provide display-sketch
         define-input-pin
         define-output-pin
         states
         initial-state
         states-outputs)



; An InputPair  is (list Symbol Number)   i.e.  '(Symbol Number)
; An OutputPair is (list Symbol Number)   i.e.  '(Symbol Number)
; A  State      is (list Symbol)          i.e.  '(Symbol)

; (sketch [Listof InputPair] [Listof OutputPair] [Listof States]
(define-struct sketch (inputs outputs states) #:mutable #:transparent)
(define GS (make-sketch '() '() '()))

(define-struct rule (symbol input) #:mutable #:transparent)
(define RULES (make-rule '() '()))



;;Listof States -> String
;example input:
       ;(define state-outputs '(EVEN_ON, ADD_ON, RETURN_ODD, RETURN_EVEN))
; example output:
#|
        enum State{
           EVEN_ON,
           ODD_ON,
           RETURN_ODD,
           RETURN_EVEN
        }
|#
(define (states a-los)
  (define strs
      (string-append "enumState{"
                     (for/list ([ip  a-los])
                        (symbol->string(first ip)))))
  (string-join strs "\n"))


;;State -> String
(define (initial-state a-state)
  (format "currentState= ~a;" (symbol->string(a-state))))




;; Listof Rules -> String 
;example input
#|
     '( (EVEN_ON p2)
        (ODD_ON p1)
        (RETURN_ODD )
        (RETURN_EVEN p1 p2) ))
|#

;example output
#|
     if (currentState == EVEN_ON) {
        digitalWrite(p1, LOW);
        digitalWrite(p2, HIGH);
     } else if (currentState == ODD_ON) {
        digitalWrite(p1, HIGH);
        digitalWrite(p2, LOW);
     } else if (currentState == RETURN_ODD) {
        digitalWrite(p1, LOW);
        digitalWrite(p2, LOW);
     } else if (currentState == RETURN_EVEN) {
        digitalWrite(p1, HIGH);
        digitalWrite(p2, HIGH);
|#
(define (states-outputs rules)
   0)




; Symbol Number Sketch -> Void
(define (define-input-pin sym num [sk GS])
  (define cur-inputs (sketch-inputs sk))
   ;  List curInputs = sk.inputs;
  
  (set-sketch-inputs! sk (cons `(,sym ,num) cur-inputs))
   ; sk.inputs =  new Pair(sym, num) + curInputs;
  )

(define (define-state-list sym [sk GS])
  (define cur-states (sketch-states sk))
  (set-sketch-states! sk (cons `(,sym) cur-states)))


#|(define (define-rules sym inp [r RULES])
  (define cur-rules (rule r))
  (set-rule! r (cons `(,sym ,inp) cur-rules)))
|#

  (define (define-rule-symbols sym [r RULES])
    (define cur-symbols (rule-symbol r))
    (set-rule-symbol! r (cons `(,sym) cur-symbols)))

;; rule inputs = listof inputs
(define (define-rule-inputs loi [r RULES])
    (define cur-inputs (rule-input r))
    (set-rule-input! r (cons `(,loi) cur-inputs)))



;Symbol Number Sketch -> Void
(define (define-output-pin sym num [sk GS])
  (define cur-outputs (sketch-outputs sk))
   ;  List curOutputs = sk.Outputs;
  
  (set-sketch-outputs! sk (cons `(,sym ,num) cur-outputs))
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
    (for/list ([ip loi])
      |#

(define (display-sketch [sk GS])
  (display
  @stringify{
#include <Bounce2.h>

@build-inputs-declarations[(sketch-inputs sk)]
@build-outputs-declarations[(sketch-outputs sk)]


enum State{
   EVEN_ON,
   ODD_ON,
   RETURN_ODD,
   RETURN_EVEN
   };


             
void setup() {

@build-inputs-setup[(sketch-inputs sk)]
@build-outputs-setup[(sketch-outputs sk)]
  
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



;;State currentState = EVEN_ON;
@initial-state[(...?)]

@states[(sketch-states sk)]
|#



