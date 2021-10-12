#lang at-exp racket



; An InputPair is (list Symbol Number)   i.e.  '(Symbol Number)

; (sketch [Listof InputPair]
(define-struct sketch (inputs ) #:mutable #:transparent)






(define GS (make-sketch '() ))



; Symbol Number Sketch -> Void
(define (define-input-pin sym num [sk GS])
  (define cur-inputs (sketch-inputs sk))
   ;  List curInputs = sk.inputs;
  
  (set-sketch-inputs! sk (cons `(,sym ,num) cur-inputs))
   ; sk.inputs =  new Pair(sym, num) + curInputs;
  )



; (listof Any) -> String
(define (stringify . stuff)
  (apply string-append (map (λ(s) (~a s)) stuff)))



; (listof InputPair) -> String
(define (build-inputs-declarations loi)
  ;   for (InputPair ip : loi) {
  (define strs
    (for/list ([ip  loi])
      ;(string-append "const int " (symbol->string (first ip)) " = " (number->string (second ip)) ";")))
      (format "const int ~a = ~a;" (first ip) (second ip))))

      
  (string-join strs "\n"))



(define (display-sketch [sk GS])
  (display
  @stringify{

@build-inputs-declarations[(sketch-inputs sk)]
             
void setup() {
  pinMode(____, INPUT);
  ...
}

void loop() {
  
  delay(1000);
}
 }))










;; "testing"....

(define-input-pin 'btnA 2)   ; digital input, debounced by default
(define-input-pin 'btnB 3)





