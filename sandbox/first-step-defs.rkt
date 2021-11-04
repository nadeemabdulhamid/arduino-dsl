#lang at-exp racket

(require rackunit
         2htdp/batch-io)

(provide display-sketch
         write-sketch
         define-input-pin
         define-output-pin
         states
         initial-state
         states-outputs
         transitions
         GS)


; An InputPair  is (list Symbol Number)   i.e.  '(Symbol Number)
; An OutputPair is (list Symbol Number)   i.e.  '(Symbol Number)
; A  State      is (list Symbol)          i.e.  '(Symbol)

; (sketch [Listof InputPair] [Listof OutputPair] [Listof States]  State   [Listof Rule]
(define-struct sketch (inputs outputs states init rules trans) #:mutable #:transparent)
(define GS (make-sketch '() '() '() #f '() '()))

(define-struct rule (state pins) #:mutable #:transparent)
;;;;(define RULES (make-rule '() '()))

(define-struct transition (cur inp next) #:mutable #:transparent)


;                                                        
;                                                        
;                                                        
;                                                        
;     ;;; ;  ;;;;;;;    ;;     ;;;;;;;  ;;;;;;;    ;;; ; 
;    ;   ;;  ;  ;  ;     ;     ;  ;  ;   ;    ;   ;   ;; 
;    ;    ;  ;  ;  ;    ; ;    ;  ;  ;   ;  ; ;   ;    ; 
;     ;;;;   ;  ;  ;    ; ;    ;  ;  ;   ;;;;      ;;;;  
;         ;     ;      ;   ;      ;      ;  ;          ; 
;    ;    ;     ;      ;;;;;      ;      ;    ;   ;    ; 
;    ;;   ;     ;     ;     ;     ;      ;    ;   ;;   ; 
;    ; ;;;    ;;;;;  ;;;   ;;;  ;;;;;   ;;;;;;;   ; ;;;  
;                                                        
;                                                        
;                                                        
;

;;State -> String
   ;example input:
       ; (initial-state 'EVEN_ON)
   ;example output:
       ; State currentState = EVEN_ON;

(define (initial-state a-state [sk GS])
  (set-sketch-init! sk a-state))
 ; (format "currentState= ~a;" (symbol->string(a-state))))



;;Listof States -> String
   ;example input:
       ;(define state-outputs '(EVEN_ON, ADD_ON, RETURN_ODD, RETURN_EVEN))
   ; example output:
#|       enum State{
             EVEN_ON,
             ODD_ON,
             RETURN_ODD,
             RETURN_EVEN
         }|#

(define (states a-los [sk GS])
  (set-sketch-states! sk a-los))

(define (generate-states-def a-los)
  (define strs (map symbol->string a-los))
  ;    (for/list ([s a-los])
  ;      (symbol->string s)))
  (string-append "enum State {" (string-join strs ",\n") "};"))



;; Symbol Sketch -> Void 
(define (define-state-list sym [sk GS])
  (define cur-states (sketch-states sk))
  (set-sketch-states! sk (cons `(,sym) cur-states)))


;                                               
;                                               
;                                               
;                                               
;   ;;;;;    ;;;  ;;; ;;;;;    ;;;;;;;    ;;; ; 
;    ;   ;    ;    ;    ;       ;    ;   ;   ;; 
;    ;   ;    ;    ;    ;       ;  ; ;   ;    ; 
;    ;   ;    ;    ;    ;       ;;;;      ;;;;  
;    ;;;;     ;    ;    ;   ;   ;  ;          ; 
;    ;   ;    ;    ;    ;   ;   ;    ;   ;    ; 
;    ;    ;   ;    ;    ;   ;   ;    ;   ;;   ; 
;   ;;;   ;;   ;;;;   ;;;;;;;  ;;;;;;;   ; ;;;  
;                                               
;                                               
;                                               
;                                               


; ListOf rules -> Sketch
;; updates the sketch-rules with the outputs given by the user
;example input
#|
     '( (EVEN_ON p2)
        (ODD_ON p1)
        (RETURN_ODD )
        (RETURN_EVEN p1 p2) ))
|#
; the output is just the updated sketch 
(define (states-outputs rules [sk GS])
  (set-sketch-rules! sk (for/list ([r rules])
                          (rule (first r) (rest r))))
  sk)

(check-equal? (states-outputs '() (make-sketch '() '() '() #f '() '()))
              (make-sketch '() '() '() #f '() '()))
(check-equal? (states-outputs '( (EVEN_ON p1 p2))
                              (make-sketch '() '() '() #f '() '()))
              (make-sketch '() '() '() #f (list (rule 'EVEN_ON '(p1 p2))) '()))


;; Sketch -> String
;; uses sketch-rules to express the rules needed for each state 
;input is just the current sketch

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
(define (express-rules sk)

  
  (define all-pins (map first (sketch-outputs sk)))  ; listof symbol
  
  (define (express-write-for cur-pin rule-pins)
    (format "digitalWrite(~a, ~a);"
                 cur-pin
                 (if (member cur-pin rule-pins) "HIGH" "LOW")))

  (string-join
   (for/list ([rule (sketch-rules sk)])  ; for each rule in rules provided by user 
     (define pin-settings  
       (for/list ([p all-pins]) (express-write-for p (rule-pins rule)))) ; for each output pin,  if they are declared in state outputs-> turn them HIGH, otherwise, LOW
     (format "if (currentState == ~a) {~a}"
             (rule-state rule)  ; each rule in the sketch-rules
             (string-join pin-settings "\n")))
   "\n"))


; (define-struct transition (cur inp next) #:mutable #:transparent)



(define (transitions a-lot [sk GS])
  (set-sketch-trans! sk (for/list ([t a-lot])
                          (transition (first t) (second t) (third t)))))


(define (express-transitions [sk GS])
  (string-append
  (string-join
    (for/list ([b (sketch-inputs sk)])
      (format  "debouncer_~a.update(); \n boolean ~a_pressed = debouncer_~a.fell();"
              (first b)(first b) (first b))) "\n")

  "\n"
  (format "if (currentState == ~a && ~a_pressed) { currentState = ~a;}" ; builds the first if statement
          (transition-cur (first (sketch-trans sk)))
          (transition-inp (first (sketch-trans sk)))
          (transition-next(first (sketch-trans sk))))
  "\n"
  (string-join
  (for/list ([t (sketch-trans sk)])
      (format "else if (currentState == ~a && ~a_pressed){ currentState = ~a;}" ; does the same thing but is now else if statements 
              (transition-cur t)
              (transition-inp t)
              (transition-next t))) "\n")))


#|

void loop() {
  bool btnA_pressed =  debouncer_btnA.fell();
  bool btnB_pressed =  debouncer_btnB.fell();


 ....


  if (currentState == EVEN_ON && btnA_pressed) { currentState = ODD_ON; }
  else if (currentState == ODD_ON && btnA_pressed) { currentState = EVEN_ON; }
  ...


|#


;                                               
;                                               
;                                               
;                                               
;   ;;;;;;;  ;;;  ;;; ;;;;;;   ;;;  ;;; ;;;;;;; 
;      ;      ;;   ;   ;    ;   ;    ;  ;  ;  ; 
;      ;      ; ;  ;   ;    ;   ;    ;  ;  ;  ; 
;      ;      ; ;  ;   ;    ;   ;    ;  ;  ;  ; 
;      ;      ;  ; ;   ;;;;;    ;    ;     ;    
;      ;      ;  ; ;   ;        ;    ;     ;    
;      ;      ;   ;;   ;        ;    ;     ;    
;   ;;;;;;;  ;;;  ;;  ;;;;;      ;;;;    ;;;;;  
;                                               
;                                               
;                                               
;                                               

; Symbol Number Sketch -> Void
(define (define-input-pin sym num [sk GS])
  (define cur-inputs (sketch-inputs sk))
  (set-sketch-inputs! sk (cons `(,sym ,num) cur-inputs))
  )



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
      (format "debouncer_~a.attach(~a, INPUT);" (first ip)(first ip))))
  (string-join strs "\n"))



;                                                        
;                                                        
;                                                        
;                                                        
;     ;;;;   ;;;  ;;; ;;;;;;;  ;;;;;;   ;;;  ;;; ;;;;;;; 
;    ;    ;   ;    ;  ;  ;  ;   ;    ;   ;    ;  ;  ;  ; 
;   ;      ;  ;    ;  ;  ;  ;   ;    ;   ;    ;  ;  ;  ; 
;   ;      ;  ;    ;  ;  ;  ;   ;    ;   ;    ;  ;  ;  ; 
;   ;      ;  ;    ;     ;      ;;;;;    ;    ;     ;    
;   ;      ;  ;    ;     ;      ;        ;    ;     ;    
;    ;    ;   ;    ;     ;      ;        ;    ;     ;    
;     ;;;;     ;;;;    ;;;;;   ;;;;;      ;;;;    ;;;;;  
;                                                        
;                                                        
;                                                        
;                                                        


;Symbol Number Sketch -> Void
(define (define-output-pin sym num [sk GS])
  (define cur-outputs (sketch-outputs sk))
   ;  List curOutputs = sk.Outputs;
  
  (set-sketch-outputs! sk (cons `(,sym ,num) cur-outputs))
   ; sk.inputs =  new Pair(sym, num) + curOutputs;
  )


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

;                                                                 
;                                                                 
;                                                                 
;                                                                 
;   ;;;  ;;; ;;;;;;;  ;;;;;    ;;;;;;   ;;;;;;;  ;;;;;      ;;; ; 
;    ;    ;   ;    ;    ;       ;    ;   ;    ;   ;   ;    ;   ;; 
;    ;    ;   ;  ; ;    ;       ;    ;   ;  ; ;   ;   ;    ;    ; 
;    ;;;;;;   ;;;;      ;       ;    ;   ;;;;     ;   ;     ;;;;  
;    ;    ;   ;  ;      ;   ;   ;;;;;    ;  ;     ;;;;          ; 
;    ;    ;   ;    ;    ;   ;   ;        ;    ;   ;   ;    ;    ; 
;    ;    ;   ;    ;    ;   ;   ;        ;    ;   ;    ;   ;;   ; 
;   ;;;  ;;; ;;;;;;;  ;;;;;;;  ;;;;;    ;;;;;;;  ;;;   ;;  ; ;;;  
;                                                                 
;                                                                 
;                                                                 
;                                                                 

; (listof Any) -> String
(define (stringify . stuff)
  (apply string-append (map (λ(s) (~a s)) stuff)))









;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                                                                              
;   ;;;;;;;  ;;;;;;;  ;;;  ;;;   ;;     ;;;;;               ;;; ;  ;;; ;;;  ;;;;;;;  ;;;;;;;    ;;; ;  ;;;  ;;;
;    ;    ;     ;      ;;   ;     ;       ;                ;   ;;   ;   ;    ;    ;  ;  ;  ;   ;   ;;   ;    ; 
;    ;  ; ;     ;      ; ;  ;    ; ;      ;                ;    ;   ;  ;     ;  ; ;  ;  ;  ;  ;     ;   ;    ; 
;    ;;;;       ;      ; ;  ;    ; ;      ;                 ;;;;    ;;;      ;;;;    ;  ;  ;  ;         ;;;;;; 
;    ;  ;       ;      ;  ; ;   ;   ;     ;   ;                 ;   ;  ;     ;  ;       ;     ;         ;    ; 
;    ;          ;      ;  ; ;   ;;;;;     ;   ;            ;    ;   ;   ;    ;    ;     ;     ;         ;    ; 
;    ;          ;      ;   ;;  ;     ;    ;   ;            ;;   ;   ;    ;   ;    ;     ;      ;    ;   ;    ; 
;   ;;;;     ;;;;;;;  ;;;  ;; ;;;   ;;; ;;;;;;;            ; ;;;   ;;;   ;; ;;;;;;;   ;;;;;     ;;;;   ;;;  ;;;
;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                                                                              



(define (display-sketch [sk GS])
  (display (full-sketch-string sk)))

(define (write-sketch filepath [sk GS])
 (write-file filepath (full-sketch-string sk) ))

(define (full-sketch-string sk)
  @stringify{
#include <Bounce2.h>

@build-inputs-declarations[(sketch-inputs sk)]
@build-outputs-declarations[(sketch-outputs sk)]

@generate-states-def[(sketch-states sk)]

State currentState;

             
void setup() {

@build-inputs-setup[(sketch-inputs sk)]
@build-outputs-setup[(sketch-outputs sk)]
  
  @format["currentState= ~a;" (symbol->string (sketch-init sk))]
}

void loop() {
  @express-rules[sk]

  @express-transitions[sk]
}
 })











;; "testing"....
#|
(define-input-pin 'btnA 2)   ; digital input, debounced by default
(define-input-pin 'btnB 3)
(define-output-pin 'p1 8)
(define-output-pin 'p2 9)



;;State currentState = EVEN_ON;
@initial-state[(...?)]
|#

