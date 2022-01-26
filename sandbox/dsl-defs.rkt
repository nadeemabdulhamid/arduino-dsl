#lang typed/racket


(module+ test
  (require rackunit))



(define-type Pin-Decl
  (List* 'pin
         Symbol
         (U 'input 'output)
         (U 'analog 'digital)
         Nonnegative-Integer
         (Listof Symbol)))

(define-type States-Decl
  (List 'states
        Symbol
        (Listof Symbol)))

(define-type Decl
  (U Pin-Decl
     States-Decl))

(define-type Stmt (Listof Any))
(define-type Prog-Tree (List (Listof Stmt)   ; globals
                             (Listof Stmt)   ; setup() body
                             (List          ; loop() body
                              (Listof Stmt)     ; output updates
                              (Listof Stmt)     ; input  updates
                              (Listof Stmt))))  ; transitions


;; EXAMPLES 
(define declrs : (Listof Decl)
  `((pin btnA input digital 2)
    (pin btnB input digital 3)
    (pin p1 output digital 8)
    (pin p2 output digital 9)

    (states main (EVEN-ON ODD-ON RETURN-ODD RETURN-EVEN))
    ))


(define blank-program : Prog-Tree
  '(() () (() () ())))


;; FUNCTIONS


(define (id-link [name : Symbol] [prop : Symbol]) : Symbol
  (string->symbol (string-append (symbol->string name) "-" (symbol->string prop))))



(define (expand-declrs [all-ds : (Listof Decl)]) : Prog-Tree
  (for/fold ([root : Prog-Tree blank-program])
            ([d all-ds])
    (expand-declr d all-ds root)))



(define (expand-declr [d : Decl] [all-ds : (Listof Decl)] [root : Prog-Tree]) : Prog-Tree
  (match d
    [(cons 'pin _) (expand-pin-declr d all-ds root)]
    [(cons 'states _) (expand-states-declr d all-ds root)]
    [else (printf "Unhandled ~a~n" d) root]))



(define (expand-states-declr [d : States-Decl] [all-ds : (Listof Decl)] [root : Prog-Tree]) : Prog-Tree
  (match root
    [(list glob setup loop)
     (match d
       [(list 'states id labels)
        (define new-glob
          `(,@glob
            (%enum ,(id-link id 'State) ,labels)
            (%define ,(id-link id 'State) ,(id-link id 'current) ,(first labels))))
        
        (list new-glob setup loop)])]))



(define (expand-pin-declr [d : Pin-Decl] [all-ds : (Listof Decl)] [root : Prog-Tree]) : Prog-Tree
  ;(match root
    ;[(list glob setup (list outp-upd inp-upd trans))
     (match d
       ;; digital input pin
       [(list-rest 'pin id 'input 'digital num opts)
        (expand-digital-input-pin id num opts all-ds root)]

       ;; digital output pin
       [(list-rest 'pin id 'output 'digital num opts)
        (expand-digital-output-pin id num opts all-ds root)]

       [else
        (printf "Unhandled ~a~n" d) root]))



;; (pin <id> output digital <num> <opts...>)
(define (expand-digital-output-pin [id : Symbol] num opts all-ds [root : Prog-Tree]) : Prog-Tree
  (match root
    [(list glob setup loop)
     (define new-glob
       `(,@glob
         (%define int ,id ,num)))

     (define new-setup
       `(,@setup
         (pinMode ,id OUTPUT)))

     `(,new-glob ,new-setup ,loop)
     ]))



;; (pin <id> input digital <num> <opts...>)
(define (expand-digital-input-pin [id : Symbol] num opts all-ds [root : Prog-Tree]) : Prog-Tree
  (match root
    [(list glob setup (list outp-upd inp-upd trans))
     (define incl? (has-include? glob 'Bounce2.h))
     
     (define new-glob
       `(,@(if incl? '() `((%include Bounce2.h)))
         ,@glob
         (%define int ,id ,num)
         (%define Bounce ,(id-link id 'debouncer) (Bounce))
         ))

     (define new-setup
       `(,@setup
         (%send ,(id-link id 'debouncer) attach ,id INPUT)))

     (define new-inp-upd
       `(,@inp-upd
         (%send ,(id-link id 'debouncer) update)))
        
     `(,new-glob ,new-setup (,outp-upd ,new-inp-upd ,trans))
     ]))



;; is the given header included already in the glob
(define (has-include? [glob : (Listof Any)] [header : (U Symbol String)]) : Boolean
  (for/or ([stmt glob])
    (match stmt
      [(list '%include h)
       (equal? h header)]
      [else #f])))






;;; Translating AST to C

(define indent-level (make-parameter 0))
(define-syntax-rule (incr-indent body)
  (parameterize ([indent-level (+ 2 (indent-level))])
    body))
(define (apply-indent [str : String]) : String
  (string-append (build-string (indent-level) (λ(_) #\space)) str))



(define (symbol->c [sym : Symbol]) : String
  (string-replace (symbol->string sym) "-" "_"))


(define (expr->c [expr : Any]) : String
  (match expr
    [(? number? _)
     (~a expr)]

    [(? symbol? _)
     (symbol->c expr)]

    [(? string? _)
     (format "\"~a\"" expr)]

    ; duplicate in stmt->c
    [(list* '%send obj meth params)
     (format "~a.~a(~a)"
             (symbol->c (cast obj Symbol))
             (symbol->c (cast meth Symbol))
             (string-join (map expr->c (cast params (Listof Any))) ", "))]

    ; duplicate in stmt->c
    [(list* fn params)
     (format "~a(~a)" fn (string-join (map expr->c (cast params (Listof Any))) ", "))]
    
    [else "###"]))


(define (stmt->c [stmt : Stmt]) : String
  (apply-indent
   (match stmt
     [(list '%include (? symbol? name))
      (format "#include <~a>" name)]
     [(list '%include (? string? name))
      (format "#include \"~a\"" name)]

     [(list '%define ty id expr)
      (format "~a ~a = ~a;" (symbol->c (cast ty Symbol)) (symbol->c (cast id Symbol)) (expr->c expr))]

     [(list '%enum id labels)
      (format "enum ~a { ~a };" (symbol->c (cast id Symbol))
              (string-join (map symbol->c (cast labels (Listof Symbol))) ", "))]

     ; duplicate in expr->c
     [(list* '%send obj meth params)
      (format "~a.~a(~a);"
              (symbol->c (cast obj Symbol))
              (symbol->c (cast meth Symbol))
              (string-join (map expr->c (cast params (Listof Any))) ", "))]
        
     ; duplicate in expr->c
     [(list* fn params)
      (format "~a(~a);" fn (string-join (map expr->c (cast params (Listof Any))) ", "))]
    
     [else "----"])))


(define (prog-tree->c-sketch [root : Prog-Tree]) : String
  (match root
    [(list glob setup (list outp-upd inp-upd trans))
     (string-append
      "\n"
      (string-join (map stmt->c glob) "\n")
      "\n\n"
      "void setup() {\n"
      (string-join (map (λ([s : Stmt]) (incr-indent (stmt->c s))) setup) "\n")
      "\n}\n\n"
      "void loop() {\n"
      (string-join (map (λ([s : Stmt]) (incr-indent (stmt->c s))) outp-upd) "\n")
      "\n"
      (string-join (map (λ([s : Stmt]) (incr-indent (stmt->c s))) inp-upd) "\n")
      "\n"
      (string-join (map (λ([s : Stmt]) (incr-indent (stmt->c s))) trans) "\n")
      "\n}\n\n")]))

  




(expand-declrs declrs)
  
(display
 (prog-tree->c-sketch (expand-declrs declrs)))















