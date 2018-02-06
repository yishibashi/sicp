
; 4.1.1 The Core of the Evaluator

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((equoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition expt env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond-if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((conmpound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" precedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))  
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)



; ex 4.1

;   original "list-of-values"
;   (define (list-of-values exps env)
;     (if (no-operands? exps)
;       '()
;       (cons (eval (first-operand exps) env)
;             (list-of-values (rest-operands exps) env))))


(define (list-of-values-left2right exps env)
  (if (no-operands? exps)
    '()
    (let ((left (eval (first-operand exps) env))
          (right (list-of-values2 (rest-operands exps) env)))
      (cons (left right)))))

(define (list-of-values-right2left exps env)
  (if (no-operands? exps)
    '()
    (let ((right (list-of-values2 (rest-operands exps) env))
          (left (eval (first-operand exps) env)))
      (cons (left right)))))


; 4.1.2 Representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (caddr exp))
(define (if-consequent exp) (cadr exp))
(define (if-alternative exp)
  (if (not (null? cddr exp))
  (cadddr exp)
  'false))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cons-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (cdr clause))
(define (cond-if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if ((null? clauses)
       'false
       (let ((first (car clauses))
             (rest (cdr clauses)))
         (if (cond-else-clause? first)
           (if (null? rest)
             (sequence->exp (cond-actions first))
             (error "ELSE clause isn't last: COND->IF" caluses))
           (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-caluses rest)))))))


; ex 4.02 

; ex 4.02 a
;
;   (define (eval exp env)
;     (cond ((self-evaluating? exp) exp)
;           ; .........................
;
;           ; application
;           ((application? exp)
;            (apply (eval (operator exp) env)
;                   (list-of-values (operands exp) env)))
;
;           ; ........................
;
;           ; assignments
;           ((assignment? exp) (eval-assignment exp env))
;           ((definition? exp) (eval-definition expt env))
;
;           (else
;             (error "Unknown expression type: EVAL" exp))))

; define未定義?


; ex 4.02 b
; 次のように変更する.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((equoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition expt env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond-if exp) env))
        (else
          (error "Unknown expression type: EVAL" exp))))

(define (application? exp) 
  (tagged-list? 'call exp))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

; ex 4.03 




; ex 4.04 

; 追加
;
;        ((and? exp) (eval-and exp env))
;        ((or? exp) (eval-or exp env))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((equoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition expt env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond-if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))


(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))


(define (eval-and exp env)
  (if (null? exp)
    #t
    (let ((evaled (eval (car exp))))
      (cond (((false? evaled) false)
             ((not (null? exp)) (eval-and (cdr exp) env))
             (else evaled)))))
;(define (eval-and exp env)
;  (define (and-iter exp result)
;    (if (null? exp)
;      result
;      (let ((evaled (eval (car exp))))
;        (if (true? evaled)
;          (and-iter (cdr exp) evaled)
;          #f))))
;  (and-iter exp #t))


(define (eval-or exp env)
  (if (null? exp)
    #f
    (let ((evaled (eval (car exp))))
      (cond ((true? evaled) evaled)
            (else (eval-or (cdr exp) env))
;(define (eval-and exp env)
;  (define (or-iter exp)
;    (if (null? exp)
;      #f
;      (let ((evaled (eval (car exp))))
;        (if (true? evaled)
;          evaled
;          (or-iter (cdr exp))))))
;  (or-iter exp)

; ex 4.05 
; ex 4.06 

(define (let->combination vars exps body)
  ((make-lambda vars body) exps))

;(define (lambda? exp) (tagged-list? exp 'lambda))
;(define (lambda-parameters exp) (cadr exp))
;(define (lambda-body exp) (cddr exp))
;
;(define (make-lambda parameters body)
;  (cons 'lambda (cons parameters body)))





; ex 4.07 
; ex 4.08 
; ex 4.09
; ex 4.10 


















