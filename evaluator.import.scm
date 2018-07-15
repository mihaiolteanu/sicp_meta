;; -*- geiser-scheme-implementation: 'chicken -*-
;; https://wizardbook.wordpress.com/2010/12/26/exercise-4-4/

(module evaluator (my-eval)
  (import chicken scheme)

(define (text-of-quotation exp)
  (cdr exp))

(define (true? exp)
  (not (eq? exp #f)))

(define (false? exp)
  (eq? exp #f))

;; Syntax definitions.
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (my-eval (assignment-value exp) env)
   env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

(define (make-lambda params body)
  `(lambda ,params ,body))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))
(define (make-if predicate
                 consequent
                 alternative)
  (list 'if predicate consequent alternative))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-body exp) (cdr exp))
(define (or-first-clause exp) (car exp))
(define (or-rest-clauses exp) (cdr exp))
(define (or-null-clause? exp) (null? exp))
(define (eval-or exp env)
  (eval-or-body (or-body exp) env))
(define (eval-or-body exp env)
  (if (or-null-clause? exp)
      #f
      (let ((result (my-eval (or-first-clause exp) env)))
        (if (true? result)
            result
            (eval-or-body (or-rest-clauses exp) env)))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-body exp) (cdr exp))
(define (and-first-clause exp) (car exp))
(define (and-rest-clauses exp) (cdr exp))
(define (and-null-clause? exp) (null? exp))
(define (eval-and exp env)
  (eval-and-body (and-body exp) env))
(define (eval-and-body exp env)
  (if (and-null-clause? exp)
      #t
      (let ((result (my-eval (and-first-clause exp))))
        (if (false? result)
            #f
            (eval-and-body (and-rest-clauses exp) env)))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))


(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (first-exp exps) (car exps))
(define (rest-exps exps) (cdr exps))
(define (last-exp? exps)
  (null? (rest-exps exps)))

;; Transform a sequence into a single expression.
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (self-evaluation? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps)
                            env))))

;; Exercise 4.1
(define (list-of-values-left-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left (my-eval (first-operand exps) env)))
        (let ((right (list-of-values-left-right
                      (rest-operands exps)
                      env)))
          (cons left right)))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (my-eval (first-exp exps) env))        
        (else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (primitive-procedure? procedure)
  (true? (assoc procedure primitive-procedures)))
(define primitive-procedures
  `((+ . ,+)
    (- . ,-)))
(define (apply-primitive-procedure procedure arguments)
  (cond ((assoc procedure primitive-procedures) =>
         (lambda (entry)
           (apply (cdr entry) arguments)))
        (else (error "Unknown primitive procedure: APPLY-PRIMITIVE-PROCEDURE" procedure))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))


(define (my-eval exp env)
  (cond ((self-evaluation? exp) exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((or? exp)
         (eval-or exp env))
        ((and? exp)
         (eval-and exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp)
          env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((application? exp)
         (my-apply (my-eval (operator exp))
                   (list-of-values
                    (operands exp)
                    env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment environment))))
        (else (error ("Unknown procedure type: APPLY" procedure)))))


)
