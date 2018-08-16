;; -*- geiser-scheme-implementation: 'chicken -*-
;; On windows Geiser doesn't seem to work, so, from babun, run emacs and then
;; (setq scheme-program-name "/usr/local/bin/csi")
;; (define-key scheme-mode-map (kbd "C-x C-e") 'scheme-send-last-sexp)
;; Disable geiser-mode

(load "common.scm")

;; Syntax definitions.
(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (my-eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

(define (eval-or exp env)
  (eval-or-body (or-body exp) env))
(define (eval-or-body exp env)
  (if (or-null-clause? exp)
      #f
      (let ((result (my-eval (or-first-clause exp) env)))
        (if (true? result)
            result
            (eval-or-body (or-rest-clauses exp) env)))))

(define (eval-and exp env)
  (eval-and-body (and-body exp) env))
(define (eval-and-body exp env)
  (cond ((and-null-clause? exp) #t)
        ((and-last-clause? exp) (my-eval (and-first-clause exp) env))
        (else (let ((result (my-eval (and-first-clause exp) env)))
                (if (false? result)
                    #f
                    (eval-and-body (and-rest-clauses exp) env))))))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))

(define (let->combination exp)
  (if (symbol? (cadr exp))
      ;; Named let
      (make-begin
       (list (list 'define
                   (cons (cadr exp)
                         (map car (caddr exp)))
                   (cadddr exp))
             (cons (cadr exp)
                   (map cadr (caddr exp)))))
      ;; Normal let
      (let* ((bindings (let-bindings exp))
             (params (map car bindings))
             (args (map cadr bindings))
             (body (let-body exp)))
        (cons (list 'lambda
                    params
                    body)
              args))))
(define (bindings->let bindings body)
  (if (null? bindings)
      body
      (cons 'let
            (cons (list (car bindings))
                  (list (bindings->let (cdr bindings) body))))))
(define (let*->nested-lets exp)
  (bindings->let (let-bindings exp)
                 (let-body exp)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps)
                            env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (my-eval (first-exp exps) env))        
        (else
         (my-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (variadic-map op . args)
  (define (simple-map op lst)
  (if (null? lst)
      '()
      (cons (op (car lst))
            (simple-map op (cdr lst)))))
  (define (helper op args)
    (if (null? (car args))
        '()
        (cons (my-apply op (simple-map car args)) 
              (helper op (simple-map cdr args)))))
  (helper op args))

(define (my-eval exp env)
  (cond
   ((self-evaluation? exp) exp)
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
   ((unless? exp)
    (my-eval (unless->if exp)
             env))
   ((let? exp)
    (my-eval (let->combination exp)
             env))
   ((let*? exp)
    (my-eval (let*->nested-lets exp)
             env))
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
    (my-eval (cond->if exp) env))
   ((application? exp)
    (my-apply (my-eval (operator exp) env)
              (list-of-values
               (operands exp)
               env)))
   (else (error "Unknown expression type: MY-EVAL" exp))))

(define (my-apply procedure arguments)
  (cond
   ((primitive-procedure? procedure)
    (apply-primitive-procedure
     procedure
     arguments))
   ((compound-procedure? procedure)
    (eval-sequence
     (procedure-body procedure)
     (extend-environment
      (procedure-parameters procedure)
      arguments
      (procedure-environment procedure))))
   (else (error ("Unknown procedure type: MY-APPLY" procedure)))))
