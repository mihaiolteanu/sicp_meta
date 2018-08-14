(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

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
                   (caddr exp))))
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (analyze-sequence (definition-value exp)))
  'ok)

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda params body)
  (list 'lambda
        params
        (analyze-sequence body)))

(define (analyze-self-evaluating exp)
  (lambda (env)
    exp))

(define (analyze-quoted exp)
  (let ((quote-body (cdr exp)))
    (lambda (env)
      quote-body)))

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)))

(define (analyze-if exp)
  (let ((predicate   (analyze (if-predicate exp)))
        (consequent  (analyze (if-consequent exp)))
        (alternative (analyze (if-alternative exp))))
    (lambda (env)
      (if (myeval predicate env)
          (myeval consequent env)
          (myeval alternative env)))))

(define (application-operator exp) (car exp))
(define (application-body))
(define (analyze-application exp)
  (let ((op (application-operator exp)))
    (lambda (env)
      (let ((proc (lookup-variable-value exp env))
            (body ()))
        
        )))
  )

(define (my-foldr f init lst)
  (if (null? lst)
      init
      (f (car lst)
         (my-foldr f init (cdr lst)))))

(my-foldr cons '(4 5) '(1 2 3))

(define (analyze-sequence exps)
  (map analyze exps))

(define (myeval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) 
         (analyze-quoted exp))
        ((variable? exp) 
         (analyze-variable exp))
        ((assignment? exp) 
         (analyze-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((if? exp) 
         (analyze-if exp))
        ((lambda? exp) 
         (analyze-lambda exp))
        ((begin? exp) 
         (analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (analyze (cond->if exp)))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression 
                 type: ANALYZE" 
                exp))))

(let ((myf (lambda (n)
             (if (= n 1)
                 1
                 (* n (myf (- n 1)))))))
  (myf 5))



((lambda (lst)
  lst) '(1 2 3))
