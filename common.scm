(define (text-of-quotation exp)
  (cdr exp))

(define (true? exp)
  (not (eq? exp #f)))
(define (false? exp)
  (eq? exp #f))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

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

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
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

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-body exp) (cdr exp))
(define (and-first-clause exp) (car exp))
(define (and-rest-clauses exp) (cdr exp))
(define (and-null-clause? exp) (null? exp))
(define (and-last-clause? exp) (null? (cdr exp)))

(define (unless? exp)
  (tagged-list? exp 'unless))
(define (unless-predicate exp)   (cadr exp))
(define (unless-consequent exp)  (caddr exp))
(define (unless-alternative exp) (cadddr exp))

(define (let? exp)
  (tagged-list? exp 'let))
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (caddr exp))

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
(define (cond-lambda-syntax? clause)
  (eq? '=> (car (cond-actions clause))))
(define (cond-apply-lambda-syntax predicate action)
  (list action predicate))
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
                     (if (cond-lambda-syntax? first)
                         (cond-apply-lambda-syntax
                          (cond-predicate first)
                          (cadr (cond-actions first)))
                         (sequence->exp 
                          (cond-actions first)))
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
        ((boolean? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))

(define (primitive-procedure? procedure)
  (tagged-list? procedure 'primitive))
(define primitive-procedures
  `((+ . (primitive ,+))
    (- . (primitive ,-))
    (* . (primitive ,*))
    (/ . (primitive ,/))
    (< . (primitive ,<))
    (> . (primitive ,>))
    (= . (primitive ,=))
    (cons . (primitive ,cons))
    (car . (primitive ,car))
    (cdr . (primitive ,cdr))
    (list . (primitive ,list))
    (null? . (primitive ,null?))
    ;; (map . (primitive ,variadic-map))
    ))
(define (primitive-implementation procedure)
  (cadr procedure))
(define (apply-primitive-procedure procedure arguments)
  (apply (primitive-implementation procedure) arguments))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (let ((frame '()))
    (map (lambda (x y)
           (set! frame (cons (cons x y) frame)))
         variables values)
    frame))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define base-environment
  (extend-environment
   (map (lambda (e)
          (car e))
        primitive-procedures)
   (map (lambda (e)
          (cdr e))
        primitive-procedures)
   the-empty-environment))

(define (add-binding-to-frame! var val frame)
  (cond ((null? (cdr frame))
         (set-cdr! frame (list (cons var val))))
        (else (add-binding-to-frame! var val (cdr frame)))))

(define (lookup-in-frame var frame)
  (cond ((or (null? frame)
             ;; Hackish. first-frame returns the first variable binding
             ;; when there are no more enclosing envs
             (not (pair? (car frame))))
         #f)
        ((eq? var (caar frame)) (cdar frame))
        (else (lookup-in-frame var (cdr frame)))))

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((res (lookup-in-frame var (first-frame env))))
        (if (not (false? res))
            res
            (lookup-variable-value var (enclosing-environment env))))))

(define (set-variable! var val frame)
  (if (null? frame)
      (error "Unbound variable: SET!" var)
      (if (eq? var (caar frame))
          (set-cdr! (car frame) val)
          (set-variable! var val (cdr frame)))))

(define (set-variable-value! var val env)
  (set-variable! var val (first-frame env)))

(define (define-var! var val frame)
  (if (null? (cdr frame))               ;No definition found
      (set-cdr! frame (list (cons var val)))
      (if (eq? var (caar frame))        ;Redefine otherwise
          (set-cdr! (car frame) val)
          (define-var! var val (cdr frame)))))

(define (define-variable! var val env)
  (define-var! var val (first-frame env)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (map (lambda (e)
                 (car e))
               primitive-procedures)
          (map (lambda (e)
                 (cdr e))
               primitive-procedures)
          the-empty-environment)))
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt "> ")
(define output-prompt "")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (display string))

(define (announce-output string)
  (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (begin
        (display object)
        (newline))))

(define (interpreter exp)
  (my-eval exp the-global-environment))
