(define (my-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst)
               (my-filter pred (cdr lst))))
        (else (my-filter pred (cdr lst)))))

(define (test exps)
  (my-filter
   (lambda (e)
     (not (equal? e #t)))
   (map (lambda (e)
           ;Cleanup before the next test
          (set! the-global-environment (setup-environment))
          (let ((res (interpreter (caddr e))))
            (if (equal? (cadr e) res)
                #t
                (list "evaled" (caddr e) "expecting" (cadr e) "but got" res))))
        exps)))

(define (display-test-results tests)
  (display "Failed-tests: ")
  (map (lambda (t)
         (newline)
         (display t))
       tests)
  (newline))

(define tests
  '(test '(
      ("simple addition"
       21 (+ 1 20))
      ("multiple evaluations"
       10 (+ 1 (+ 2 (+ 4 5) (- 2 4))))
      ("car"
       1 (car '(1 2 3)))
      ("cdr"
       (2 3) (cdr '(1 2 3)))
      ("cons"
       (1 2 3) (cons 1 (cons 2 (cons 3 '()))))
      ("null?"
       #t (null? '()))
      ("null? false"
       #f (null? '(1 2 3)))

      ;; Cond clauses
      ("cond - basic return before else"
       2 (begin
            (define x 7)
            (cond ((> x 10) 1)
                  ((> x 5) 2)
                  (else 3))))
      
      ("cond - basic, return from else"
       3 (begin
            (define x 2)
            (cond ((> x 10) 1)
                  ((> x 5) 2)
                  (else 3))))

      ("cond - eval all the actions"
       15 (begin
           (define x 5)
           (cond ((> x 1)
                  (define y 10)
                  (+ x y))
                 (else 1))))
      
      ("cond => expression"
       8 (cond (5 => (lambda (e) (+ e 3)))
                (else 2)))

      ;; Let
      ("let - basic"
       10 (let ((x 4)
                (y 6))
            (+ x y)))
      ("let - with closure"
       15 (begin
             (define x 10)
             (let ((a 2)
                   (b 3))
               (+ a b x))))
      
      ;; And tests
      ("and - eval all expressions"
       (5 6) (begin
               (define x 10)
               (define y 20)
               (and (set! x 5) (set! y 6))
               (list x y)))
      ("and - eval until first false expression"
       (5 20) (begin
                (define x 10)
                (define y 20)
                (and (set! x 5) #f (set! y 6))
                (list x y)))
      ("and - return last expression if all are true"
       3 (and 1 2 3))
      ("and empty"
       #t (and))
      ("and false"
       #f (and 3 #f 4))

      ;; Or tests
      ("or empty"
       #f (or))
      ("or false"
       #f (or #f #f #f))
      ("or - eval "
       (5 20) (begin
                (define x 10)
                (define y 20)
                (or #f (set! x 5) (set! y 6))
                (list x y)))

      ;; Unless
      ("unless consequent"
       2 (begin
           (define a 10)
           (define b 5)
           (unless (= b 0)
             (/ a b)
             0)))

      ("unless alternative"
       0 (begin
           (define a 10)
           (define b 0)
           (unless (= b 0)
             (/ a b)
             0)))

      ("define a variable"
       10 (begin
            (define x 10)
            x))
      ("redefinition"
       10 (begin
            (define x 5)
            (define x 10)
            x))
      ("set!"
       10 (begin
            (define x 5)
            (set! x 10)
            x))
      ("define a function"
       10 (begin
            (define (myf x y) (+ x y))
            (myf 4 6)))
      ("define a function with a lambda"
       10 (begin
            (define myf (lambda (x y) (+ x y)))
            (myf 4 6)))
      ;; ("mapping"
      ;;  (1 3) (map car '((1 2) (3 4))))
      ;; ("mapping"
      ;;  (2 3 4) (map (lambda (e)
      ;;                 (+ e 1))
      ;;               '(1 2 3)))
      )))

(define (run-evaluator-tests)
  (load "evaluator.scm")
  (display-test-results (eval tests)))

(define (run-lazy-evaluator-tests)
  (load "analyzing_evaluator.scm")
  (display-test-results (eval tests)))
