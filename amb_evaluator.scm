(define fail 
  (lambda () 
    (error "Amb tree exhausted")))

(define-syntax amb 
   (syntax-rules () 
     ((amb) (fail))                      ; two shortcuts. 
     ((amb expression) expression) 
     ((amb expression ...) 
      (let ((fail-save fail)) 
        ((call-with-current-continuation ; capture a continuation to 
           (lambda (k-success)           ;   which we return possibles. 
             (call-with-current-continuation 
               (lambda (k-failure)       ; k-failure will try the next 
                 (set! fail              ;   possible expression. 
                       (lambda () (k-failure #f))) 
                 (k-success              ; note that the expression is 
                  (lambda ()             ;   evaluated in tail position 
                    expression))))       ;   with respect to amb. 
             ... 
             (set! fail fail-save)      ; finally, if this is reached, 
             fail-save)))))))           ;   we restore the saved fail.

(define (require condition) 
  (if (not condition) 
      (fail)))


(define *unparsed* '(the cat eats))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb 
     noun-phrase
     (maybe-extend 
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word words)
  (if (member (car *unparsed*) words)
      (let ((word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car words) word))
      (error "invalid sentence")))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) 
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the cat sleeps))
(parse '(for the cat))

(parse '(the student with the cat 
         sleeps in the class))

(parse '(the professor lectures to 
             the student with the cat))

(fail)

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between i high))
         (k (an-integer-between j high)))
    (require (= (+ (* i i) (* j j)) 
                (* k k)))
    (list i j k)))

(define (all-pythagorean-triples)
  (let* ((j (an-integer-starting-from 1))
         (i (an-integer-between 1 j))
         (k (an-integer-between j (+ (* i i) (* j j)))))
    (require (= (+ (* i i) (* j j)) 
                (* k k)))
    (list i j k)))

(a-pythagorean-triple-between 5 15)

(define (find x lst)
  (if (null? lst)
      #f
      (if (eq? x (car lst))
          #t
          (find x (cdr lst)))))

(define (distinct? lst)
  (define bkup '())
  (define (helper l)
    (cond ((null? l) #t)
          ((find (car l) bkup) #f)
          (else
           (set! bkup (cons (car l) bkup))
           (helper (cdr l)))))
  (helper lst))

(define (rest x lst)
  (cond ((null? lst) '())
        ((eq? x (car lst)) (cdr lst))
        (else (rest x (cdr lst)))))

(define building '(1 2 3 4 5))
(let* ((baker (an-element-of '(1 2 3 4)))
       (cooper (an-element-of '(2 3 4 5)))
       (fletcher (an-element-of '(2 3 4)))
       (miller (an-element-of (rest cooper building)))
       (smith (an-element-of building)))
  (let ((inhabitants (list baker cooper fletcher miller smith)))
    (require (distinct? inhabitants))
    (require (> (abs (- smith fletcher)) 1))
    (require (> (abs (- cooper fletcher)) 1))
    inhabitants))

;; Ex. 4.42
(let ((betty (amb 1 2 3 4 5))
      (ethel (amb 1 2 3 4 5))
      (joan  (amb 1 2 3 4 5))
      (kitty (amb 1 2 3 4 5))
      (mary  (amb 1 2 3 4 5)))
  (let ((standings (list betty ethel joan kitty mary)))    
    (require (or (and (= kitty 2)
                      (not (= betty 3)))
                 (and (not (= kitty 2))
                      (= betty 3))))
    (require (or (and (= ethel 1)
                      (not (= joan 2)))
                 (and (not (= ethel 1))
                      (= joan 2))))
    (require (or (and (= joan 3)
                      (not (= ethel 5)))
                 (and (not (= joan 3))
                      (= ethel 5))))
    (require (or (and (= kitty 2)
                      (not (= mary 4)))
                 (and (not (= kitty 2))
                      (= mary 4))))
    (require (or (and (= mary 4)
                      (not (= betty 1)))
                 (and (not (= mary 4))
                      (= betty 1))))
    (require (distinct? standings))
    standings))

(define rows '(1 2 3 4 5 6 7 8))
(define one-of an-element-of)
(let ((q1 (one-of rows))
      (q2 (one-of rows))
      (q3 (one-of rows))
      (q4 (one-of rows))
      (q5 (one-of rows))
      (q6 (one-of rows))
      (q7 (one-of rows))
      (q8 (one-of rows)))
  (let ((solution (list q1 q2 q3 q4 q5 q6 q7 q8)))
    (require (distinct? solution))
    solution))
(one-of rows)

(let ((q1 (one-of rows)))
  (let ((q2 (one-of rows)))
    (require (not (= q1 q2)))
    (require (not (= (abs (- q1 q2))
                     1)))
    (let ((q3 (one-of rows)))
      (require (and (not (= q3 q2))
                    (not (= q3 q1))))
      (require (and (not (= (abs (- q3 q2))
                            1))
                    (not (= (abs (- q3 q1))
                            2))))
      )))
