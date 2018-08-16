(define (list-of-values-left-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left (my-eval (first-operand exps) env)))
        (let ((right (list-of-values-left-right
                      (rest-operands exps)
                      env)))
          (cons left right)))))
