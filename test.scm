(use test)
(import evaluator)

(test-group
 "evaluator_test"
 (test 3 (my-eval '(+ 1 2)))

 )
