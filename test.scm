(use test)
(import evaluator)

(test-group
 "evaluator_test"
 (test 3 (interpreter '(+ 1 2)))
 (test 5 (interpreter '(- 7 2)))

 )
