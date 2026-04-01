;; Note: this is an automatically generated file. Do not edit.
;; serialization

;; associativity

(defun test-serialization-negassociativity-negright-negassociative-operator ()

  (assert (equal (a -> b) -> c -> d (a -> b) -> c -> d)))

;; case statements

(defun test-serialization-negcase-statements-negsimple-case-statement ()

  (assert (equal case x > 42 of
  False -> Big
  True -> Small case x > 42 of
  False -> Big
  True -> Small)))

(defun test-serialization-negcase-statements-negnested-case-statement ()

  (assert (equal case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small)))

;; lambdas

(defun test-serialization-neglambdas-negsimple-lambda ()

  (assert (equal \x y -> x + y \x y -> x + y)))

;; lists

(defun test-serialization-neglists-negempty-list ()

  (assert (equal [] [])))

(defun test-serialization-neglists-negsimple-non-negempty-list ()

  (assert (equal [1, 2, 3] [1, 2, 3])))

(defun test-serialization-neglists-negnested-list ()

  (assert (equal [[1, 3], 2] [[1, 3], 2])))

(defun test-serialization-neglists-neglist-with-parenthesized-expression-inside ()

  (assert (equal [[1, (2 + 3) * (1 + 10)], 2] [[1, (2 + 3) * (1 + 10)], 2])))

;; precedence

(defun test-serialization-negprecedence-negoperators-with-different-precedence--neg-no-parens-needed ()

  (assert (equal 2 * 3 + 1 * 10 2 * 3 + 1 * 10)))

(defun test-serialization-negprecedence-negoperators-with-different-precedence--neg-parens-needed ()

  (assert (equal (2 + 3) * (1 + 10) (2 + 3) * (1 + 10))))

(defun test-serialization-negprecedence-negassociative-operator-left-nesting ()

  (assert (equal x * y * z x * y * z)))

(defun test-serialization-negprecedence-negassociative-operator-right-nesting ()

  (assert (equal x * y * z x * y * z)))
