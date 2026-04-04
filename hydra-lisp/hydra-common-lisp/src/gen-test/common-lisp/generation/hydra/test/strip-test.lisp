;; Note: this is an automatically generated file. Do not edit.
;; strip

;; deannotateTerm

(defun test-strip-negdeannotateterm-negunannotated-literal-unchanged ()

  (assert (equal 42:int32 42:int32)))

(defun test-strip-negdeannotateterm-negunannotated-variable-unchanged ()

  (assert (equal x x)))

(defun test-strip-negdeannotateterm-negunannotated-lambda-unchanged ()

  (assert (equal λx.x λx.x)))

(defun test-strip-negdeannotateterm-negsingle-annotation-stripped ()

  (assert (equal 42:int32 42:int32)))

(defun test-strip-negdeannotateterm-negnested-annotations-stripped ()

  (assert (equal 42:int32 42:int32)))

(defun test-strip-negdeannotateterm-negannotated-lambda-stripped ()

  (assert (equal λx.x λx.x)))

(defun test-strip-negdeannotateterm-negannotated-application-stripped ()

  (assert (equal (f @ x) (f @ x))))

;; deannotateType

(defun test-strip-negdeannotatetype-negunannotated-primitive-type-unchanged ()

  (assert (equal int32 int32)))

(defun test-strip-negdeannotatetype-negunannotated-string-type-unchanged ()

  (assert (equal string string)))

(defun test-strip-negdeannotatetype-negunannotated-function-type-unchanged ()

  (assert (equal (int32 → string) (int32 → string))))

(defun test-strip-negdeannotatetype-negsingle-annotation-stripped ()

  (assert (equal int32 int32)))

(defun test-strip-negdeannotatetype-negnested-annotations-stripped ()

  (assert (equal string string)))

(defun test-strip-negdeannotatetype-negannotated-list-type-stripped ()

  (assert (equal list<int32> list<int32>)))

(defun test-strip-negdeannotatetype-negannotated-function-type-stripped ()

  (assert (equal (int32 → string) (int32 → string))))
