;; Note: this is an automatically generated file. Do not edit.
;; JSON serialization

;; primitives

(defun test-writer-negprimitives-negnull ()

  (assert (equal null null)))

(defun test-writer-negprimitives-negtrue ()

  (assert (equal true true)))

(defun test-writer-negprimitives-negfalse ()

  (assert (equal false false)))

(defun test-writer-negprimitives-negzero ()

  (assert (equal 0 0)))

(defun test-writer-negprimitives-negpositive-integer ()

  (assert (equal 42 42)))

(defun test-writer-negprimitives-negnegative-integer ()

  (assert (equal -17 -17)))

(defun test-writer-negprimitives-neglarge-integer ()

  (assert (equal 1000000 1000000)))

(defun test-writer-negprimitives-negdecimal ()

  (assert (equal 3.14 3.14)))

(defun test-writer-negprimitives-negnegative-decimal ()

  (assert (equal -2.5 -2.5)))

(defun test-writer-negprimitives-negsmall-decimal ()

  (assert (equal 1.0e-3 1.0e-3)))

;; strings

(defun test-writer-negstrings-negempty-string ()

  (assert (equal "" "")))

(defun test-writer-negstrings-negsimple-string ()

  (assert (equal "hello" "hello")))

(defun test-writer-negstrings-negstring-with-spaces ()

  (assert (equal "hello world" "hello world")))

(defun test-writer-negstrings-negstring-with-double-quote ()

  (assert (equal "say \"hi\"" "say \"hi\"")))

(defun test-writer-negstrings-negstring-with-backslash ()

  (assert (equal "path\\to\\file" "path\\to\\file")))

(defun test-writer-negstrings-negstring-with-newline ()

  (assert (equal "line1\nline2" "line1\nline2")))

(defun test-writer-negstrings-negstring-with-carriage-return ()

  (assert (equal "line1\rline2" "line1\rline2")))

(defun test-writer-negstrings-negstring-with-tab ()

  (assert (equal "col1\tcol2" "col1\tcol2")))

(defun test-writer-negstrings-negstring-with-mixed-escapes ()

  (assert (equal "a\"b\\c\nd" "a\"b\\c\nd")))

;; arrays

(defun test-writer-negarrays-negempty-array ()

  (assert (equal [] [])))

(defun test-writer-negarrays-negsingle-element ()

  (assert (equal [1] [1])))

(defun test-writer-negarrays-negmultiple-numbers ()

  (assert (equal [1, 2, 3] [1, 2, 3])))

(defun test-writer-negarrays-negmultiple-strings ()

  (assert (equal ["a", "b"] ["a", "b"])))

(defun test-writer-negarrays-negmixed-types ()

  (assert (equal [1, "two", true, null] [1, "two", true, null])))

;; objects

(defun test-writer-negobjects-negempty-object ()

  (assert (equal {} {})))

(defun test-writer-negobjects-negsingle-key-negvalue ()

  (assert (equal {"name": "Alice"} {"name": "Alice"})))

(defun test-writer-negobjects-negmultiple-keys ()

  (assert (equal {"a": 1, "b": 2} {"a": 1, "b": 2})))

(defun test-writer-negobjects-negmixed-value-types ()

  (assert (equal {"active": true, "count": 42, "name": "test"} {"active": true, "count": 42, "name": "test"})))

;; nested structures

(defun test-writer-negnested-structures-negnested-arrays ()

  (assert (equal [[1, 2], [3, 4]] [[1, 2], [3, 4]])))

(defun test-writer-negnested-structures-negobject-with-array ()

  (assert (equal {"items": [1, 2]} {"items": [1, 2]})))

(defun test-writer-negnested-structures-negarray-of-objects ()

  (assert (equal [{"id": 1}, {"id": 2}] [{"id": 1}, {"id": 2}])))

(defun test-writer-negnested-structures-negnested-object ()

  (assert (equal {"user": {"name": "Bob"}} {"user": {"name": "Bob"}})))
