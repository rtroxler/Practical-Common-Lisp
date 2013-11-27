;;DEPRECATED
(defun test-+ ()
   (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
   (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
   (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

;;DEPRECATED
;;refactoring test-+ using report-result
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;;DEPRECATED
;;write a macro so don't have to always type (= (+ 1 2) 3) '(= (+ 1 2) 3)
(defmacro check (form)
  `(report-result ,form ',form))

;;DEPRECATED
;;rewrite test-+ again
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

;;DEPRECATED -- using combine-results instead of progn
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',defun))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

;; rewrite report-result to return result
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

;;need a gensym for combine-results, since you'll pass in a variable you don't want it to leak by having a dupe
;;name
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;need a way to make an AND that doesn't exit as soon as something fails, macro time, call it combine-results
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;;now change progn in check to combine-results
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;;adding tests

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;;but if a test fails, we want to know which test case it came from
;;need a dynamic var for the test-name
(defvar *test-name* nil)


;;add *test-name* to report result
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ...~a: ~a~%" result *test-name* form)
  result)

;;rewrite test cases to bind test-name to the name of the testcase
(defun test-* ()
  (let ((*test-name* 'test-*))
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15))))

(defun test-+ ()
  (let ((*test-name* 'test-+))
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4))))

;;but now there's a lot of duplication, what we need is a macro to write test functions with...
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;;now we can rewrite tests like this:
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

;;setting up a 'hierarchy' so that if there's multiple test suites (test-arithmetic being a suite) then you know
;;which suite a lower level test came from. Made changes to deftest up there, I'm cpying too much shit everywhere

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

