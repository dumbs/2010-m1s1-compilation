;; all-tests             : <module-struct>
;; <module-struct>       : (<alist-of-submodules> executed-bit variables tests)
;; <alist-of-submodules> : ((nom-module . <module-struct>) (nom-module2 . <module>) ...)
(defvar all-tests (list nil nil nil nil) "Liste de tous les tests")

(defun test-get-module (module &optional (from all-tests))
  (unless (listp module) (setq module (list module)))
  (if (endp module)
      from
      (let ((association (assoc (car module) (car from))))
        (unless association
          (let ((new-assoc (list (car module) nil nil nil nil)))
            (push new-assoc (car from))
            (setq association new-assoc)))
        (test-get-module (cdr module) (cdr association)))))

(defun test-get-submodules (module) (first  (test-get-module module)))
(defun test-get-executed   (module) (second (test-get-module module)))
(defun test-get-variables  (module) (third  (test-get-module module)))
(defun test-get-tests      (module) (fourth (test-get-module module)))

(defun test-collect-down-tree (fn module &optional (from all-tests))
  (unless (listp module) (setq module (list module)))
  (if (endp module)
      (cons (funcall fn from) nil)
      (cons (funcall fn from)
            (test-collect-down-tree fn (cdr module)
                                    (cdr (assoc (car module) (car from)))))))

(defun test-get-variables-and-above (module &optional (from all-tests))
  (remove-duplicates (apply #'append (mapcar #'reverse (test-collect-down-tree #'third module from))) :key #'car))

(defun test-set-executed (from &optional (value t))
  (setf (second from) value))

(defun test-clear-all-executed (&optional (from all-tests))
  (setf (second from) nil)
  (mapcar #'test-clear-all-executed
		  (mapcar #'cdr (first from))))

(defun test-add-variable (module variable)
  (push variable (third (test-get-module module)))
  t)

(defun test-add-test (module test)
  (push test (fourth (test-get-module module)))
  t)

(defun test-remove-module (module)
  (if (null module)
      (setf all-tests (list nil nil nil nil))
      (let ((from (test-get-module (butlast module))))
        (setf (first from)
              (delete (car (last module))
                      (first from)
                      :key #'car)))))

(defun booleq (a b)
  (if a b (not b)))

(defmacro deftest (module test expected &optional (compare #'equal))
  (let ((vars (test-get-variables-and-above module)))
    `(test-add-test
      ',module
      (lambda ()
        (let* ((state-1 (make-random-state))
               (state-2 (make-random-state state-1))
               (res (labels ((random-test (n) (random n state-1)))
                      (let* ,vars ,@(mapcar #'car vars) ,test)))
               (exp (labels ((random-test (n) (random n state-2)))
                      (let* ,vars ,@(mapcar #'car vars) ,expected))))
          (if (funcall ,compare res exp)
              (progn
                (format t "~&    [SUCCESS] ~w~&" ',test)
                t)
              (progn
                (format t "~&    [FAILURE] Test       : ~w~&" ',test)
                (format t "~&              got        : ~w~&" res)
                (format t "~&              expected   : ~w~&" exp)
                (format t "~&              comparison : ~w~&" ,compare)
                nil)))))))

(defmacro generates-error-p (code)
  `(car (handler-case (cons nil ,code)
          (error (e) (cons t e)))))

(defmacro deftest-error (module test &optional (expected t))
  `(deftest ,module (generates-error-p ,test)
     ,expected))

(defmacro deftestvar (module name value)
  `(test-add-variable ',module
                      (list ',name ',value)))

(defvar run-tests-counter 0)

(declaim (ftype function real-run-tests)) ;; récursion mutuelle real-run-tests / run-tests-submodules
(defun run-tests-submodules (module-name submodules)
  (if (endp submodules)
      t
      (and (real-run-tests (append module-name (list (caar submodules))) (cdar submodules))
           (run-tests-submodules module-name (cdr submodules)))))

(defun real-run-tests (module-name from)
  (if (second from)
      (progn
        (format t "~&~%-~{ ~w~}~&    [Déjà vu]~&" (or module-name '(all-tests)))
        t)
      (progn
        (format t "~&~%>~{ ~w~}~&" (or module-name '(all-tests)))
        (setf (second from) t) ;; marquer comme exécuté.
        (let ((nb-fail (count-if-not #'funcall (reverse (fourth from)))))
          (if (= nb-fail 0)
              (progn
                (incf run-tests-counter (length (fourth from)))
                (run-tests-submodules module-name (reverse (first from))))
              (format t "Module ~w failed ~w tests. Stopping.~&" module-name nb-fail))))))

(defmacro run-tests (&rest modules)
  (when (null modules) (setq modules '(nil)))
  (setq modules (substitute nil t modules))
  (setq run-tests-counter 0)
  `(progn
	 (test-clear-all-executed)
	 (if (every #'real-run-tests
                ',(mapcar (lambda (x) (if (listp x) x (list x))) modules)
                ',(mapcar #'test-get-module modules))
         (progn (format t "~a tests passed sucessfully." run-tests-counter)
                t)
         nil)))

(defun count-nb-tests (from)
  (apply #'+
         (length (fourth from))
         (mapcar (lambda (x) (count-nb-tests (cdr x))) (car from))))

(defun real-show-tests (module-name from)
  (format t "~&~4@<~d~> ~4@<~d~> >~{ ~w~}~&"
                        (length (fourth from))
                        (count-nb-tests from)
                        (or module-name '(all-tests)))
  (mapcar (lambda (x) (real-show-tests (append module-name (list (car x))) (cdr x)))
          (first from))
  nil)

(defmacro show-tests (&optional module)
  (unless (listp module) (setq module (list module)))
  `(let ((mod (test-get-module ',module)))
     (real-show-tests ',module mod)
     (format t "~a tests." (count-nb-tests mod))
     t))

(defmacro erase-tests (&optional module)
  (unless (listp module) (setq module (list module)))
  `(test-remove-module ',module))

(erase-tests test-unitaire)
(deftest (test-unitaire copy-all)
  (let* ((foo #(a b (1 #(2 4 6) 3) c))
           (copy-of-foo (copy-all foo)))
      copy-of-foo
      (setf (aref (cadr (aref copy-of-foo 2)) 1) (cons 'MODIFIED (random-test 42)))
      (equalp foo #(a b (1 #(2 4 6) 3) c)))
  t #'booleq)

;;; Exemples d'utilisation.

;; (erase-tests (a sub-1))
;; (erase-tests b)
;; (erase-tests)

;; (deftestvar a foo 'bar)
;; (deftest a nil nil)
;; (deftest a (eq 42 42) t)
;; (deftest (a) (eq 'bar 'bar) t)
;; (deftest (a) (eq 'baz 'quux) nil)
;; (deftest (a sub-1) (eq 'x 'y) nil)
;; (deftest (a sub-1) (eq 'x 'x) t)
;; (deftest (a sub-2) (eq 'x 'x) t)
;; (deftest (b sub-1) (eq 'y 'y) t)
;; (deftest c (eq 'foo 'foo) t)
;; (deftest-error c (if (eq 42 42) (error "foo") (error "bar")))

;; Pour lancer les tests :
;; (run-tests (a sub-1) b t)
;; (run-tests ())
;; (run-tests t)
;; (run-tests)

(provide 'test-unitaire)
