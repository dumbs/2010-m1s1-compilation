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
  `(test-add-test
    ',module
    (lambda ()
      (let* ((vars (test-get-variables ',module))
             (_test ',test)
             (_expected ',expected)
             (_compare ,compare)
             ;; Les "eval" ci-dessous exécutent :
             ;; (let ((var1 val1) (var2 val2) ...) ;; On définit les
             ;;                                    ;; variables de deftestvar.
             ;;   var1 var2 ... ;; On "utilise" les variables pour
             ;;                 ;; éviter le unused variable warning
             ;;   corps-du-test) ;; On évalue le corps du test dans 
             ;;                  ;; un environement où les deftestvar
             ;;                  ;; sont accessibles.
             (res (eval `(let ,vars ,@(mapcar #'car vars) ,_test)))
             (exp (eval `(let ,vars ,@(mapcar #'car vars) ,_expected))))
        (if (funcall _compare res exp)
            (progn
              (format t "~&    [SUCCESS] ~w~&" ',test)
              t)
            (progn
              (format t "~&    [FAILURE] Test       : ~w~&" ',test)
              (format t "~&              got        : ~w~&" res)
              (format t "~&              expected   : ~w~&" exp)
              (format t "~&              comparison : ~w~&" _compare)
              nil))))))

(defmacro deftestvar (module name value)
  `(test-add-variable ',module
                      (list ',name (list 'copy-tree ',value))))

(defun run-tests-submodules (module-name submodules)
  (if (endp submodules)
      t
      (and (real-run-tests (append module-name (list (caar submodules))) (cdar submodules))
           (run-tests-submodules module-name (cdr submodules)))))

(defvar run-tests-counter 0)
(defun real-run-tests (module-name from)
  (if (second from)
      (progn
        (format t "~&~%-~{ ~a~}~&    [Déjà vu]~&" (or module-name '("all-tests")))
        t)
      (progn
        (format t "~&~%>~{ ~a~}~&" (or module-name '("all-tests")))
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

(defmacro erase-tests (&optional module)
  (unless (listp module) (setq module (list module)))
  `(test-remove-module ',module))

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

;; (run-tests (a sub-1) b t)
;; (run-tests ())
;; (run-tests t)
;; (run-tests)
