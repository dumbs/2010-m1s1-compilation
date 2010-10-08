(let ((tests nil))
  (defmacro deftest (module expected test)
    (setf tests (cons (list module expected test) tests))
     nil)
  (defmacro run-test (module)
    (mapcar (lambda (test)
              (let ((res (eval (third test))))
                (if (equal (second test) res)
                    (print "[SUCCESS] ~a" (third test))
                  (print  "[FAILURE] ~a\n           got ~a\n           expected ~a" (third test) res (second test)))))
              tests) nil)
  (defun show-test ()
    tests))

;; Test de debugage du test unitaire
(deftest environnement nil nil)
(show-test)
(run-test environnement)

