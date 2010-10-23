;; TODO : exploser tout ça en plein de petites fonctions, c'est trop gros...
;; Mutation cons.
(defvar all-tests nil "Liste de tous les tests")

;(defmacro eval-in-env (expr env)
;  `(eval-in-env-2 ',expr ,env))
;
;(defun eval-in-env-2 (qexpr env)
;  '(eval `(let ,env ,qexpr)))

(defmacro deftest (module test expected)
  `(progn
     (if (not (assoc ',module all-tests))
         (setf all-tests (cons (list ',module nil nil) all-tests)))
     (push (lambda ()
             (let* ((vars (second (assoc ',module all-tests)))
                    (_test ',test)
                    (_expected ',expected)
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
               (if (equal res exp)
                   (progn
                     (format t "~&    [SUCCESS] ~w~&" ',test)
                     t)
                   (progn
                     (format t "~&    [FAILURE] Test     : ~w~&" ',test)
                     (format t "~&              got      : ~w~&" (list res (random 10)))
                     (format t "~&              expected : ~w~&" exp)
                     nil))))
           (third (assoc ',module all-tests)))))

(defmacro deftestvar (module name value)
  `(progn
     (if (not (assoc ',module all-tests))
         (setf all-tests (cons (list ',module nil nil) all-tests)))
     (push (list ',name ',value)
           (second (assoc ',module all-tests)))))

(defmacro run-tests (&rest modules)
  (if (or (not modules)
          (eq (car modules) t))
      `(real-run-tests all-tests)
      `(real-run-tests (remove-if-not (lambda (x)
                                        (member (car x) ',modules))
                                      all-tests))))

;; OMFG that's not lisp, that's english o_O
(defun real-run-tests (modules)
  (loop
     for (module vars tests) in (reverse modules)
     for nb-fail = (loop
                      initially (format t "~&Module ~w :~&" module)
                      for test
                      in (reverse tests)
                      count (not (funcall test)))
     when (> nb-fail 0)
       do (format t "Module ~w failed ~w tests. Stopping.~&" module nb-fail)
       and return nil
     finally (return t)))

(defun erase-tests ()
  (setf all-tests nil))

(deftest moda nil nil)
(deftest moda (eq 42 42) t)
(deftest modb (eq 'a 'a) t)
(deftest modb (eq 'a 'b) nil)
(deftest modb (eq 'a 'c) t)
(deftest modb 1 1)
(deftest modc (+ 1 2) (+ 2 1))
(deftestvar modc x 1)
(deftest modc (+ x 2) (+ 2 1))
