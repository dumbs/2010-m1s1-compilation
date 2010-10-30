;; TODO : reste a gerer les defuns, les let, les lambda expression, les setf, les progn, ...
(defun lisp2li (expr env)
  "Convertit le code LISP en un code intermédiaire reconnu
par le compilateur et par l’interpréteur"
  (cond ((and (atom expr) (constantp expr)) ; literaux
         (cons :lit expr))
        ((symbolp expr) ; symboles
         (let ((cell (get-binding env expr)))
           (if cell
               (cons :var (car cell))
             (warn "Variable ~S unknown" (car expr)))))
        ((and (consp (car expr))
              (eq 'lambda (caar expr)))
         ;; λ-expressions
         ;; => recursion sur arguments
         ;; => construction environnement
         ;; => recursion sur corps de la λ-fonction
         (error "Lambda expression NYI"))
        ((not (symbolp (car expr)))
         (warn "~S isn't a symbol" (car expr)))
        ((not (fboundp (car expr)))
         (list :unknown expr env))
        ((eq 'if (car expr)) ; if
         (list :if
               (lisp2li (second expr) env)
               (lisp2li (third expr) env)
               (lisp2li (fourth expr) env)))
        ((eq 'quote (car expr)) ;; quotes
         (cons :lit (second expr)))
        ((eq 'defun (car expr)) ;; TODO : a verifier que le cas de defun est bien gerer comme on le veux
         (cons :call (cons 'add-binding (list (list :call 'push-new-env `(:lit . ,env) '(:lit . "DEFUN"))
                                              (cons :lit (second expr))
                                              (cons :lit (cons (length (third expr))
                                                               (lisp2li (fourth expr)
                                                                        (make-stat-env env (third expr)))))))))
        ((eq 'setq (car expr))
         (cons :call (cons 'set-binding (list `(:lit . ,env)
                                              (cons :lit (second expr))
                                              (cons :lit (third expr))))))
        ((macro-function (car expr))
         (lisp2li (macroexpand-1 expr) env)) ; macros
        ((not (special-operator-p (car expr))) ; fonctions normales. (Attention) sur sbcl special-form-p ne marche pas il faut utiliser special-operator-p
                                           ; => recursion sur tous les arguments
                                           ; => eventuellement construction d'environnement
                                           ; => et analyse du corps de la fonction appelee
         (cons :call (cons (first expr) (map-lisp2li (cdr expr) env))))
        (T
         (error "special forme NYI ~S" (car expr)))
        ))

(defun map-lisp2li (expr env)
  (mapcar (lambda (x) (lisp2li x env)) expr))

(defun make-stat-env (env params) ;; TODO : Verifier si on ne doit pas plutot chercher s'il existe pas deja un environnement avec la valeur et le mettre plutot que nil.
  (mapcar (lambda (x) (add-binding env x nil)) params)
  env)

;; Test unitaire
(load "test-unitaire")
;(erase-tests)

(deftest lisp2li
  (lisp2li '3 ())
  '(:lit . 3))

(deftest lisp2li
  (lisp2li ''x ())
  '(:lit . x))

(deftest lisp2li
  (lisp2li ''(1 2 3) ())
  '(:lit 1 2 3))

;; test des if
(deftest lisp2li
  (lisp2li '(if T T nil) ())
  '(:if (:lit . T) (:lit . T) (:lit . nil)))

(deftest lisp2li
  (lisp2li '(if T nil T) ())
  '(:if (:lit . T) (:lit . nil) (:lit . T)))

;; test des fonctions predefinies
(deftest lisp2li
  (lisp2li '(eq 1 1) ())
  '(:call eq (:lit . 1) (:lit . 1)))

(deftest lisp2li
  (lisp2li '(and 1 1) ())
  '(:lit . 1))

;; test des variables
(deftest lisp2li
  (lisp2li 'x '(("TOP-LEVEL" (x . 1) (y . 2))))
  '(:var . x))

(deftest lisp2li
  (lisp2li '(if (eq x 3) (- x 3) (+ x 3)) '(("TOP-LEVEL" (x . 3))))
  '(:if (:call eq (:var . x) (:lit . 3))
        (:call - (:var . x) (:lit . 3))
        (:call + (:var . x) (:lit . 3))))

(deftest lisp2li
  (lisp2li '(if (eq x 3)
                (- z 3)
              (- x 5))
           '(("TEST" (z . 5)) ("TOP-LEVEL" (x . 4))))
  '(:IF (:CALL EQ (:VAR . X) (:LIT . 3)) (:CALL - (:VAR . Z) (:LIT . 3))
        (:CALL - (:VAR . X) (:LIT . 5))))

;; Test avec des expression plus complexe
(deftest lisp2li
  (lisp2li '(if (eq 1 1) 2 2) ())
  '(:if (:call eq (:lit . 1) (:lit . 1)) (:lit . 2) (:lit . 2)))

(deftest lisp2li
  (lisp2li '(if (eq "abc" 1) "abc" 2) ())
  '(:IF (:CALL EQ (:LIT . "abc") (:LIT . 1)) (:LIT . "abc") (:LIT . 2)))

(deftest lisp2li
  (lisp2li '(foo 1 1) ())
  '(:unknown (foo 1 1) ()))

(deftest lisp2li
  (lisp2li '(if (and (eq 1 1) (= 2 2)) (foo 1 2) (bar 3 4)) ())
  '(:IF (:CALL = (:LIT . 2) (:LIT . 2)) (:UNKNOWN (FOO 1 2) NIL) (:UNKNOWN (BAR 3 4) NIL)))

;; Test sur le setq
(deftestvar lisp2li env (add-binding (empty-env-stack) 'x 1))
(deftest lisp2li
  (lisp2li '(setq x 2) env)
  '(:call set-binding (:lit (("TOP-LEVEL" (x . 1)))) (:lit . x) (:lit . 2)))

;(run-tests t)