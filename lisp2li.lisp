;; TODO : reste a gerer les defuns, les let, les lambda expression, les setf, les progn, ...
(defun lisp2li (expr env)
  (cond ((and (atom expr) (constantp expr)) ;;cas des litteraux
         (cons :lit expr))
        ((atom expr) ;;cas des variables
         (let ((cell (get-binding env expr)))
           (if cell
               (cons :var (car cell))
             (warn "Variable ~S unknown" (car expr)))))
        ((eq 'if (car expr)) ;;cas des if
         (list :if
               (lisp2li (second expr) env)
               (lisp2li (third expr) env)
               (lisp2li (fourth expr) env)))
        ((eq 'quote (car expr)) ;;cas des quotes
         (cons :lit (second expr)))
        ((and (fboundp (car expr)) (eq (macroexpand-1 expr) expr)) ;;cas des fonctions
         (cons :call (cons (first expr) (map-lisp2li (cdr expr) env))))
        ((and (fboundp (car expr)) (not (eq (macroexpand-1 expr) expr))) ;;cas des macros
         (lisp2li (macroexpand-1 expr) env))
        (T (list :unknown expr env))
        ))

(defun map-lisp2li (expr env)
  (mapcar (lambda (x) (lisp2li x env)) expr))

;; Test unitaire
(load "test-unitaire")
;(erase-tests)
;; test des litteraux
(deftest lisp2li
  (lisp2li 1 ())
  '(:lit . 1))

(deftest lisp2li
  (lisp2li 2.3 ())
  '(:lit . 2.3))

(deftest lisp2li
  (lisp2li "abc" ())
  '(:lit . "abc"))

(deftest lisp2li
  (lisp2li T ())
  '(:lit . T))

(deftest lisp2li
  (lisp2li nil ())
  '(:lit . nil))

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
  '(:if (:lit . 1) (:call the (:lit . T) (:lit . 1)) (:lit . nil)))

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
  (lisp2li '(if (and (eq 1 1) (= 2 2)) (foo 1 2) (bar 3 4)) ())
  '(:if (:if (:call eq (:lit . 1) (:lit . 1))
             (:call the (:lit . T) (:call = (:lit . 2) (:lit . 2))) (:lit . nil))
        (:unknown (foo 1 2) nil) (:unknown (bar 3 4) nil)))

(deftest lisp2li
  (lisp2li '(foo 1 1) ())
  '(:unknown (foo 1 1) ()))

;(run-tests t)