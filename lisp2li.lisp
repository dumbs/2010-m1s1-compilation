
;; TODO : reste a gere les variables, les macros predefinies, les defuns.
(defun lisp2li (expr env)
  (cond ((and (atom expr) (constantp expr))
         (cons :lit expr))
        ((eq 'if (car expr))
         (list :if
               (lisp2li (second expr) env)
               (lisp2li (third expr) env)
               (lisp2li (fourth expr) env)))
        ((eq 'quote (car expr))
         (cons :lit (second expr)))
        ((fboundp (car expr))
         (cons :call (cons (first expr) (map-lisp2li (cdr expr) env))))
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
  '(:call and (:lit . 1) (:lit . 1)))

;; Test avec des expression plus complexe
(deftest lisp2li
  (lisp2li '(if (eq 1 1) 2 2) ())
  '(:if (:call eq (:lit . 1) (:lit . 1)) (:lit . 2) (:lit . 2)))

(deftest lisp2li
  (lisp2li '(if (eq "abc" 1) "abc" 2) ())
  '(:IF (:CALL EQ (:LIT . "abc") (:LIT . 1)) (:LIT . "abc") (:LIT . 2)))

(deftest lisp2li
  (lisp2li '(if (and (eq 1 1) (= 2 2)) (or 1 2) (and 1 2)) ())
  '(:if (:call and (:call eq (:lit . 1) (:lit . 1))
               (:call = (:lit . 2) (:lit . 2)))
        (:call or (:lit . 1) (:lit . 2))
        (:call and (:lit . 1) (:lit . 2))))

(deftest lisp2li
  (lisp2li '(foo 1 1) ())
  '(:unknown (foo 1 1) ()))

(run-tests t)