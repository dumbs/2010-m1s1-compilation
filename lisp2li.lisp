(load "util.lisp")
;; `
(defvar my-quasiquote (car '`(,a)))

;; ,
(defvar my-unquote (caaadr '`(,a)))

;; ,@
(defvar my-unquote-unsplice (caaadr '`(,@a)))

(defun map-lisp2li (expr env)
  (mapcar (lambda (x) (lisp2li x env)) expr))

(defun make-stat-env (params &optional env)
   (append
    (loop
    for var in params
    for j = 1 then (+ j 1)
    for num-env = (+ (or (second (first env)) -1) 1)
    collect (list var num-env j)
    )
    env))

(defun transform-quasiquote (expr)
  (cond
   ;; a
   ((atom expr)
	 `',expr)
   ;; (a)
   ((atom (car expr))
    `(cons ',(car expr)
	   ,(transform-quasiquote (cdr expr))))
   ;; (,a)
   ((eq my-unquote (caar expr))
    `(cons ,(cadar expr)
	   ,(transform-quasiquote (cdr expr))))
   ;; (,@a)
   ((eq my-unquote-unsplice (caar expr))
    (if (endp (cdr expr))
	(cadar expr)
      `(append ,(cadar expr)
	       ,(transform-quasiquote (cdr expr)))))
   ;; ((a ...) ...)
   (T
    `(cons ,(transform-quasiquote (car expr))
	   ,(transform-quasiquote (cdr expr))))))
   
(defmacro get-defun (symb)
  `(get ,symb :defun))

(defun set-defun (li)
  (setf (get-defun (cdaddr li)) (cdddr li)))

(defun lisp2li (expr env)
  "Convertit le code LISP en un code intermédiaire reconnu
par le compilateur et par l’interpréteur"
  (cond
   ;; literaux
   ((and (atom expr) (constantp expr)) 
    (cons :const expr))
   ;; symboles
   ((symbolp expr)
    (let ((cell (assoc expr env)))
      (if cell
          `(:cvar ,(cadr cell) ,(caddr cell))
        (error "Variable ~S unknown" expr))))
   ;; lambda solitaire ex: (lambda (x) x)
   ((eq 'lambda (car expr))
    `(:lclosure . ,(cons (length (second expr))
                         (lisp2li (third expr)
                                  (make-stat-env (second expr))))))
   ;; lambda ex: ((lambda (x) x) 1)
   ((and (consp (car expr))
         (eq 'lambda (caar expr)))
    `(:mcall ,(lisp2li (car expr) env)
            ,@(mapcar (lambda (param)
                        (lisp2li param env))
                      (cdr expr))))
   ;; (not-symbol ...)
   ((not (symbolp (car expr)))
    (warn "~S isn't a symbol" (car expr)))
   ;; fonction inconnue
   ((and (not (fboundp (car expr)))
         (not (get-defun (car expr))))
    `(:unknown ,expr ,env))
   ;; if
   ((eq 'if (car expr))
    (list :if
          (lisp2li (second expr) env)
          (lisp2li (third expr) env)
          (lisp2li (fourth expr) env)))
   ;; quote
   ((eq 'quote (car expr)) 
    (cons :const (second expr)))
   ;; quasiquote `
   ((eq my-quasiquote (car expr))
    (lisp2li (transform-quasiquote (cadr expr)) env))
   ;; #'fn (FUNCTION fn)
   ((eq 'function (car expr))
    `(:sclosure (cadr expr)))
   ;; defun
   ((eq 'defun (car expr))
    `(:mcall set-defun (:const . ,(second expr))
                ,(lisp2li `(lambda ,(third expr) ,@(cdddr expr)) env)))
   ;; apply
   ((eq 'apply (car expr))
    `(:sapply ,(second expr) (cddr expr)))
   ;; setf
   ((eq 'setf (car expr))
    (if (symbolp (cadr expr))
        (let ((cell (assoc (cadr expr) env)))
          `(:set-var (,(second cell) ,(third cell)) ,(third expr)))
      `(:set-fun ,(caadr expr) ,@(last expr) ,@(cdadr expr))))
   ;; progn
   ((eq 'progn (car expr))
    (cons :progn (map-lisp2li (cdr expr) env)))
   ;; macros
   ((macro-function (car expr))
    (lisp2li (macroexpand-1 expr) env))
   ;; foctions normales
   ((not (special-operator-p (car expr)))
    `(:call ,(first expr) ,@(map-lisp2li (cdr expr) env)))
   (T
    (print expr)
    (error "special form not yet implemented ~S" (car expr)))))

;; Test unitaire
(load "test-unitaire")
(erase-tests lisp2li)

(deftest (lisp2li make-stat-env)
  (make-stat-env '(x y z))
  '((x 0 1) (y 0 2) (z 0 3)))

(deftest (lisp2li make-stat-env)
  (make-stat-env '(a b c d))
  '((a 0 1) (b 0 2) (c 0 3) (d 0 4)))

(deftest (lisp2li make-stat-env)
  (make-stat-env '(a b) '((x 0 1) (y 0 2)))
  '((a 1 1) (b 1 2) (x 0 1) (y 0 2)))

(deftest (lisp2li constante)
  (lisp2li '3 ())
  '(:const . 3))

(deftest (lisp2li constante)
  (lisp2li ''x ())
  '(:const . x))

(deftest (lisp2li constante)
  (lisp2li ''(1 2 3) ())
  '(:const 1 2 3))

(deftest (lisp2li defun)
  (lisp2li '(defun foo (x) x) ())
  '(:mcall set-defun (:const . foo) (:lclosure 1 :cvar 0 1)))

(deftest (lisp2li defun)
  (lisp2li '(defun foo (x y z) (list x y z)) ())
  '(:mcall set-defun (:const . foo)
           (:lclosure 3 :call list
                      (:cvar 0 1)
                      (:cvar 0 2)
                      (:cvar 0 3))))

(deftest (lisp2li setf)
  (lisp2li '(setf y 42) '((x 0 1) (y 0 2)))
  '(:set-var (0 2) 42))

(deftest (lisp2li setf)
  (lisp2li '(setf (cdr '(1 2 3)) 42) ())
  '(:set-fun cdr 42 '(1 2 3)))

(deftest (lisp2li lambda)
  (lisp2li '(mapcar (lambda (x y z) (list x y z)) '(1 2 3)) ())
  '(:call mapcar (:lclosure 3 :call list
                            (:cvar 0 1)
                            (:cvar 0 2)
                            (:cvar 0 3))
          (:const 1 2 3)))