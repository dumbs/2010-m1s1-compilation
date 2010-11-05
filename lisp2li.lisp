(load "util.lisp")
;; `
(defvar my-quasiquote (car '`(,a)))

;; ,
(defvar my-unquote (caaadr '`(,a)))

;; ,@
(defvar my-unquote-unsplice (caaadr '`(,@a)))

(defun map-lisp2li (expr env)
  (mapcar (lambda (x) (lisp2li x env)) expr))

(defun make-stat-env-optional (params env position num-env)
  (cond ((endp params)
         env)
        ((consp (car params))
         `((,(caar params) ,num-env ,position)
           (,(intern (format nil "~a-P" (caar params))) ,num-env ,(+ 1 position))
           . ,(make-stat-env-optional (cdr params) env (+ 2 position) num-env)))
        ((eq '&rest (car params))
         (make-stat-env (cdr params) env position num-env))
        (T
         `((,(car params) ,num-env ,position)
           . ,(make-stat-env-optional (cdr params) env (+ 1 position) num-env)))))
  
(defun make-stat-env (params &optional env (position 1) num-env)
  (unless num-env (setq num-env (+ (or (second (first env)) -1) 1)))
  (cond ((endp params)
         env)
        ((eq '&optional (car params))
         (make-stat-env-optional (cdr params) env position num-env))
        ((eq '&rest (car params))
         (make-stat-env (cdr params) env position num-env))
        (T
         `((,(car params) ,num-env ,position)
           . ,(make-stat-env (cdr params) env (+ 1 position))))))

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

(defun get-nb-params (params)
  "renvoie le nombre exact de paramètres sans les &optional et &rest"
  (defun get-nb-params-t (params r)
    (cond ((endp params)
           r)
          ((or (eq '&optional (car params))
               (eq '&rest (car params)))
           (get-nb-params-t (cdr params) r))
          (T
           (get-nb-params-t (cdr params) (+ 1 r)))))
  (get-nb-params-t params 0))

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
   ((eq 'lambda (car expr)) ;; TODO : ameliorer le cas du lambda
    (if (member '&rest (second expr))
      `(:lclosure . (,(get-nb-params (second expr))
                           ,(+ 1 (position '&rest (second expr)))
                           ,(lisp2li (caddr expr)
                                    (make-stat-env (second expr)))))
      `(:lclosure . ,(cons (get-nb-params (second expr))
                           (lisp2li (caddr expr)
                                    (make-stat-env (second expr)))))))
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
    `(:sclosure ,(cadr expr)))
   ;; defun
   ((eq 'defun (car expr))
    `(:mcall set-defun (:const . ,(second expr))
                ,(lisp2li `(lambda ,(third expr) ,@(cdddr expr)) env)))
   ;; apply
   ((eq 'apply (car expr))
    `(:sapply ,(second expr) ,@(cddr expr)))
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
    (lisp2li (macroexpand expr) env))
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

(deftest (lisp2li make-stat-env)
  (make-stat-env '(a b &optional c &rest d))
  '((a 0 1) (b 0 2) (c 0 3) (d 0 4)))

(deftest (lisp2li make-stat-env)
  (make-stat-env '(x y &optional (z t)))
  '((x 0 1) (y 0 2) (z 0 3) (z-p 0 4)))

(deftest (lisp2li constante)
  (lisp2li '3 ())
  '(:const . 3))

(deftest (lisp2li constante)
  (lisp2li ''x ())
  '(:const . x))

(deftest (lisp2li constante)
  (lisp2li ''(1 2 3) ())
  '(:const 1 2 3))

(deftest (lisp2li variables)
  (lisp2li 'x '((x 0 1) (y 0 2)))
  '(:cvar 0 1))

(deftest (lisp2li fonctions-normales)
  (lisp2li '(+ 3 4) ())
  '(:call + (:const . 3) (:const . 4)))

(deftest (lisp2li fonctions-normales)
  (lisp2li '(list 3 4 (* 6 7)) ())
  '(:call list (:const . 3) (:const . 4)
          (:call * (:const . 6) (:const . 7))))

(deftest (lisp2li if)
  (lisp2li '(if T T nil) ())
  '(:if (:const . T) (:const . T) (:const . nil)))

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

(deftest (lisp2li lambda)
  (lisp2li '((lambda (x y z) (list x y z)) 1 2 3) ())
  '(:mcall (:lclosure 3 :call list
                     (:cvar 0 1)
                     (:cvar 0 2)
                     (:cvar 0 3))
          (:const . 1) (:const . 2) (:const . 3)))

(deftest (lisp2li lambda)
  (lisp2li `(lambda (x y z) (list x y z)) ())
  '(:lclosure 3 :call list
                      (:cvar 0 1)
                      (:cvar 0 2)
                      (:cvar 0 3)))

(deftest (lisp2li unknown)
  (lisp2li '(foo 3) ())
  '(:unknown (foo 3) ()))

(deftest (lisp2li function)
  (lisp2li '#'car ())
  '(:sclosure car))

(deftest (lisp2li apply)
  (lisp2li '(apply 'list '(1 2 3)) ())
  '(:sapply 'list '(1 2 3)))

(deftest (lisp2li apply)
  (lisp2li '(apply #'list '(1 2 3)) ())
  '(:sapply #'list '(1 2 3)))

(deftest (lisp2li progn)
  (lisp2li '(progn (list 1 2 3) (+ 3 4)) ())
  '(:progn (:call list
                  (:const . 1)
                  (:const . 2)
                  (:const . 3))
           (:call +
                  (:const . 3)
                  (:const . 4))))

(deftest (lisp2li macro)
  (lisp2li '(cond ((eq (car '(1 2 3)) 1) T)
                  ((eq (car '(1 2 3)) 2) 2)
                  (T nil))
           ())
  '(:if (:call eq (:call car (:const 1 2 3)) (:const . 1))
        (:const . T)
        (:if (:call eq (:call car (:const 1 2 3)) (:const . 2))
             (:const . 2)
             (:const . nil))))

(deftest (lisp2li macro)
  (lisp2li '(and (eq (car '(1 2)) 1)
                 T)
           ())
  '(:if (:call not
               (:call eq (:call car (:const 1 2)) (:const . 1)))
        (:const . nil)
        (:const . T)))