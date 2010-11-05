(load "environnement")
(erase-tests lisp2li)

;; `
(defvar my-quasiquote (car '`(,foo)))

;; ,
(defvar my-unquote (caaadr '`(,foo)))

;; ,@
(defvar my-unquote-unsplice (caaadr '`(,@foo)))

(defun map-lisp2li (expr env-var env-fun)
  (mapcar (lambda (x) (lisp2li x env-var env-fun)) expr))

(defun map-lisp2li-let (expr env)
  (mapcar (lambda (x) (add-binding env (car x) (lisp2li (cadr x) env))) (cadr expr)))

(defun make-stat-env (env params)
  (mapcar (lambda (x) (add-binding env x nil)) params)
  env)

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

(defun lisp2li (expr env-var env-fun)
  "Convertit le code LISP en un code intermédiaire reconnu
par le compilateur et par l’interpréteur"
  (cond ((null env-var) (lisp2li expr (empty-env-stack) env-fun))
	((null env-fun) (lisp2li expr env-var (empty-env-stack)))
	;; literaux
        ((and (atom expr) (constantp expr)) 
         (cons :lit expr))
	;; symboles
        ((symbolp expr)
         (let ((cell (get-binding env-var expr)))
           (if cell
               (cons :var (car cell))
             (error "Variable ~S unknown" expr))))
	;; lambda solitaire ex: (lambda (x) x)
        ((eq 'lambda (car expr))
         (let ((env-bis (make-stat-env (push-new-env env-var "LAMBDA") (second expr))))
           `(:lclosure (,env-bis . ,env-fun)
                       ,(lisp2li (third expr)
                                 env-bis env-fun))))
	;; lambda ex: ((lambda (x) x) 1)
        ((and (consp (car expr))
              (eq 'lambda (caar expr)))
         `(:call ,(lisp2li (car expr) env-var env-fun)
                 ,@(mapcar (lambda (param)
                             (lisp2li param env-var env-fun))
                           (cdr expr))))
	;; (not-symbol ...)
        ((not (symbolp (car expr)))
         (warn "~S isn't a symbol" (car expr)))
	;; fonction inconnue
        ((and (not (fboundp (car expr))) (not (get-binding env-fun (car expr))))
         `(:unknown ,expr (,env-var . ,env-fun)))
	;; if
        ((eq 'if (car expr))
         (list :if
               (lisp2li (second expr) env-var env-fun)
               (lisp2li (third expr) env-var env-fun)
               (lisp2li (fourth expr) env-var env-fun)))
	;; quote
        ((eq 'quote (car expr)) 
         (cons :lit (second expr)))
	;; quasiquote `
	((eq my-quasiquote (car expr))
	 (lisp2li (transform-quasiquote (cadr expr)) env-var env-fun))
	;; #'fn (FUNCTION fn)
	((eq 'function (car expr))
	 (list :call 'function (car expr)))
        ;; defun
	((eq 'defun (car expr)) 
         (let ((env-bis (make-stat-env (push-new-env env-var "DEFUN") (third expr))))
           (add-top-level-binding env-fun
                                  (second expr)
                                  (cons :lclosure (cons (cons env-bis env-fun)
                                                        (map-lisp2li (cdddr expr)
								     env-bis env-fun)))))
         (cons :lit (second expr)))
	;; defvar
	((eq 'defvar (car expr))
	 (add-top-level-binding env-var
				(second expr)
				(lisp2li (third expr) env-var env-fun)))
	;; setq/setf
        ((eq 'setq (car expr))
         (cons :call (cons 'set-binding (list `(:lit . ,env-var)
                                              (cons :lit (second expr))
                                              (cons :lit (third expr))))))
	;; let
        ((eq 'let (car expr)) 
         (let ((bindings (cadr expr))
               (body (cddr expr)))
           (lisp2li `((lambda ,(mapcar #'car bindings)
               ,@body)
                      ,@(mapcar #'cadr bindings)) env-var env-fun)))
        ;; let*
        ((eq 'let* (car expr))
         (let ((bindings (cadr expr))
               (body (caddr expr)))
           (lisp2li (if (endp bindings)
                        body
                      `(let (,(car bindings))
                         (let* ,(cdr bindings)
                           ,body))) env-var env-fun)))
        ;; labels
        ((eq 'labels (car expr))
         (let ((bindings (cadr expr))
               (body (cddr expr)))
           (lisp2li `((lambda ,(mapcar #'car bindings)
                        ,@body)
                      ,@(mapcar #'cadr bindings)) env-var env-fun)))
        ;; `
        ((eq '` (car expr))
         (print "Ca marche"))
	;; progn
	((eq 'progn (car expr))
	 (cons :progn (map-lisp2li (cdr expr) env-var env-fun)))
	;; macros
        ((macro-function (car expr))
         (lisp2li (macroexpand-1 expr) env-var env-fun))
	;; fonctions normales
        ((not (special-operator-p (car expr)))
         (cons :call (cons (first expr) (map-lisp2li (cdr expr) env-var env-fun))))
        (T
	 (print expr)
         (error "special form not yet implemented ~S" (car expr)))
        ))

(defun map-lisp2li (expr env-var env-fun)
  (mapcar (curry #'lisp2li :skip env-var env-fun) expr))

(defun map-lisp2li-let (expr env)
  (mapcar (lambda (x) (add-binding env (car x) (lisp2li (cadr x) env))) (cadr expr)))

(defun make-stat-env (env params)
  (mapcar (lambda (x) (add-binding env x nil)) params)
  env)

;; Test unitaire
(load "test-unitaire")
(erase-tests lisp2li)

(deftest (lisp2li :lit)
  (lisp2li '3 () ())
  '(:lit . 3))

(deftest (lisp2li :lit)
  (lisp2li ''x () ())
  '(:lit . x))

(deftest (lisp2li :lit)
  (lisp2li ''(1 2 3) () ())
  '(:lit 1 2 3))

;; test des if
(deftest (lisp2li :if)
  (lisp2li '(if T T nil) () ())
  '(:if (:lit . T) (:lit . T) (:lit . nil)))

(deftest (lisp2li :if)
  (lisp2li '(if T nil T) () ())
  '(:if (:lit . T) (:lit . nil) (:lit . T)))

;; test des fonctions predefinies
(deftest (lisp2li :call)
  (lisp2li '(eq 1 1) () ())
  '(:call eq (:lit . 1) (:lit . 1)))

(deftest (lisp2li macros)
  (lisp2li '(and 1 1) () ())
  '(:lit . 1))

;; test des variables
(deftest (lisp2li :var)
  (lisp2li 'x '(("TOP-LEVEL" (x . 1) (y . 2))) ())
  '(:var . x))

(deftest (lisp2li :var)
  (lisp2li '(if (eq x 3) (- x 3) (+ x 3)) '(("TOP-LEVEL" (x . 3))) ())
  '(:if (:call eq (:var . x) (:lit . 3))
        (:call - (:var . x) (:lit . 3))
        (:call + (:var . x) (:lit . 3))))

(deftest (lisp2li :var)
  (lisp2li '(if (eq x 3)
                (- z 3)
              (- x 5))
           '(("TEST" (z . 5)) ("TOP-LEVEL" (x . 4))) ())
  '(:IF (:CALL EQ (:VAR . X) (:LIT . 3)) (:CALL - (:VAR . Z) (:LIT . 3))
        (:CALL - (:VAR . X) (:LIT . 5))))

;; Test avec des expression plus complexe
(deftest (lisp2li complexe)
  (lisp2li '(if (eq 1 1) 2 2) () ())
  '(:if (:call eq (:lit . 1) (:lit . 1)) (:lit . 2) (:lit . 2)))

(deftest (lisp2li complexe)
  (lisp2li '(if (eq "abc" 1) "abc" 2) () ())
  '(:IF (:CALL EQ (:LIT . "abc") (:LIT . 1)) (:LIT . "abc") (:LIT . 2)))

(deftest (lisp2li :unknown)
  (lisp2li '(foo 1 1) () ())
  '(:unknown (foo 1 1) ((("TOP-LEVEL")) ("TOP-LEVEL"))))

(deftest (lisp2li :unknown)
  (lisp2li '(if (and (eq 1 1) (= 2 2)) (foo 1 2) (bar 3 4)) () ())
  '(:IF (:CALL = (:LIT . 2)
	       (:LIT . 2))
	(:UNKNOWN (FOO 1 2) ((("TOP-LEVEL")) ("TOP-LEVEL")))
	(:UNKNOWN (BAR 3 4) ((("TOP-LEVEL")) ("TOP-LEVEL")))))

;; Test sur le setq
(deftestvar (lisp2li setq) env (add-binding (empty-env-stack) 'x 1))
(deftest (lisp2li setq)
  (lisp2li '(setq x 2) env ())
  '(:call set-binding (:lit . (("TOP-LEVEL" (x . 1)))) (:lit . x) (:lit . 2)))

;; Test sur le defun
(deftest (lisp2li defun valeur-de-retour)
  (lisp2li '(defun fact (n r) 
	      (if (= n 0) 
		  r
		(fact (- n 1) (* n r))))
	   () ())
  '(:lit . fact))

(deftestvar (lisp2li defun environnement) env (empty-env-stack))
(deftest (lisp2li defun environnement)
  (progn 
    (lisp2li '(defun fact (n r)
		(if (= n 0)
		    r
		  (fact (- n 1) (* n r))))
	     () env)
    env)
  '#1=(("TOP-LEVEL"
     (FACT :LCLOSURE (#2=(("DEFUN" (R) (N)) ("TOP-LEVEL")) . #1#)
      (:IF (:CALL = (:VAR . N) (:LIT . 0)) (:VAR . R)
       (:UNKNOWN (FACT (- N 1) (* N R)) (#2# . #1#)))))))

;; Test sur la lambda expression
(deftest lisp2li
  (lisp2li '(mapcar (lambda (x) x) '(1 2 3))
	   () ())
  '(:call mapcar (:lclosure ((("LAMBDA" (X)) ("TOP-LEVEL")) ("TOP-LEVEL"))
                            (:var . x)) (:lit 1 2 3)))

(deftest lisp2li
  (lisp2li '((lambda (x y z) (list x y z)) 1 2 3) () ())
  '(:call (:lclosure ((("LAMBDA" (Z) (Y) (X)) ("TOP-LEVEL")) ("TOP-LEVEL"))
                     (:call list (:var . x) (:var . y) (:var . z)))
          (:lit . 1) (:lit . 2) (:lit . 3)))

;; Test sur le LET
(deftest lisp2li
  (lisp2li '(let ((x 1) (y 2)) (list x y)) () ())
  '(:call (:lclosure ((("LAMBDA" (Y) (X)) ("TOP-LEVEL")) ("TOP-LEVEL"))
                     (:call list (:var . x) (:var . y)))
          (:lit . 1) (:lit . 2)))

(deftest lisp2li
  (lisp2li '(let ((x 1) (y (+ x 2))) (list x y)) '(("TOP-LEVEL" (x . 1))) ())
  '(:call (:lclosure ((("LAMBDA" (Y) (X)) ("TOP-LEVEL" (x . 1))) ("TOP-LEVEL"))
                     (:call list (:var . x) (:var . y)))
          (:lit . 1) (:call + (:var . x) (:lit . 2))))

;; Test sur le LET*
(deftest lisp2li
  (lisp2li '(let* ((x 1) (y (+ x 2))) (list x y)) () ())
  '(:CALL (:LCLOSURE ((("LAMBDA" (X)) ("TOP-LEVEL")) ("TOP-LEVEL"))
                     (:CALL (:LCLOSURE ((("LAMBDA" (Y)) ("LAMBDA" (X)) ("TOP-LEVEL")) ("TOP-LEVEL"))
				       (:CALL LIST (:VAR . X) (:VAR . Y)))
			    (:CALL + (:VAR . X) (:LIT . 2))))
          (:LIT . 1)))

;(run-tests t)