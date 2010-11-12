(load "util.lisp")
(load "match.lisp")

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
         (make-stat-env1 (cdr params) env position num-env))
        (T
         `((,(car params) ,num-env ,position)
           . ,(make-stat-env-optional (cdr params) env (+ 1 position) num-env)))))

(defun env-depth (env)
  (+ (or (second (first env)) -1) 1))

(defun recalculation (env)
  (cond ((endp env)
         env)
        (T
         `((,(caar env) ,(+ 1 (cadar env)) ,(caddar env))
           . ,(recalculation (cdr env))))))

(defun make-stat-env (params &optional env (position 1))
  (defun make-stat-env1 (params &optional env (position 1) num-env)
    (cond ((endp params)
           env)
          ((eq '&optional (car params))
           (make-stat-env-optional (cdr params) env position num-env))
          ((eq '&rest (car params))
           (make-stat-env1 (cdr params) env position num-env))
          (T
           `((,(car params) 0 ,position)
             . ,(make-stat-env1 (cdr params) env (+ 1 position) num-env)))))
  (make-stat-env1 params (recalculation env) position 0))

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
  "Renvoie le nombre exact de paramètres sans les &optional et &rest"
  (defun get-nb-params-t (params r)
    (cond ((endp params)
           r)
          ((or (eq '&optional (car params))
               (eq '&rest (car params)))
           (get-nb-params-t (cdr params) r))
          (T
           (get-nb-params-t (cdr params) (+ 1 r)))))
  (get-nb-params-t params 0))

(defun implicit-progn (expr)
  (if (n-consp 2 expr)
      (cons 'progn  expr)
    (car expr)))


(defun simplify (li) ;; TODO : a finir
  (cond-match li
              ((:nil :progn :expr _)
               (simplify expr))
              ((:nil :progn :body0 _* (:nil :progn :body1 _*)+ :body2 _*)
               (simplify `(:progn ,@body0 ,@(car body1) ,@body2)))
              ((:nil :progn :body0 _* (:nil :let :size (? integerp) :body1 _*)+ :body2 _*)
               (simplify `(:let ,@size ,@body0 ,@(car body1) ,@body2)))
              ((:nil :let :size1 (? integerp) :body1 _*
                     (:nil :let :size2 (? integerp)
                           (:nil :set-var (:depth-set (? integerp) :index-set (? integerp)) @.)+
                           :var1 (:nil :cvar :depth1 (? integerp) :index1 (? integerp))*
                           :body2 _* :var2 (:nil :cvar :depth2 (? integerp) :index2 (? integerp))*)
                     :body3 _*)
               (simplify `(:let ,(+ size1 size2)
                                ,@body1
                                ,@(mapcar
                                 (lambda (depth index)
                                   `(:cvar ,(- depth 1) ,(+ index size1))) depth1 index1)
                                ,@body2
                                ,@(if (and depth2 index2)
                                     (mapcar
                                        (lambda (depth index)
                                          `(:cvar ,(- depth 1) ,(+ index size1))) depth2 index2))
                                ,@body3)))
              ((:nil :let :body0 _* (:nil :progn :body1 _*)+ :body2 _*)
               (simplify `(:let ,@body0 ,@(car body1) ,@body2)))
              ((:nil :let :size1 (? integerp) :body0 _* (:nil :let :size2 (? integerp) :body1 _*)+ :body2 _*)
               (simplify `(:let ,(+ size1 (car size2)) ,@body0 ,@(car body1) ,@body2)))
              ((:nil :if (:nil :const :cond . _) :then @. :else @.)
               (simplify (if cond then else)))
              (@. li)))

(defun lisp2li (expr &optional env)
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
                           ,(+ 1 (mposition '&rest (second expr)))
                           ,@(lisp2li (implicit-progn (cddr expr))
                                    (make-stat-env (second expr) env))))
      `(:lclosure . ,(cons (get-nb-params (second expr))
                           (lisp2li (implicit-progn (cddr expr))
                                    (make-stat-env (second expr) env))))))
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
   ;; fonction meta-definie
   ((get (car expr) :defun)
    `(:mcall ,(car expr)
          ,@(mapcar (lambda (x) (lisp2li x env)) (cdr expr))))
   ;; fonction inconnue
   ((and (not (fboundp (car expr)))
         (not (get (car expr) :defun)))
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
   ;; let
   ((eq 'let (car expr))
    (match (let :bindings ((:names $ :values _)*) :body _*) expr
           (let ((new-env (make-stat-env names env)))
             `(:let ,(length bindings)
                    ,@(mapcar
                       (lambda (name value)
                         (let ((cell (assoc name new-env)))
                           `(:set-var (,(second cell) ,(third cell))
                                      ,(lisp2li value new-env))))
                       names values)
                    ,(lisp2li (implicit-progn body) new-env)))))
   ((eq 'let* (car expr))
    (cond-match expr
                (((? (eq x 'let*)) :bindings () :body _*)
                 (lisp2li (implicit-progn body) env))
                (((? (eq x 'let*)) :bindings ((:name $ :value _) :rest ($ _)*) :body _*)
                 (lisp2li `(let ((,name ,value))
                             (let* ,rest
                               ,@body)) env))))
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
          `(:set-var (,(second cell) ,(third cell))
                     ,(lisp2li (third expr) env)))
      `(:set-fun ,(caadr expr) ,@(last expr) ,@(cdadr expr))))
   ;; setq
   ((eq 'setq (car expr))
    (lisp2li `(setf ,@(cdr expr)) env))
   ;; progn
   ((eq 'progn (car expr))
    (cons :progn (map-lisp2li (cdr expr) env)))
   ;; declaim
   ((eq 'declaim (car expr))
    (cons :const nil))
   ;; macros
   ((macro-function (car expr))
    (lisp2li (macroexpand expr) env))
   ;; foctions normales
   ((not (special-operator-p (car expr)))
    `(:call ,(first expr) ,@(map-lisp2li (cdr expr) env)))
   (T
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
  '((a 0 1) (b 0 2) (x 1 1) (y 1 2)))

(deftest (lisp2li make-stat-env)
  (make-stat-env '(a b &optional c &rest d))
  '((a 0 1) (b 0 2) (c 0 3) (d 0 4)))

(deftest (lisp2li make-stat-env)
  (make-stat-env '(x y &optional (z t)))
  '((x 0 1) (y 0 2) (z 0 3) (z-p 0 4)))

;; (deftest (lisp2li simplify :progn)
;;   (simplify '(:progn (:const . 3)))
;;   '(:const . 3))

;; (deftest (lisp2li simplify :progn)
;;   (simplify '(:progn (:call list (:const . 1) (:const . 2))))
;;   '(:call list (:const . 1) (:const . 2)))

;; (deftest (lisp2li simplify :progn)
;;   (simplify '(:progn (:progn (:const . 3) (:const . 4))))
;;   '(:progn (:const . 3) (:const . 4)))

;; (deftest (lisp2li simplify :progn)
;;   (simplify '(:progn (:const . 3) (:const . 4) (:progn (:const . 5) (:const . 6))))
;;   '(:progn (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :progn)
;;   (simplify '(:progn (:const . 1) (:const . 2) (:progn (:const . 3) (:const . 4)) (:const . 5) (:const . 6)))
;;   '(:progn (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :progn)
;;   (simplify '(:progn (:const . 1) (:const . 2) (:progn (:const . 3) (:const . 4)) (:const . 5) (:progn (:const . 6) (:const . 7))))
;;   '(:progn (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6) (:const . 7)))

;; (deftest (lisp2li simplify :let-progn)
;;   (simplify '(:let (:progn (:const . 3) (:const . 4))))
;;   '(:let (:const . 3) (:const . 4)))

;; (deftest (lisp2li simplify :let-progn)
;;   (simplify '(:let (:const . 3) (:const . 4) (:progn (:const . 5) (:const . 6))))
;;   '(:let (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :let-progn)
;;   (simplify '(:let (:const . 1) (:const . 2) (:progn (:const . 3) (:const . 4)) (:const . 5) (:const . 6)))
;;   '(:let (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :let-progn)
;;   (simplify '(:let (:const . 1) (:const . 2) (:progn (:const . 3) (:const . 4)) (:const . 5) (:progn (:const . 6) (:const . 7))))
;;   '(:let (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6) (:const . 7)))

;; (deftest (lisp2li simplify :progn-let)
;;   (simplify '(:progn (:let 0 (:const . 3) (:const . 4))))
;;   '(:let 0 (:const . 3) (:const . 4)))

;; (deftest (lisp2li simplify :progn-let)
;;   (simplify '(:progn (:let 2 (:const . 3) (:const . 4) (:progn (:const . 5) (:const . 6)))))
;;   '(:let 2 (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :progn-let)
;;   (simplify '(:progn (:const . 1) (:const . 2) (:let 1 (:const . 3) (:const . 4)) (:const . 5) (:const . 6)))
;;   '(:let 1 (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :progn-let)
;;   (simplify '(:progn (:const . 1) (:const . 2) (:let 5 (:const . 3) (:const . 4)) (:const . 5) (:progn (:const . 6) (:const . 7))))
;;   '(:let 5 (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6) (:const . 7)))

;; (deftest (lisp2li simplify :let-let)
;;   (simplify '(:let 1 (:let 1 (:const . 3) (:const . 4))))
;;   '(:let 2 (:const . 3) (:const . 4)))

;; (deftest (lisp2li simplify :let-let)
;;   (simplify '(:let 3 (:let 2 (:const . 3) (:const . 4) (:progn (:const . 5) (:const . 6)))))
;;   '(:let 5 (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :let-let)
;;   (simplify '(:let 2 (:const . 1) (:const . 2) (:let 1 (:const . 3) (:const . 4)) (:const . 5) (:const . 6)))
;;   '(:let 3 (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6)))

;; (deftest (lisp2li simplify :let-let)
;;   (simplify '(:let 5 (:const . 1) (:const . 2) (:let 5 (:const . 3) (:const . 4)) (:const . 5) (:let 2 (:const . 6) (:const . 7))))
;;   '(:let 12 (:const . 1) (:const . 2) (:const . 3) (:const . 4) (:const . 5) (:const . 6) (:const . 7)))

;; (deftest (lisp2li simplify :if)
;;   (simplify '(:if (:const . nil) (:call list 1 2 3) (:const . T)))
;;   '(:const . T))

;; (deftest (lisp2li simplify :if)
;;   (simplify '(:if (:const . nil) (:progn (:let 2 (:const . 1)) (:const . 2)) (:call list (:const 1 2 3))))
;;   '(:call list (:const 1 2 3)))

;; (deftest (lisp2li simplify :if)
;;   (simplify '(:if (:const . nil) (:const . nil) (:progn (:let 2 (:const . 1)) (:const . 2)) ))
;;   '(:let 2 (:const . 1) (:const . 2)))

;; (deftest (lisp2li simplify :if)
;;   (simplify '(:if (:const . 2) (:const . nil) (:const . T)))
;;   '(:const . nil))

;; (deftest (lisp2li simplify :if)
;;   (simplify '(:if (:const . T) (:const . 3) (:const . 4)))
;;   '(:const . 3))

;; (deftest (lisp2li simplify :if)
;;   (simplify '(:if (:const 1 2 3) (:progn (:let 3 (:const . 3)) (:let 4 (:const . 4))) (:const . 4)))
;;   '(:let 7 (:const . 3) (:const . 4)))

;;  (deftest (lisp2li simplify :let-cvar)
;;    (simplify '(:let 3 (:const . T) (:let 4 (:cvar 1 1) (:const . 4))))
;;    '(:let 7 (:const . T) (:cvar 0 4) (:const . 4)))

;;  (deftest (lisp2li simplify :let-cvar)
;;    (simplify '(:progn (:cvar 0 1)
;;                       (:LET 1 (:CONST . T)
;;                             (:LET 2 (:CVAR 0 1) (:cvar 1 1) (:cvar 2 1) (:CONST . 4)))))
;;    '(:let 3 (:cvar 0 1) (:const . T) (:cvar 0 1) (:cvar 0 2) (:cvar 1 2) (:const . 4)))


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
  (lisp2li '(defun bar (x) x) ())
  '(:mcall set-defun (:const . bar) (:lclosure 1 :cvar 0 1)))

(deftest (lisp2li defun)
  (lisp2li '(defun foo (x y z) (list x y z)) ())
  '(:mcall set-defun (:const . foo)
           (:lclosure 3 :call list
                      (:cvar 0 1)
                      (:cvar 0 2)
                      (:cvar 0 3))))

(deftest (lisp2li setf)
  (lisp2li '(setf y 42) '((x 0 1) (y 0 2)))
  '(:set-var (0 2) (:const . 42)))

(deftest (lisp2li setf)
  (lisp2li '(setf (cdr '(1 2 3)) 42) ())
  '(:set-fun cdr 42  '(1 2 3)))

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

(deftest (lisp2li lambda)
  (lisp2li `(lambda (x y z) (list x y z) (+ x y)) ())
  '(:lclosure 3 :progn (:call list
                               (:cvar 0 1)
                               (:cvar 0 2)
                               (:cvar 0 3))
                        (:call +
                               (:cvar 0 1)
                               (:cvar 0 2))))

(deftest (lisp2li rest)
  (lisp2li `(lambda (x &rest y) (cons x y)) ())
  '(:lclosure 2 2 :call cons
              (:cvar 0 1)
              (:cvar 0 2)))

(deftest (lisp2li unknown)
  (lisp2li '(bar 3) ())
  '(:unknown (bar 3) ()))

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

(deftest (lisp2li let)
  (lisp2li '(let ((x 1) (y 2))
              (cons x y)) ())
  '(:let 2 (:set-var (0 1) (:const . 1))
         (:set-var (0 2) (:const . 2))
         (:call cons (:cvar 0 1) (:cvar 0 2))))

(deftest (lisp2li let)
  (lisp2li '(let ((x 1) (y 2))
              (cons x y)
              (list x y)) ())
  '(:let 2 (:set-var (0 1) (:const . 1))
         (:set-var (0 2) (:const . 2))
         (:progn
          (:call cons (:cvar 0 1) (:cvar 0 2))
          (:call list (:cvar 0 1) (:cvar 0 2)))))

(deftest (lisp2li let)
  (lisp2li '(let ((x z) (y 2))
              (cons x y)) '((z 0 1)))
  '(:let 2 (:set-var (0 1) (:cvar 1 1))
         (:set-var (0 2) (:const . 2))
         (:call cons (:cvar 0 1) (:cvar 0 2))))

(deftest (lisp2li let)
  (lisp2li '(let ((x 2))
              (cons x z)) '((z 0 1)))
  '(:let 1 (:set-var (0 1) (:const . 2))
         (:call cons (:cvar 0 1) (:cvar 1 1))))

