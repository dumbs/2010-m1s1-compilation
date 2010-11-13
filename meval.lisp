(load "match")

(defun env-size (env)
  (if (or (equalp env #()) (eq env nil))
      0
    (+ 1 (env-size (aref env 0)))))

(defun get-env-num (num env)
  "Récupère l’environnement correspondant à celui souhaité."
  (defun get-env-num-r (num env counter)
    (cond ((or (equalp env #()) (eq env nil))
           env)
          ((= num counter)
           env)
          (T
           (get-env-num-t num (aref env 0) (- counter 1)))))
  (get-env-num-r num env (- (env-size env) 1)))

(defun current-env (env)
  (let ((env-size (- (env-size env) 1)))
    (defun current-env-r (env counter)
      (if (= counter env-size)
          env
        (current-env-r (aref env 0) (+ counter 1))))
    (current-env-r env 0)))
                          
(defun get-lower-env (env)
  "Récupère l’environnement le plus bas"
  (if (or (= (array-total-size env) 0)
          (eq (aref env 0) nil))
      env
     (get-lower-env (aref env 0))))

(defun make-rest (env values &optional (pos-rest 1))
  "Construit l'environnement en rajoutant tous les valeurs
du &rest dans une cellule de l'env sous forme d'une liste"
  (let ((size (- (array-total-size env) 1)))
    (defun make-rest-lower-env (lower-env pos values)
      (cond ((= pos pos-rest)
             (setf (aref lower-env pos) values))
            (T
             (setf (aref lower-env pos) (car values))
             (make-rest-lower-env lower-env
                                  (+ pos 1)
                                  (cdr values)))))
    (make-rest-lower-env env 1 values))
  env)

(defun make-env (size list-values env &optional pos-rest)
  "Construis l’environnement en appariant les paramètres aux valeurs
 correspondantes et signale une exception si paramètres et arguments
 ne concordent pas. Si l’environnement passe en paramètre n’est pas vide,
 le nouvel environnement y est inclus."
  (let ((new-env (copy-all env)))
    (cond ((and (not pos-rest)
                (< size (length list-values)))
           (error "Too arguments"))
          ((> size (length list-values))
           (error "Too few arguments"))
          (T
           (if (= (array-total-size new-env) 0)
               (setf new-env (make-array (+ 1 size)))
             (setf (aref (get-lower-env new-env) 0) (make-array (+ 1 size))))
           (let ((lower-env (get-lower-env new-env)))
             (if pos-rest
                 (make-rest lower-env
                            list-values
                            pos-rest)
               (loop
                for value in list-values
                for rank = 1 then (+ rank 1)
                do (setf (aref lower-env rank) value)
                )))
           new-env))))

(defun map-meval (list env)
  (mapcar (lambda (x) (meval x env)) list))

(defun meval-body (list-expr env)
  "Évalue en séquence la liste des expressions et
retourne la valeur retournée par la dernière"
  (if (endp list-expr)
      nil
    (if (endp (cdr list-expr))
        (meval (car list-expr) env)
      (progn
        (meval (car list-expr) env)
        (meval-body (cdr list-expr) env)))))

(defun meval-args (list-expr env)
  "Évalue en séquence la liste des expressions et
retourne la liste de leurs valeurs"
  (if (endp list-expr)
      nil
    (if (endp (cdr list-expr))
        `(,(meval (car list-expr) env))
      `(,(meval (car list-expr) env)
        ,@(meval-args (cdr list-expr) env)))))

(defun meval-lambda (lclosure args env)
  "Applique une λ-fonction quelconque à des valeurs
d’arguments dans un certain environnement."
  (match (:nil :lclosure :size (? integerp) :rest (? integerp)? :body _*) lclosure
         (meval lclosure
                (make-env size args env rest))))

(defun msetf (place val env)
  (let ((sub-env (get-env-num (first place) env)))
    (if sub-env
        (setf (aref sub-env (second place))
              (meval val env)))))

(defun make-empty-list (size)
  (if (= size 0)
      nil
    (cons nil (make-empty-list (- size 1)))))

(defun meval (expr &optional (env #()))
  "Interprète le langage intermédiaire passé en paramètre."
  (cond-match expr
              ((:nil :const :val . _) expr val)
              ((:nil :cvar :num-env (? integerp) :index (? integerp))
               (if (= num-env 0)
                   (aref (current-env env) index)
                 (let ((sub-env (get-env-num num-env env)))
                   (if sub-env
                       (aref sub-env index)
                     (error "The variable unbound : ~w" expr)))))
              ((:nil :if :predicat @. :expr1 @. :expr2 @.)
               (if (meval predicat env)
                   (meval expr1 env)
                 (meval expr2 env)))
              ((:nil :mcall set-defun :func-name @. :closure _*)
               (let ((name (meval func-name env)))
                 (setf (get name :defun) closure)
                 name))
              ((:nil :mcall set-defmacro :macro-name @. :closure _*)
               (let ((name (meval macro-name env)))
                 (setf (get name :defmacro) closure)
                 name))
              ((:nil :mcall :func-name (? (get x :defun)) :params _*)
               (let ((values (meval-args params env)))
                 (meval-lambda (car (get func-name :defun))
                               values
                               (make-env (length values) values env))))
              ((:nil :mcall :macro-name (? (get x :defmacro)) :params _*)
               (let ((values (meval-args params env)))
                 (meval (lisp2li (meval-lambda (car (get macro-name :defmacro))
                               params
                               (make-env (length values) values env))
                                 env)
                        env)))
              ((:nil :mcall :lambda (:nil :lclosure (? integerp) (? integerp)? _*) :args _*)
               (meval-lambda lambda (meval-args args env) env))
              ((:nil :call :func-name _ :body _*)
               (apply (symbol-function func-name) (meval-args body env)))
              ((:nil :progn :body @.+)
               (meval-body body env))
              ((:nil :lclosure :size (? integerp) :rest (? integerp)? :body _*)
               (meval-body `(,body) env))
              ((:nil :set-var :place @. :value _)
               (msetf place value env))
              ((:nil :let :size (? integerp) :affectations (:nil :set-var :places @ :values _)* :body _*)
               (meval-body body (make-env size (meval-args values env) env)))
              ((:nil :unknown :call (:name $ :params _*) :environ _*)
               (lisp2li call environ))
              (_*
               (error "form special ~S not yet implemented" expr))))

;; Test unitaire
(load "test-unitaire")
(load "lisp2li")
(erase-tests meval)
(deftest (meval :const)
  (meval (lisp2li 3 ()))
  3)

(deftest (meval quote)
  (meval (lisp2li '3 ()))
  3)

(deftest (meval quote)
  (meval (lisp2li ''3 ()))
  3)

(deftest (meval quote)
  (meval (lisp2li '''3 ()))
  ''3)

(deftest (meval :cvar)
  (meval (lisp2li 'x '((x 0 2))) #(() 4 5 6))
  5)

(deftest (meval :cvar)
  (meval '(:cvar 1 2) #(#(() 7 8) 4 5 6)) 
  8)

(deftest (meval :call)
  (meval '(:call + (:const . 3) (:cvar 0 1)) #(() 4 5 6))
  7)

(deftest (meval :call)
  (meval '(:call list (:const . 3) (:const . 2)))
  '(3 2))

(deftest (meval :if)
  (meval '(:if (:const . T)
               (:const . T)
               (:const . nil)))
  T)

(deftest (meval :if)
  (meval '(:if (:call eq (:const . 1)
                      (:cvar 0 1))
               (:const . T)
               (:const . nil)) #(() 1 2 3))
  T)

(deftestvar (meval make-env) empty-env #())
(deftest (meval make-env)
  (make-env 2 '(1 2) empty-env)
  #(() 1 2)
  #'equalp)

(deftestvar (meval make-env) env #(() 1 2))
(deftest (meval make-env)
  (make-env 2 '(7 8) env)
  #(#(() 7 8) 1 2)
  #'equalp)

(deftestvar (meval make-env make-rest) env #(() nil nil))
(deftest (meval make-env make-rest)
  (make-rest env '(1 2 3 4) 2)
  #(() 1 (2 3 4))
  #'equalp)

(deftestvar (meval make-env &rest) env #(() 1 2))
(deftest (meval make-env &rest)
  (make-env 2 '(7 8 9) env 2)
  #(#(() 7 (8 9)) 1 2)
  #'equalp)

(deftest (meval make-env &rest)
  (make-env 1 '(nil) env 1)
  #(#(() (nil)) 1 2)
  #'equalp)

(deftest (meval meval-body)
  (meval-body '((:const . 3)) #())
  '3)

(deftest (meval meval-body)
  (meval-body '((:const . 3) (:call cons (:const . 1) (:const . 2))) #())
  '(1 . 2))

(deftest (meval meval-args)
  (meval-args '((:const . 3)) #())
  '(3))

(deftest (meval meval-args)
  (meval-args '((:const . 3) (:const 1 2 3)) #())
  '(3 (1 2 3)))

(deftest (meval meval-args)
  (meval-args '((:cvar 0 1) (:call cons (:cvar 0 3)
                                  (:cvar 0 2))) #(() 1 2 3))
  '(1 (3 . 2)))

(deftest (meval meval-lambda)
  (meval-lambda '(:lclosure 2 :call cons
                            (:cvar 0 1)
                            (:cvar 0 2))
                '(1 2) #())
  '(1 . 2))

(deftest (meval :mcall :lclosure)
  (meval (lisp2li '((lambda (x y) (cons x y)) 1 2) ()))
  '(1 . 2))

(deftest (meval :mcall :lclosure)
  (meval (lisp2li '((lambda (x &rest y) (cons x y)) 1 2 3 4) ()))
  '(1 2 3 4))

(deftestvar (meval :set-var) env #(() 2))
(deftest (meval :set-var)
  (progn
    (meval (lisp2li '(setf x 42) '((x 0 1))) env)
    env)
  #(() 42)
  #'equalp)
