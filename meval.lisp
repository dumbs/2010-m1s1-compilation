(setq *debug* nil)
(load "match")

(defun get-env-num (num env)
  (format *debug* "~&get-env-num ~&~T=> num = ~a ~&~T=> env = ~a" num env)
  (defun get-env-num-t (num env counter)
    (format *debug* "~&get-env-num-t ~&~T=> num = ~a ~&~T=> env = ~a ~&~T=> counter = ~a" num env counter)
    (cond ((= counter num) env)
          ((eq (aref env 0) nil) nil)
          (T
           (get-env-num-t num (aref env 0) (+ 1 counter))
           )))
  (get-env-num-t num env 0))

(defun get-lower-env (env)
  (format *debug* "~&get-lower-env ~&~T=> env = ~a" env)
  (if (or (= (array-total-size env) 0)
          (eq (aref env 0) nil))
      env
     (get-lower-env (aref env 0))))

(defun make-env (size list-values env)
  "Construit un nouvel environnement de taille <size> dans <env>
et remplie ce nouvelle environnement avec les valeurs contenu dans
<list-values>"
  (format *debug* "~&make-env ~&~T=> size = ~a ~&~T=> list-value = ~a ~&~T=> env = ~a" size list-values env)
  (if (= (array-total-size env) 0)
      (setf env (make-array (+ 1 size)))
    (setf (aref (get-lower-env env) 0) (make-array (+ 1 size))))
  (let ((lower-env (get-lower-env env)))
    (format *debug* "~&(make-env let) ~&~T=> lower-env = ~a" lower-env)
      (loop
       for value in list-values
       for rank = 1 then (+ rank 1)
       do (setf (aref lower-env rank) value)
       ))
  env)

(defun map-meval (list env)
  (format *debug* "~&map-meval ~&~T=> list = ~a ~&~T=> env = ~a" list env)
  (mapcar (lambda (x) (meval x env)) list))

(defun meval-progn (list env)
  "Mevalue toutes les sous expressions et renvoie
la valeur de la dernier"
  (format *debug* "~&meval-progn ~&~T=> list = ~a ~&~T env = ~a" list env)
  (if (endp list)
      nil
    (if (endp (cdr list))
        (meval (car list) env)
      (progn
        (meval (car list) env)
        (meval-progn (cdr list) env)))))

(defun modify-lower-env (lower-env value pos)
  (format *debug* "~&modify-lower-env ~&~T=> lower-env = ~a ~&~T=> value = ~a ~&~T=> pos = ~a" lower-env value pos)
  (let ((env-bis (make-array (+ pos 1))))
    (defun construct-new-lower-env (new-env old-env)
      (format *debug* "~&construct-new-lower-env ~&~T=> new-env = ~a ~&~T=> old-env = ~a" new-env old-env)
      (loop
       for i = 0 then (+ i 1)
       do (setf (aref new-env i) (aref old-env i))
       while (<= i (- pos 1))
       ))
    (setf (aref lower-env pos) value)
    (construct-new-lower-env env-bis lower-env)
    (format *debug* "~&modify-lower-env ~&~T env-bis = ~a" env-bis)
    (setf lower-env env-bis)
  ))

(defun make-rest (env &optional (pos-rest 1))
  (format *debug* "~&make-rest ~&~T=> env = ~a ~&~T=> pos-rest = ~a" env pos-rest)
  (let* ((lower-env (get-lower-env env))
         (size (- (if (= 0 (array-total-size lower-env))
                      1
                    (array-total-size lower-env))
                    1)))
    (defun make-rest-lower-env (lower-env pos)
      (format *debug* "~&make-rest-lower-env ~&~T=> lower-env = ~a ~&~T=> pos = ~a ~&~T=> size = ~a" lower-env pos size)
      (cond ((>= pos size)
             (cons (aref lower-env pos) nil))
            ((< pos pos-rest)
             (make-rest-lower-env lower-env (+ pos 1)))
            (T
             (cons (aref lower-env pos)
                   (make-rest-lower-env lower-env (+ pos 1))))))
    (modify-lower-env (get-lower-env env) (make-rest-lower-env (get-lower-env env) pos-rest) pos-rest)
    (format *debug* "~&make-rest ~&~T=> lower-env = ~a" (get-lower-env env)))
  env)

(defun meval (expr &optional (env #()))
  "Interprète le langage intermédiaire passé en paramètre."
  (format *debug* "~&meval ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
  (cond ((match :const (first expr))
         (format *debug* "~&(meval :const) ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
         (cdr expr))
        ((match :cvar (first expr))
         (format *debug* "~&(meval :cvar)  ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
         (let ((sub-env (get-env-num (second expr) env)))
           (if sub-env
               (aref sub-env (third expr))
             (error "The variable ~S is unbound" expr))))
        ((match :if (first expr))
         (format *debug* "~&(meval :if)  ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
         (if (meval (second expr) env)
             (meval (third expr) env)
           (meval (fourth expr) env)))
        ((match :call (first expr))
         (format *debug* "~&(meval :call)  ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
         (apply (symbol-function (cadr expr)) (map-meval (cddr expr) env)))
        ((match :mcall (first expr))
         (format *debug* "~&(meval :mcall)  ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
         (if (consp (second expr))
             (let ((closure (second expr)))
               (format *debug* "~&~T=> closure = ~a" closure)
               (cond ((and (atom (third closure))
                           (constantp (third closure))
                           (integerp (third closure)))
                      (meval closure
                             (make-rest (make-env (length (cddr expr))
                                                  (map-meval (cddr expr) env)
                                                  env)
                                        (caddr closure))))
                     (T
                      (cond ((< (second closure) (length (cddr expr)))
                             (error "Too arguments"))
                            ((> (second closure) (length (cddr expr)))
                             (error "Too few arguments"))
                            (T
                             (meval closure
                                    (make-env (second closure)
                                              (map-meval (cddr expr)env)
                                              env)))))))
           (error "form not yet implemented")))
        ((match :progn (first expr))
         (format *debug* "~&(meval :progn) ~&~T=> expr = ~a ~&~T=> env = ~a" expr env)
         (meval-progn (cdr expr) env))
        ((match :lclosure (first expr))
         (format *debug* "~&(meval :lclosure) ~&~T=>  expr = ~a~&~T=>  env = ~a" expr env)
         (if (and (atom (caddr expr))
                  (constantp (caddr expr))
                  (integerp (caddr expr)))
             (meval-progn (cdddr expr) env)
           (meval-progn `(,(cddr expr)) env)))
        (T
         (error "form special ~S not yet implemented" (car expr)))))

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

(deftest (meval :mcall :lclosure)
  (meval (lisp2li '((lambda (x y) (cons x y)) 1 2) ()))
  '(1 . 2))

(deftest (meval :mcall :lclosure)
  (meval (lisp2li '((lambda (x &rest y) (cons x y)) 1 2 3 4) ()))
  '(1 2 3 4))

(deftest (meval defun)
  (meval '(defun foo (x) x))
  foo)

(deftest (meval defun)
  (meval '(defun foo (x y z) (list x y z)))
  foo)

