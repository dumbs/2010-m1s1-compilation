(load "match")

(defun get-env-num (num env)
  (defun get-env-num-t (num env counter)
    (cond ((= counter num) env)
          ((eq (aref env 0) nil) nil)
          (T
           (get-env-num-t num (aref env 0) (+ 1 counter))
           )))
  (get-env-num-t num env 0))

(defun map-meval (list env)
  (mapcar (lambda (x) (meval x env)) list))

(defun meval-progn (list env)
  (loop
   for expr in list
   do (meval expr env)
  ))

(defun meval (expr &optional env)
  "Interprète le langage intermédiaire passé en paramètre."
  (cond ((match :const (first expr))
         (cdr expr))
        ((match :cvar (first expr))
         (let ((sub-env (get-env-num (second expr) env)))
           (if sub-env
               (aref sub-env (third expr))
             (error "The variable ~S is unbound" expr))))
        ((match :if (first expr))
         (if (meval (second expr) env)
             (meval (third expr) env)
           (meval (fourth expr) env)))
        ((match :call (first expr))
         (apply (symbol-function (cadr expr)) (map-meval (cddr expr) env)))
        ((match :progn (first expr))
         (map-meval ())
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

(deftest (meval defun)
  (meval '(defun foo (x) x))
  foo)

(deftest (meval defun)
  (meval '(defun foo (x y z) (list x y z)))
  foo)

