(load "match")
(defun meval (expr &optional env)
  "Interprète le langage intermédiaire passé en paramètre."
  (cond ((match :const (first expr))
         (cdr expr))
        ((match :cvar (first expr))
         )
        ((match :lclosure (first expr))
         )
        (T
         (error "form special ~S not yet implemented" expr))))

;;   (cond ((eq ':const (first expr))
;;          (cdr expr))
;;         ((eq ':var (first expr))
;;          (let ((cell (get-binding env (cdr expr))))
;;            (if cell
;;                 (cdr cell)
;;                (error "The variable ~S is unbound" (cdr expr)))))
;;         ((eq ':if (car expr))
;;          (if (meval (second expr) env)
;;              (meval (third expr) env)
;;            (meval (fourth expr) env)))
;;         ((eq ':call (first expr))
;;          (apply (second expr) (map-meval (cddr expr) env)))
;;         ))

(defun map-meval (list env)
  (mapcar (lambda (x) (meval x env)) list))

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
  '3)

(deftest (meval :cvar)
  (meval (lisp2li 'x '((x 0 2))) #(() 4 5 6))
  5)

(deftest (meval :cvar)
  (meval '(:cvar 1 2) #(#(() 7 8) 4 5 6)) 
  8)

(deftest (meval :call)
  (meval '(:call + (:const . 3) (:cvar 0 1)) #(() 4 5 6))
  8)

(deftest (meval defun)
  (meval '(defun foo (x) x))
  foo)