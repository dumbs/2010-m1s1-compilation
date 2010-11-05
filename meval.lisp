(defun meval (expr env)
  "Interprète le langage intermédiaire passé en paramètre."
  (cond ((eq ':lit (first expr))
         (cdr expr))
        ((eq ':var (first expr))
         (let ((cell (get-binding env (cdr expr))))
           (if cell
                (cdr cell)
               (error "The variable ~S is unbound" (cdr expr)))))
        ((eq ':if (car expr))
         (if (meval (second expr) env)
             (meval (third expr) env)
           (meval (fourth expr) env)))
        ((eq ':call (first expr))
         (apply (second expr) (map-meval (cddr expr) env)))
        ))

(defun map-meval (list env)
  (mapcar (lambda (x) (meval x env)) list))

;; Test unitaire
(load "test-unitaire")
(erase-tests meval)
(deftest meval
  (meval '(:lit . 3) ())
  3)

(deftest meval
  (meval '(:var . x) '(("TEST" (s . ()) (z . 4) (x . 5) (u . 6))))
  5)

(deftest meval
  (meval '(:var . s2) '(("TEST" (s . ()) (s1 . 7) (s2 . 8))
                       ("TOP-LEVEL" (x . 4) (x1 . 5) (x2 . 6))))
  8)

(deftest meval
  (meval '(:call + (:lit . 3) (:var . x)) '(("TOP-LEVEL" (x1 . 4) (x . 5) (z . 6))))
  8)
