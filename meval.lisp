;; meval donnee en cours

(defun meval (expr env)
  (cond ((and (atom expr) (constantp expr)) expr) ;; Literal
        ((atom expr) ;; symboles
         (let ((cell (assoc expr env)))
           (if cell (cdr cell)
             (error ""))))
        ;; .
        ;; .
        ;; .
        ((eq 'quote (car expr)) (cadr expr)) ;;Quote (' ou quote)
        ((and (consp (car expr)) (eq 'lambda (caar expr)))
         (meval-lambda (car expr) (cdr expr) env env)) ;; TODO : a remplir
        ((eq 'defun (car expr))
         (set-defun (cadr expr) (cons 'lambda (cddr expr))) ;; TODO : sera remplacer par add-top-level-binding
         (get-defun (car expr))
         (meval-lambda (get-defun (car expr)) (cdr expr) env ()))
        ((eq 'if (car expr))
         (if (meval (second expr) env)
             (meval (third expr) env)
           (meval (fourth expr) env)))
        ;;cas des marcros/forme speciale deja traiter
        ((fboundp (car expr)) ;;denier cas vrais fonctin globale predefinie
         (apply (car expr) (map-meval (cdr expr) env))
        )
  ))

(defun map-meval (list env)
  (mapcar (lambda (x) (meval x env)) list))

(defun meval-lambda (lbd args env-args old-env)
 (meval (third (car lbd))
        (make-env (second (car lbd))
                  (map-meval args env-args)
                  old-env))
)