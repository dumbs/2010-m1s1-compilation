;; todo : utiliser copy-seq à la place.
(defun copytree (l)
  (if (atom l)
      l
    (cons (copytree (car l))
          (copytree (cdr l)))))
(load "environnement")
(load "instructions")
;; ...
(run-test t)
;(print-env-stack exemple-env-stack)
