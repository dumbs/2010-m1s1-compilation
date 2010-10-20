;; todo : utiliser copy-seq à la place.
(defun copytree (l)
  (if (atom l)
      l
    (cons (copytree (car l))
          (copytree (cdr l)))))
(load "environnement")
(load "VM/instruction")
;; ...
(run-test t)
;(print-env-stack exemple-env-stack)
