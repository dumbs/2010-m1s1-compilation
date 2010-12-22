(require 'match "match")

;; TODO : util : mapnth

(defun squash-lisp-2 (expr)
  "Transforme les let, let*, flet, labels, lambda en simple-let et simple-lambda,
   détecte les variables globales et stocke leurs noms dans une liste,
   et rend tous les noms de fonction et de variables _locales_ uniques."
  (cond-match
   expr
   ((progn :body _*)
    `(progn ,@(mapcar squash-lisp-2 body)))
   ((unwind-protect :body _ :cleanup _)
    (and (squash-lisp-1-check body)
         (squash-lisp-1-check cleanup)))
   ((unwind-catch :object _ :body _ :catch-code _)
    (and (squash-lisp-1-check object)
         (squash-lisp-1-check body)
         (squash-lisp-1-check catch-code)))
   ((unwind :object _)
    (squash-lisp-1-check object))
   ((half-unwind :object _ :post-unwind-code _)
    (and (squash-lisp-1-check object)
         (squash-lisp-1-check post-unwind-code)))
   ((jump-label :name _) ;; TODO : être plus précis que "_"
    t)
   ((jump :dest _) ;; TODO : être plus précis que "_"
    t)
   (((? (member x '(let let* flet labels))) ((:name $$ :value _)*) :body _)
    (every #'squash-lisp-1-check (cons body value)))
   ((lambda :params ($$*) :body _)
    (squash-lisp-1-check body))
   ((function :fun $$)
    t)
   ((funcall :fun _ :params _*)
    (every #'squash-lisp-1-check (cons fun params)))
   ((quote _)
    t)
   ((get-var $$)
    t)
   (_
    (error "squash-lisp-2: Assertion failed ! This should not be here : ~a" expr))))


(provide 'squash-lisp-2)