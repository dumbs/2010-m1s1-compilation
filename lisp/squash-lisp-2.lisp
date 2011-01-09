(require 'match "match")
(require 'util "match") ;; derived-symbol, assoc-or, assoc-or-push

(defun squash-lisp-2 (expr &optional env-var env-fun (globals (cons nil nil)))
  "Transforme les let, let*, flet, labels, lambda en simple-let et simple-lambda,
   détecte les variables globales et stocke leurs noms dans une liste,
   et rend tous les noms de fonction et de variables _locales_ uniques."
  (cond-match
   expr
   ((progn :body _*)
    `(progn ,@(mapcar (lambda (x) (squash-lisp-2 x env-var env-fun globals)) body)))
   ((if :condition _ :si-vrai _ :si-faux _)
    `(if ,(squash-lisp-2 condition env-var env-fun globals)
         ,(squash-lisp-2 si-vrai   env-var env-fun globals)
         ,(squash-lisp-2 si-faux   env-var env-fun globals)))
   ((unwind-protect :body _ :cleanup _)
    `(unwind-protect ,(squash-lisp-2 body env-var env-fun globals)
       ,(squash-lisp-2 cleanup env-var env-fun globals)))
   ((:type (? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body _ :catch-code _)
    `(,type ,(squash-lisp-2 object env-var env-fun globals)
            ,(squash-lisp-2 body env-var env-fun globals)
            ,(squash-lisp-2 catch-code env-var env-fun globals)))
   ((unwind :object _)
    `(unwind ,(squash-lisp-2 object env-var env-fun globals)))
   ((unwind-for-tagbody :object _ :post-unwind-code _)
    `(unwind-for-tagbody ,(squash-lisp-2 object env-var env-fun globals)
                         ,(squash-lisp-2 post-unwind-code env-var env-fun globals)))
   ((jump-label :name $$)
    expr)
   ((jump :dest $$)
    expr)
   ((let ((:name $$ :value _)*) :body _)
    (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
    (let ((new-env-var (append name env-var)))
      `(simple-let ,(mapcar #'cdr name)
                   (progn ,@(mapcar (lambda (n v)
                                      `(setq ,(cdr n) ,(squash-lisp-2 v env-var env-fun globals)))
                                    name value)
                          ,(squash-lisp-2 body new-env-var env-fun globals)))))
   (((? (eq x 'let*)) ((:name $$ :value _)*) :body _)
    (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
    (let ((new-env-var env-var)) ;; old = ((new-env-var (append name env-var)))
      `(simple-let ,(mapcar #'cdr name)
                   (progn ,@(mapcar (lambda (n v)
                                      (push (cons n v) new-env-var) ;; Ajouté
                                      `(setq ,(cdr n) ,(squash-lisp-2 v new-env-var env-fun globals))) ;; env-var -> new-env-var !!!
                                    name value)
                          ,(squash-lisp-2 body new-env-var env-fun globals)))))
   ((simple-flet ((:name $$ :value _)*) :body _)
    (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
    (let ((new-env-fun (append name env-fun))) ;; new-env-var -> new-env-fun   +   env-var -> env-fun
      `(simple-let ,(mapcar #'cdr name)
                   (progn ,@(mapcar (lambda (n v)
                                      `(setq ,(cdr n) ,(squash-lisp-2 v env-var env-fun globals)))
                                    name value)
                          ,(squash-lisp-2 body env-var new-env-fun globals))))) ;; env-var -> env-fun
   ((simple-labels ((:name $$ :value _)*) :body _)
    (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
    (let ((new-env-fun (append name env-fun))) ;; new-env-var -> new-env-fun   +   env-var -> env-fun
      `(simple-let ,(mapcar #'cdr name)
                   (progn ,@(mapcar (lambda (n v)
                                      `(setq ,(cdr n) ,(squash-lisp-2 v env-var new-env-fun globals))) ;; env-fun -> new-env-fun
                                    name value)
                          ,(squash-lisp-2 body env-var new-env-fun globals))))) ;; env-var -> env-fun
   ;; TODO
   ((lambda :params @ :body _)
    ;; TODO : simplifier la lambda-list
    (squash-lisp-1-check body))
   ;; TODO
   ((function :fun $$)
    (assoc-or fun env-fun
              (assoc-or-push fun (derived-symbol (string fun)) (cdr globals))))
   ((funcall :fun _ :params _*)
    `(funcall ,(squash-lisp-2 fun env-var env-fun globals)
              ,@(mapcar (lambda (x) (squash-lisp-2 x env-var env-fun globals)) params)))
   ((quote _)
    expr)
   ((get-var :var $$)
    (assoc-or var env-var
              (assoc-or-push var (derived-symbol var) (car globals))))
   ((setq :name $$ :value _)
    `(setq ,(assoc-or name env-var
                      (assoc-or-push name (derived-symbol name) (car globals)))
           ,(squash-lisp-2 value env-var env-fun globals)))
   (_
    (error "squash-lisp-2: Assertion failed ! This should not be here : ~a" expr))))

(squash-lisp-2 '(let ((x (quote 123))) (let* ((x (quote 1)) (y (get-var x))) (funcall (function +) (get-var x) (get-var y) (quote 1)))))

(provide 'squash-lisp-2)