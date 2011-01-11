(require 'mini-meval "mini-meval") ;; slice-up-lambda-list
(require 'match "match")
(require 'util "match") ;; derived-symbol, assoc-or, assoc-or-push

(defun squash-lisp-2 (expr &optional env-var env-fun (globals (cons nil nil)))
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
   ((super-let :name ($$*) :stuff _*)
    (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
    (labels ((transform-super-let (expr)
               `(progn
                  ,@(loop
                       for (type . clause) in stuff
                       when (eq type 'set)
                       collect `(setq ,(cdr (assoc (car clause) name)) (squash-lisp-2 (cadr clause) env-var env-fun globals))
                       when (eq type 'use-var)
                       do (push (assoc (car clause) name) env-var)
                       when (eq type 'use-fun)
                       do (push (assoc (car clause) name) env-fun)
                       when (eq type 'if)
                       do `(if ,(squash-lisp-2 (car clause) env-var env-fun globals)
                               (progn ,(mapcar #'transform-super-let (cadr clause)))
                               (progn ,(mapcar #'transform-super-let (caddr clause))))
                       when (eq type 'progn)
                       collect `(progn ,(mapcar (lambda (x) (squash-lisp-2 x env-var env-fun globals)) clause))))))
      `(let ,(mapcar #'cdr name)
         ,(transform-super-let expr))))
   ((let ((:name $$ :value _)*) :body _)
    (squash-lisp-2
     `(super-let ,name
                 ,@(mapcar (lambda (n v) `(set ,n ,v)) name value)
                 ,@(mapcar (lambda (n) `(use-var ,n)) name)
                 (progn ,body))
     env-var env-fun globals))
   (((? (eq x 'let*)) ((:name $$ :value _)*) :body _)
    (squash-lisp-2
     `(super-let ,name
                 ,@(loop
                      for n in name
                      for v in value
                      collect `(set ,n ,v)
                      collect `(use-var ,n))
                 (progn ,body))
     env-var env-fun globals))
   ((simple-flet ((:name $$ :value _)*) :body _)
    (squash-lisp-2
     `(super-let ,name
                 ,@(mapcar (lambda (n v) `(set ,n ,v)) name value)
                 ,@(mapcar (lambda (n) `(use-fun ,n)) name)
                 (progn ,body))
     env-var env-fun globals))
   ((simple-labels ((:name $$ :value _)*) :body _)
    (squash-lisp-2
     `(super-let ,name
                 ,@(mapcar (lambda (n) `(use-fun ,n)) name)
                 ,@(mapcar (lambda (n v) `(set ,n ,v)) name value)
                 (progn ,body))
     env-var env-fun globals))
   ;; ((let ((:name $$ :value _)*) :body _)
   ;;  (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
   ;;  (let ((new-env-var (append name env-var)))
   ;;    `(let ,(mapcar #'cdr name)
   ;;       (progn ,@(mapcar (lambda (n v)
   ;;                          `(setq ,(cdr n) ,(squash-lisp-2 v env-var env-fun globals)))
   ;;                        name value)
   ;;              ,(squash-lisp-2 body new-env-var env-fun globals)))))
   ;; (((? (eq x 'let*)) ((:name $$ :value _)*) :body _)
   ;;  (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
   ;;  (let ((new-env-var env-var)) ;; old = ((new-env-var (append name env-var)))
   ;;    `(let ,(mapcar #'cdr name)
   ;;       (progn ,@(mapcar (lambda (n v)
   ;;                          (push (cons n v) new-env-var) ;; Ajouté
   ;;                          `(setq ,(cdr n) ,(squash-lisp-2 v new-env-var env-fun globals))) ;; env-var -> new-env-var !!!
   ;;                        name value)
   ;;              ,(squash-lisp-2 body new-env-var env-fun globals)))))
   ;; ((simple-flet ((:name $$ :value _)*) :body _)
   ;;  (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
   ;;  (let ((new-env-fun (append name env-fun))) ;; new-env-var -> new-env-fun   +   env-var -> env-fun
   ;;    `(let ,(mapcar #'cdr name)
   ;;       (progn ,@(mapcar (lambda (n v)
   ;;                          `(setq ,(cdr n) ,(squash-lisp-2 v env-var env-fun globals)))
   ;;                        name value)
   ;;              ,(squash-lisp-2 body env-var new-env-fun globals))))) ;; env-var -> env-fun
   ;; ((simple-labels ((:name $$ :value _)*) :body _)
   ;;  (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
   ;;  (let ((new-env-fun (append name env-fun))) ;; new-env-var -> new-env-fun   +   env-var -> env-fun
   ;;    `(let ,(mapcar #'cdr name)
   ;;       (progn ,@(mapcar (lambda (n v)
   ;;                          `(setq ,(cdr n) ,(squash-lisp-2 v env-var new-env-fun globals))) ;; env-fun -> new-env-fun
   ;;                        name value)
   ;;              ,(squash-lisp-2 body env-var new-env-fun globals))))) ;; env-var -> env-fun
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

;; TODO : test uniraire
;; (squash-lisp-2 '(let ((x (quote 123))) (let* ((x (quote 1)) (y (get-var x))) (funcall (function +) (get-var x) (get-var y) (quote 1)))))


(defvar *ll* '(a b &optional u &rest c &key ((:foo bar)) :quux (:baz 'glop) &aux (x 1) (y (+ x 2))))

;; TODO : faire cette transformation dans squash-lisp-1
;; TODO : faire la transformation des let/let*/flet/labels en super-let dans squash-lisp-1
;; TODO : raison : transformer les appels de fonction en funcall, etc.


;; nil à la fin
                                
(slice-up-lambda-list '(a b &optional (u 3 v) &rest c &key ((:foo bar)) :quux (:baz 'glop) &allow-other-keys &aux (x 1) (y (+ x 2))))

(let (a b u v c foo quux baz x y)
  (set a (car ,whole-sym))
  (use a)
  (progn (setq ,whole-sym (cdr ,whole-sym)))
  (set b (car ,whole-sym))
  (use b)
  (progn (setq ,whole-sym (cdr ,whole-sym)))
  (set u (if ,whole-sym (car whole-sym)) ...)
  (set v (if ,whole-sym t nil))
  (use u)
  (use v)
  (progn (setq ,whole-sym (cdr ,whole-sym))))

((FIXED A B) (OPTIONAL (U NIL NIL)) (REST C) (REST2)
    (KEY (FOO BAR NIL NIL) (QUUX QUUX NIL NIL) (BAZ BAZ 'GLOP NIL)) (OTHER)
    (AUX (X 1) (Y (+ X 2))) (REJECT))

(provide 'squash-lisp-2)