(require 'match "match")

(defun squash-lisp-3-internal (expr globals &optional (local-env (cons nil nil)) (getset (cons nil nil)) (top-level (cons nil nil)))
  "Lorsqu'une variable à l'intérieur d'une `lambda' référence une déclaration à l'extérieur de la `lambda', on la marque comme étant *capturée*.
   
   On fusionne tous les `let' d'une `lambda' en les remontant dans un `let' unique à la racine de la `lamdba'.

   [Abandonné, fait dans la compilation] On fusionne tous les `tagbody' d'une `lambda' en les remontant dans un `tagbody' unique à la
   racine de la `lambda' ? + transformation des if en tagbody.
   
   On sort toutes les lambdas (fonctions anonymes), on les nomme avec un symbole unique, et on les met au top-level.
   
   local-env : car = variables locales, cdr = variables capturées."
  (macrolet ((transform (expr &optional (local-env 'local-env)) `(squash-lisp-3-internal ,expr globals ,local-env getset top-level)))
    (cond-match
     expr
     ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
     ((:type (? (member x '(progn simple-tagbody))) :body _*)
      (let ((res (list 'progn))
            (is-tagbody (eq type 'simple-tagbody)))
        (labels ((squash-progn (body)
                   (dolist (e body)
                     (if (and (consp e) (eq 'progn (car e)))
                         (squash-progn (cdr e))
                         (if (and (consp e) (eq 'simple-tagbody (car e)))
                             (progn (setq is-tagbody t)
                                    (squash-progn (cdr e)))
                             (push e res))))))
          (squash-progn (mapcar (lambda (x) (transform x)) body)))
        ;; TODO : ici : filtrer les expressions de `res' qui sont sans effet de bord, sauf la dernière.
        (setq res (reverse res))
        (setq res (append (remove ''nil (butlast res) :test #'equal) (last res)))
        (setq res (if (cdr res) ;; res != '(progn)
                      (if (cddr res) ;; res != '(progn single-expr)
                          res
                          (cadr res))
                      '(quote nil)))
        (when is-tagbody (setf (car res) 'simple-tagbody))
        res))
     ((if :condition _ :si-vrai _ :si-faux _)
      `(if ,(transform condition)
           ,(transform si-vrai)
           ,(transform si-faux)))
     ((unwind-protect :body _ :cleanup _)
      `(unwind-protect ,(transform body)
         ,(transform cleanup)))
     ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
     ((:type (? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
      `(,type ,(transform object)
              ,(transform body)
              ,(transform catch-code)))
     ((unwind :object _)
      `(unwind ,(transform object)))
     ((unwind-for-tagbody :object _ :post-unwind-code _)
      `(unwind-for-tagbody ,(transform object)
                           ,(transform post-unwind-code)))
     ((jump-label :name $$)
      expr)
     ((jump :dest $$)
      expr)
     ((let :vars ($$*) :body _)
      (setf (car local-env) (append vars (car local-env)))
      (dolist (v vars)
        (when (assoc v (car getset))
          (error "squash-lisp-3-internal : Assertion failed ! Duplicate definition of ~w" v))
        (push (cons v (cons nil nil)) (car getset)))
      (transform body))
     ((named-lambda :name $$ :params (&rest :params-sym $$) :unused _ :body (let ($$*) _*))
      (let* ((closure-sym (derived-symbol "CLOSURE"))
             (new-local-env (progn (push (cons params-sym (cons nil nil)) (car getset))
                                   (push (cons closure-sym (cons nil nil)) (car getset))
                                   (cons (list closure-sym params-sym) nil)))
             (tbody (transform body new-local-env))
             (transitive-captures nil)
             (here-captures nil))
        ;; on "nomme" la lambda, et ce nom est global
        (push name (car globals))
        ;; on transforme les get-var de variables capturées en get-captured-var
        (dolist (cap (cdr new-local-env))
          (unless (or (member cap here-captures) (member cap transitive-captures))
            (if (member cap (car new-local-env))
                (let ((gs (assoc cap (car getset))))
                  (unless gs (error "squash-lisp-3-internal : Assertion failed ! ~w is captured, but not in getset." cap))
                  (dolist (getter (cadr gs))
                    (setf (car getter) 'get-captured-var))
                  (dolist (setter (cddr gs))
                    (setf (car setter) 'set-captured-var))
                  (push cap here-captures))
                (progn
                  (push cap (cdr local-env))
                  (push cap transitive-captures)))))
        ;; on construit la lambda au top-level
        (push `(set ',name (lambda (,closure-sym &rest ,params-sym) (get-var ,closure-sym) ,unused
                                   (let (,@(remove closure-sym (remove params-sym (car new-local-env)))
                                         ,@transitive-captures)
                                     (progn
                                       ,@(mapcar (lambda (x) `(make-captured-var ,x)) here-captures)
                                       ,@(loop for x in transitive-captures
                                            collect `(setq ,x (funcall (fdefinition 'car) (get-var ,closure-sym)))
                                            collect `(setq ,closure-sym (funcall (fdefinition 'cdr) (get-var ,closure-sym))))
                                       ,tbody))))
              (car top-level))
        ;; on remplace toute la lambda par un accès à sa définition au top-level
        `(make-closure ,name ,@transitive-captures)))
     ((funcall :fun _ :params _*)
      `(funcall ,@(mapcar (lambda (x) (transform x)) (cons fun params))))
     ((quote _)
      expr)
     ((get-var :name $$)
      (if (member name (car globals))
          expr
          (let ((getter (list 'get-var name))
                (assoc (assoc name (car getset))))
            (unless (member name (car local-env))
              (pushnew name (cdr local-env)))
            (unless assoc
              (error "squash-lisp-3-internal : Assertion failed ! ~w used as local but not in getset alist." name))
            (push getter (cadr assoc))
            getter)))
     ((setq :name $$ :value _)
      (if (member name (car globals))
          expr
          (let ((setter (list 'setq name (transform value)))
                (assoc (assoc name (car getset))))
            (unless (member name (car local-env))
              (pushnew name (cdr local-env)))
            (unless assoc
              (error "squash-lisp-3-internal : Assertion failed ! ~w used as local but not in getset alist." name))
            (push setter (cddr assoc))
            setter)))
     ((fdefinition (quote $$))
      expr)
     ((symbol-value (quote $$))
      expr)
     ((set (quote :var $$) :value _)
      `(set ',var ,(transform value)))
     (_
      (error "squash-lisp-3-internal : Assertion failed ! this should not be here : ~w" expr)))))

(defun squash-lisp-3 (expr globals)
  (let* ((tl (cons nil nil))
         (lsym (make-symbol "MAIN"))
         (psym (make-symbol "NO-PARAMETERS")))
    (squash-lisp-3-internal `(named-lambda ,lsym (&rest ,psym) (get-var ,psym) (let () ,expr)) globals (cons nil nil) (cons nil nil) tl)
    `(top-level ,lsym ,globals (progn ,@(reverse (car tl))))))

(defun squash-lisp-3-check-2 (expr)
  "Vérifie si expr est bien un résultat valable de squash-lisp-1.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 1.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (cond-match
   expr
   ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
   (((? (member x '(progn simple-tagbody))) :body _*)
    (every #'squash-lisp-3-check-2 body))
   ((if :condition _ :si-vrai _ :si-faux _)
    (and (squash-lisp-3-check-2 condition)
         (squash-lisp-3-check-2 si-vrai)
         (squash-lisp-3-check-2 si-faux)))
   ((unwind-protect :body _ :cleanup _)
    (and (squash-lisp-3-check-2 body)
         (squash-lisp-3-check-2 cleanup)))
   ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
   (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
    (and (squash-lisp-3-check-2 object)
         (squash-lisp-3-check-2 body)
         (squash-lisp-3-check-2 catch-code)))
   ((unwind :object _)
    (squash-lisp-3-check-2 object))
   ((unwind-for-tagbody :object _ :post-unwind-code _)
    (and (squash-lisp-3-check-2 object)
         (squash-lisp-3-check-2 post-unwind-code)))
   ((jump-label :name $$)
    t)
   ((jump :dest $$)
    t)
   ;; ((let ($$*) :body _)
   ;;  (squash-lisp-3-check-2 body))
   ;; ((lambda (&rest $$) :unused _ :body (let ($$*) _*))
   ;;  (squash-lisp-3-check-2 body))
   ((funcall :fun _ :params _*)
    (every #'squash-lisp-3-check-2 (cons fun params)))
   ((quote _)
    t)
   ((get-var $$)
    t)
   ((setq :name $$ :value _)
    (squash-lisp-3-check-2 value))
   ((fdefinition (quote $$))
    t)
   ((symbol-value (quote $$))
    t)
   ((set (quote $$) :value _)
    (squash-lisp-3-check-2 value))
   ((make-closure $$ $$*)
    t)
   ((make-captured-var $$)
    t)
   ((get-captured-var $$)
    t)
   ((set-captured-var $$ :value _)
    (squash-lisp-3-check-2 value))
   (_
    (warn "squash-lisp-3-check-2: Assertion failed ! This should not be here : ~w" expr)
    nil)))

(defun squash-lisp-3-check-1 (expr)
  (cond-match
   expr
   ((set (quote $$) (lambda ($$ &rest $$) (get-var $$) (get-var $$) (let ($$*) :body _)))
    (squash-lisp-3-check-2 body))
   (_
    (warn "~&squash-lisp-3-check : this should not be here :~&~w" expr)
    nil)))

(defun squash-lisp-3-check (expr)
  (cond-match expr
              ((top-level $$ (($$*) . ($$*)) (progn :body _*))
               (every #'squash-lisp-3-check-1 body))
              (_ (warn "~&squash-lisp-3-check : this should not be here :~&~w" expr)
                 nil)))

(defun nice-squash-lisp-3-check (expr)
  (match (top-level $$ (($$*) . ($$*)) (progn (set '$$ (lambda ($$ &rest $$) (get-var $$) (get-var $$) (let ($$*) (? squash-lisp-3-check-2))))*))
         expr))
  
(defun squash-lisp-1+3 (expr &optional (etat (list nil nil nil)))
  (let ((globals (cons nil nil)))
    (squash-lisp-3 (squash-lisp-1 expr t etat nil nil globals) globals)))

(defun squash-lisp-3-wrap (expr)
  `(macrolet ((unwind-catch (object body catch-code)
                (let ((bname (make-symbol "BLOCK")))
                  `(block ,bname
                     (catch ,object (return-from ,bname ,body))
                     ,catch-code)))
              (tagbody-unwind-catch (object body catch-code)
                catch-code ;; unused variable
                ;; les (progn object) et (progn x) sert à permettre l'expansion des macros sur x
                ;; (il est possible qu'elles ne soient pas expansées à la racine du tagbody)
                `(tagbody (progn ,object) ,@(mapcar (lambda (x) (if (eq (car x) 'jump-label) (cadr x) (progn x))) (cdr body))))
              (simple-tagbody (&rest body)
                `(tagbody ,@(mapcar (lambda (x) (if (eq (car x) 'jump-label) (cadr x) (progn x))) body)))
              (unwind (object)
                `(throw ,object nil))
              (unwind-for-tagbody (object post-unwind-code)
                object ;; unused variable
                post-unwind-code)
              (top-level (main globals body)
                globals
                `(progn
                   ,body
                   (funcall ,main nil)))
              (jump (dest)
                `(go ,dest))
              (get-var (x)
                x)
              (make-closure (fun &rest vars)
                (let ((args-sym (make-symbol "AR")))
                  `(lambda (&rest ,args-sym)
                     (apply ,fun (list ,@vars) ,args-sym))))
              (make-captured-var (x)
                `(setq ,x (cons nil nil)))
              (get-captured-var (x)
                `(car ,x))
              (set-captured-var (x v)
                `(setf (car ,x) ,v)))
     ,expr))

(require 'test-unitaire "test-unitaire")
(erase-tests squash-lisp-3)

(deftest (squash-lisp-3 internal progn)
    (squash-lisp-3-internal '(progn
                              (progn (progn) (progn))
                              (progn)
                              (progn (progn) (progn) (progn)))
                            '(nil . nil))
  ''nil)

(deftest (squash-lisp-3 internal progn)
    (squash-lisp-3-internal '(progn) '(nil . nil))
  ''nil)

(deftest (squash-lisp-3 internal progn)
    (squash-lisp-3-internal '(progn (symbol-value 'a)) '((a) . nil))
  '(symbol-value 'a))

(deftest (squash-lisp-3 internal progn)
    (squash-lisp-3-internal '(progn
                              (progn (progn (symbol-value 'a)) (progn))
                              (progn)
                              (progn (progn) (progn) (progn)))
                            '((a) . nil))
  '(progn (symbol-value 'a) 'nil))

;(run-tests squash-lisp-3)