(require 'match "match")

(defun squash-lisp-3-internal (expr globals &optional (local-env (cons nil nil)) (getset (cons nil nil)) (top-level (cons nil nil)))
  "Lorsqu'une variable à l'intérieur d'une `lambda' référence une déclaration à l'extérieur de la `lambda', on la marque comme étant *capturée*.
   
   On fusionne tous les `let' d'une `lambda' en les remontant dans un `let' unique à la racine de la `lamdba'.

   [Abandonné, fait dans la compilation] On fusionne tous les `tagbody' d'une `lambda' en les remontant dans un `tagbody' unique à la
   racine de la `lambda' ? + transformation des if en tagbody.
   
   On sort toutes les lambdas (fonctions anonymes), on les nomme avec un symbole unique, et on les met au top-level.
   
   local-env : car = variables locales, cdr = variables capturées."
  (macrolet ((transform (expr &optional (local-env 'local-env) (getset 'getset)) `(squash-lisp-3-internal ,expr globals ,local-env ,getset top-level)))
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
      (transform body))
     ((named-lambda :name $$ :params (&rest :params-name $$) :unused _ :body (let ($$*) _*))
      (let* ((new-local-env (cons (list params-name) nil))
             (tbody (transform body new-local-env))
             (new-getset nil)
             (our-captures nil)
             (not-our-captures nil))
        ;; on "nomme" la lambda, et ce nom est global
        (push name (car globals))
        ;; on transforme les get-var de variables capturées en get-captured-var
        (dolist (getter (car getset))
          (if (member (cadr getter) (cdr local-env))
              (setf (car getter) 'get-captured-var)
              (push getter new-getset)))
        ;; on remplace le (car getset) par ceux qui ont été ignorés
        (setf (car getset) new-getset)
        ;; on nettoie pour faire les sets
        (setf new-getset nil)
        ;; on transforme les get-var de variables capturées en get-captured-var
        (dolist (setter (cdr getset))
          (if (member (cadr setter) (cdr local-env))
              (setf (car setter) 'set-captured-var)
              (push setter new-getset)))
        ;; on remplace le (cdr getset) par ceux qui ont été ignorés
        (setf (cdr getset) new-getset)
        ;; on récupère les noms variables qu'on a déclaré (c'est nous qui les capturons).
        (setf our-captures (intersection (car new-local-env) (cdr new-local-env)))
        (setf not-our-captures (set-difference (cdr new-local-env) our-captures))
        ;; on ajoute celles qu'on n'a pas capturé aux captures d'au-dessus
        (setf (cdr local-env) (append not-our-captures (cdr local-env)))
        ;; on construit la lambda au top-level
        (push `(set ,name (lambda ,params ,unused
                                  (let ,(car new-local-env)
                                    ,@(mapcar (lambda (x) `(make-captured-var ,x)) our-captures)
                                    ,tbody)))
              (car top-level))
        ;; on remplace toute la lambda par un accès à sa définition au top-level
        `(symbol-value ',name)))
     ((funcall :fun _ :params _*)
      `(funcall ,@(mapcar (lambda (x) (transform x)) (cons fun params))))
     ((quote _)
      expr)
     ((get-var :var $$)
      ;; chercher si var est dans (car local-env) ou bien dans global
      ;; si oui -> get-var
      ;; sinon, -> get-captured-var
      (if (or (member var (car local-env)) (member var (car globals)))
          (progn
            (push expr (car getset))
            expr)
          (progn
            (pushnew var (cdr local-env))
            `(get-captured-var ,var))))
     ((setq :name $$ :value _)
      ;; comme ci-dessus
      (if (or (member name (car local-env)) (member name (car globals)))
          (progn
            (push expr (car getset))
            `(setq ,name ,(transform value)))
          (progn
            (pushnew name (cdr local-env))
            `(set-captured-var ,name ,(transform value)))))
     ;; + (transform value)
     ((fdefinition (quote $$))
      expr)
     ((symbol-value (quote $$))
      expr)
     ((set :var (quote $$) :value _)
      `(set ,var ,(transform value)))
     (_
      (error "squash-lisp-3-internal : Assertion failed ! this should not be here : ~a" expr)))))

(defun squash-lisp-3 (expr globals)
  (let* ((tl (cons nil nil))
         (lsym (make-symbol "MAIN"))
         (psym (make-symbol "NO-PARAMETERS")))
    (squash-lisp-3-internal `(named-lambda ,lsym (&rest ,psym) (get-var ,psym) (let () ,expr)) globals (cons nil nil) (cons nil nil) tl)
    `(top-level ,lsym (progn ,@(car tl)))))


(defun squash-lisp-3-check-internal (expr)
  "Vérifie si expr est bien un résultat valable de squash-lisp-1.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 1.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (cond-match
   expr
   ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
   (((? (member x '(progn simple-tagbody))) :body _*)
    (every #'squash-lisp-3-check-internal body))
   ((if :condition _ :si-vrai _ :si-faux _)
    (and (squash-lisp-3-check-internal condition)
         (squash-lisp-3-check-internal si-vrai)
         (squash-lisp-3-check-internal si-faux)))
   ((unwind-protect :body _ :cleanup _)
    (and (squash-lisp-3-check-internal body)
         (squash-lisp-3-check-internal cleanup)))
   ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
   (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
    (and (squash-lisp-3-check-internal object)
         (squash-lisp-3-check-internal body)
         (squash-lisp-3-check-internal catch-code)))
   ((unwind :object _)
    (squash-lisp-3-check-internal object))
   ((unwind-for-tagbody :object _ :post-unwind-code _)
    (and (squash-lisp-3-check-internal object)
         (squash-lisp-3-check-internal post-unwind-code)))
   ((jump-label :name $$)
    t)
   ((jump :dest $$)
    t)
   ;; ((let ($$*) :body _)
   ;;  (squash-lisp-3-check-internal body))
   ;; ((lambda (&rest $$) :unused _ :body (let ($$*) _*))
   ;;  (squash-lisp-3-check-internal body))
   ((funcall :fun _ :params _*)
    (every #'squash-lisp-3-check-internal (cons fun params)))
   ((quote _)
    t)
   ((get-var $$)
    t)
   ((setq :name $$ :value _)
    (squash-lisp-3-check-internal value))
   ((fdefinition (quote $$))
    t)
   ((symbol-value (quote $$))
    t)
   ((set (quote $$) :value _)
    (squash-lisp-3-check-internal value))
   ((make-captured-var $$)
    t)
   ((get-captured-var $$)
    t)
   ((set-captured-var $$ :value _)
    (squash-lisp-3-check-internal value))
   (_
    (warn "squash-lisp-3-check-internal: Assertion failed ! This should not be here : ~w" expr)
    nil)))

(defun squash-lisp-3-check (expr)
  (cond-match expr
              ((top-level $$ (progn :body _*))
               (every (lambda (x)
                        (cond-match x
                                    ((set $$ (lambda (&rest $$) (get-var $$)
                                                     (let ($$*) :bodys _*)))
                                     (every #'squash-lisp-3-check-internal bodys))
                                    (_
                                     (warn "~&squash-lisp-3-check : this should not be here :~&~a" x)
                                     nil)))
                      body))
              (_ (warn "~&squash-lisp-3-check : this should not be here :~&~a" expr)
                 nil)))

(defun nice-squash-lisp-3-check (expr)
  (match (top-level $$ (progn
                         (set $$ (lambda (&rest $$) (get-var $$)
                                         (let ($$*) (? squash-lisp-3-check-internal)*)))*))
         expr))
  
(defun squash-lisp-1+3 (expr &optional (etat (list nil nil nil)))
  (let ((globals (cons nil nil)))
    (squash-lisp-3 (squash-lisp-1 expr t etat nil nil globals) globals)))

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