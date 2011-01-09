(require 'match "match")
(require 'util "util")

;; TODO : Quand l'ancienne valeur d'une variable spéciale est sauvegardée par un let, si il y a un throw pendant ce temps-là, elle n'est pas restaurée.
;;        CLTL 7.11 : Intervening dynamic bindings of special variables and catch tags are undone.
;; TODO : Les variables spéciales ne sont probablement pas correctement capturées par un lambda.

(defmacro etat-local (etat)
  `(car ,etat))

(defmacro etat-global (etat)
  `(cadr ,etat))

(defmacro etat-special (etat)
  ;; Variables spéciales et constantes. (ou devrait-on mettre les constantes dans etat-global ?)
  ;; Note sur les constantes : On ne protège pas contre la modification de parties d'une constante non atomique (mais clisp non plus, donc ça va).
  `(caddr ,etat))

(defun assoc-etat (var type etat)
  (let ((search (cons var type)))
    (or (assoc search (etat-special etat) :test #'equal)
        (assoc search (etat-local   etat) :test #'equal)
        (assoc search (etat-global  etat) :test #'equal))))

(defun assoc-special (var type etat)
  (assoc (cons var type) (etat-special etat) :test #'equal))

(defun replace-local (etat new-etat-local)
  (cons new-etat-local (cdr etat)))
    
(defun push-local (etat var type value)
  (when (and (eq type 'variable) (assoc-etat var 'constant etat))
    (error "mini-meval : Can't bind ~w : it is a constant." var))
  (replace-local etat (acons (cons var type) value (etat-local etat))))

(defun push-local-or-special (etat var type value immediate)
  (let ((association (assoc-special var type etat))
        (new-etat nil))
    (if association
        (progn
          (setq new-etat (push-local etat var 'special-bakcup (cons association (cdr association))))
          (if immediate
              (progn (setf (cdr association) value)
                     new-etat)
              (push-local new-etat var 'special-future-phantom (cons association value))))
        (push-local etat var 'variable value))))

(defun affect-future-specials (new-etat etat)
  (setq new-etat (etat-local new-etat))
  (setq etat (etat-local etat))
  (tagbody
   loop
     (when (eq new-etat etat) (go fin))
     (when (eq (cdaar new-etat) 'special-future-phantom)
       (setf (cdr (cadar new-etat)) (cddar new-etat)))
     (setq new-etat (cdr new-etat))
     (go loop)
   fin))

(defun pop-special-backups (new-etat etat)
  (setq new-etat (etat-local new-etat))
  (setq etat (etat-local etat))
  (tagbody
   loop
     (when (eq new-etat etat) (go fin))
     (when (eq (cdaar new-etat) 'special-bakcup)
       (setf (cdr (cadar new-etat)) (cddar new-etat)))
     (setq new-etat (cdr new-etat))
     (go loop)
   fin))

(defun push-global! (etat name type value)
  (setf (etat-global etat) (acons (cons name type) value (etat-global etat)))
  etat)

(defun push-special! (etat name type value)
  (setf (etat-special etat) (acons (cons name type) value (etat-special etat)))
  etat)

(defun reduce-on-local-1 (new-etat-local callback lists)
  (let ((res nil))
    (tagbody
     loop
       (when (member nil lists) (go fin))
       (setq res (apply callback new-etat-local (mapcar #'car lists)))
       (setq new-etat-local (acons (cons (car res) (cadr res))
                                   (caddr res)
                                   new-etat-local))
       (setq lists (mapcar #'cdr lists))
       (go loop)
     fin)
    new-etat-local))

(defun reduce-on-local (etat callback &rest lists)
  (if (null lists)
      etat
      (replace-local etat (reduce-on-local-1 (etat-local etat) callback lists))))

;; DONE
;; - loop
;; - dolist / dotimes
;; - match-automaton(tagbody+block)

;; HALF-DONE (TODO)
;; - read
;; - warn
;; - ` (quasiquote)

;; TODO (dans mini-meval et/ou compilateur) :
;; - syntaxe courte du let
;; - declaim
;; - format
;; - setf (écrire la macro)
;; - fdefinition, funcctionp, …
;; - symbolp, keywordp, keywords non mutables + nil et t, intern, make-symbol
;; - car / cdr, replaca replacad, cons, list (fonction), listp, consp, atom, null (ou eq nil), …
;; - and / or (macros => if)
;; - &rest
;; - eq (vérifier qu'on préserve bien l'égalité de pointeurs là où il faut) / = / eql / equal / equalp
;; - load / open / close
;; - defvar (gestion correcte des variables spéciales)
;; - array support (array-total-size, row-major-aref, copy-seq)
;; - string support (char=, map, string (symbol => string), format, print)
;; - coder un reverse rapide.
;; - transformation de la récursion terminale.

;; - vérifier qu'on a pas transformé certaines fonctions en formes spéciales (il faut qu'on puisse toujours les récupérer avec #').
;; - sortir le defun du mini-meval ?

;; cell (un seul pointeur, transparent (y compris pour le type),
;; avec trois fonctions spéciales pour le get / set / tester le type),
;; sera utilisé pour les closures et les variables spéciales.

(defun slice-up-lambda-list (lambda-list)
  (match-automaton lambda-list fixed
                   (fixed    accept)
                   (fixed    optional &optional)
                   (fixed    rest     &rest)
                   (fixed    key      &key)
                   (fixed    aux      &aux)
                   (fixed    reject   $&)
                   (fixed    fixed    (:var . $$) var)
                   (optional accept)
                   (optional rest     &rest)
                   (optional key      &key)
                   (optional aux      &aux)
                   (optional reject   $&)
                   (optional optional (:var . $$) `(,var nil nil))
                   (optional optional (:var $$ :default _? :svar $$?) `(,var ,(car default) ,(car svar)))
                   (rest     reject   $&)
                   (rest     rest2    (:var . $$) var)
                   (rest2    accept)
                   (rest2    key      &key)
                   (rest2    aux      &aux)
                   (rest2    reject   $&)
                   (key      accept)
                   (key      other    &allow-other-keys)
                   (key      aux      &aux)
                   (key      reject   $&)
                   (key      key      (:keyword . $k) `(,(keyword-to-symbol keyword) ,(keyword-to-symbol keyword) nil nil)) ;; Not in the standard !
                   (key      key      (:var . $$) `(,var ,var nil nil))
                   (key      key      (:keyword $$ :default _? :svar $$?) `(,(keyword-to-symbol keyword) ,(keyword-to-symbol keyword) ,(car default) ,(car svar))) ;; Not in the standard !
                   (key      key      (:var $$ :default _? :svar $$?) `(,var ,var ,(car default) ,(car svar)))
                   (key      key      ((:keyword $k :var $$) :default _? :svar $$?) `(,(keyword-to-symbol keyword) ,var ,(car default) ,(car svar)))
                   (other    collect  t)
                   (other    accept)
                   (other    aux      &aux)
                   (other    reject   $&)
                   (aux      accept)
                   (aux      reject   $&)
                   (aux      aux      (:var . $$) `(,var nil))
                   (aux      aux      (:var $$ :default _?) `(,var ,(car default)))
                   (reject (error "slice-up-lambda-list : ~w ne peut être ici." last-element))))

;; Exemples :
;; TODO : en faire des tests unitaires.
;; (slice-up-lambda-list '(a b &optional u &rest c &key ((:foo bar)) :quux (:baz 'glop) &aux (x 1) (y (+ x 2))))
;; (slice-up-lambda-list '(a b &rest))
;; (slice-up-lambda-list '(a b))

(declaim (ftype function mini-meval)) ;; récursion mutuelle mini-meval-get-params-from-real -> mini-meval-params / mini-meval
(defun mini-meval-params (params etat fixed optional rest key other aux)
  (let ((new-etat etat)
        (value nil)
        (svar nil)
        (current-key)
        (search-key)
        (seen-keys))
    (tagbody
     fixed
       (when (endp fixed) (go end-fixed))
       (when (endp params) (error "mini-meval-params : not enough parameters !"))
       (setq new-etat (push-local-or-special new-etat (car fixed) 'variable (car params) nil))
       (setq params (cdr params))
       (setq fixed (cdr fixed))
       (go fixed)
     end-fixed
       (affect-future-specials new-etat etat)
     optional
       (when (endp optional) (go rest))
       (if (endp params)
           (setq value (mini-meval (cadar optional) new-etat)) ;; default value
           (setq value (car params)))
       (setq new-etat (push-local-or-special new-etat (caar optional) 'variable value t))
       (setq svar (caddar optional))
       (when svar
         (setq new-etat (push-local-or-special new-etat svar 'variable (endp params) t)))
       (setq params (cdr params))
       (setq optional (cdr optional))
       (go optional)
     rest
       (unless rest (go key))
       (setq new-etat (push-local new-etat (car rest) 'variable params))
     key
       (when (or (endp key) (endp params)) (go defaults-keys))
       (when (endp (cdr params)) (error "mini-meval-params : odd number of key parameters"))
       (setq search-key (keyword-to-symbol (car params)))
       (when (eq search-key (caar key))
         (setq current-key (car key))
         (push (car current-key) seen-keys)
         (setq key (cdr key))
         (go end-assoc-key-loop))
     assoc-key-loop
       (when (endp (cdr key))
         (go unknown-key))
       (when (eq search-key (caadr key))
         (setq current-key (cadr key))
         (push (car current-key) seen-keys)
         (setf (cdr key) (cddr key))
         (go end-assoc-key-loop))
       (go assoc-key-loop)
     end-assoc-key-loop
       (setq new-etat (push-local-or-special new-etat (second current-key) 'variable (second params) t))
       (setq svar (fourth current-key))
       (when svar
         (setq new-etat (push-local-or-special new-etat svar 'variable t t)))
       (go after-unknown-key)
     unknown-key
       (unless (or other (member search-key seen-keys))
         (error "mini-meval-params : invalid key : ~w" (car params)))
     after-unknown-key
       (setq key (cdr key))
       (setq params (cddr params))
     defaults-keys
       (dolist (k key)
         (setq new-etat (push-local-or-special new-etat (second k) 'variable (mini-meval (third k) new-etat) t))
         (setq svar (fourth k))
         (when svar
           (setq new-etat (push-local-or-special new-etat svar 'variable nil t))))
     aux
       (when (endp aux) (go fin))
       (setq new-etat (push-local-or-special new-etat (caar aux) 'variable (mini-meval (cadar aux) new-etat) t))
       (setq aux (cdr aux))
     fin)
    new-etat))

(defun mini-meval-get-params-from-real (etat lambda-list effective-parameters)
  "Lambda-list doit être déjà sliced."
  (mini-meval-params effective-parameters etat
                     (cdr (assoc 'fixed    lambda-list)) ;; TODO : optimiser ça peut-être...
                     (cdr (assoc 'optional lambda-list))
                     (cdr (assoc 'rest     lambda-list))
                     (cdr (assoc 'key      lambda-list))
                     (cdr (assoc 'other    lambda-list))
                     (cdr (assoc 'aux      lambda-list))))

(defun splice-up-tagbody-1 (todo-body body result)
  (if (endp todo-body)
      (acons nil body result)
    (if (or (symbolp (car todo-body)) (numberp (car todo-body)))
        (splice-up-tagbody-1 (cdr todo-body)
                             body
                             (acons (car todo-body) body result))
      (splice-up-tagbody-1 (cdr todo-body)
                           (cons (car todo-body) body)
                           result))))

(defun splice-up-tagbody (body)
  (splice-up-tagbody-1 (reverse body) nil nil))

(defun mini-meval-error (expr etat &rest message)
  (error "mini-meval : ~w~&expression = ~w~&etat-global = ~w~&etat-local = ~w~&etat-special = ~w"
         (apply #'format nil message)
         expr
         (etat-global etat)
         (etat-local etat)
         (etat-special etat)))

#|
Mini-meval est un meval très simple destiné à évaluer les macros et les autres directives avec eval-when :compile-toplevel.

;; Fonctionnement de mini-meval
Mini-meval sera appellé sur des morceaux spécifiques du fichier source. Il faut donc qu'il puisse maintenir son état entre les appels.
|#
(defun mini-meval (expr &optional (etat (list nil nil nil)))
  #|
  L'algorithme d'évaluation est très simple et suit le schéma donné dans CLTL 5.1.3 :
    1) Si l'expression est une forme spéciale, on la traite de manière particulière
    2) Si l'expression est un appel de macro, on évalue le corps de la macro avec les paramètres tels quels (non évalués),
       puis on remplace l'appel par son résutlat, et on évalue ce résultat.
    3) Sinon, c'est un appel de fonction.
  Pour permettre au code de bas niveau de redéfinir les formes spéciales, on fera d'abord la macro-expansion (étape 2).
  |#
  
  (cond-match
   expr
   #| 2) Cas des macros |#
   ((:name $$ :params _*)
    (let ((definition (assoc-etat name 'macro etat)))
      (if definition
          (mini-meval (apply (cdr definition) params) etat)
          (else))))
   #| 1) Cas des formes spéciales |#
   ((eval-when :situations ($*) :body _*)
    (if (member :execute situations)
        (mini-meval `(progn ,@body) etat)
        nil))
   ((flet ((:name $ :lambda-list @ :fbody _*)*) :body _*)
    (mini-meval `(progn ,@body)
                (reduce-on-local
                 etat
                 (lambda (ignore name lambda-list fbody) ignore
                   (list name 'function (mini-meval `(lambda ,lambda-list ,@fbody) etat)))
                 name lambda-list fbody)))
   ((labels ((:name $ :lambda-list @ :fbody _*)*) :body _*)
    (let* ((new-etat (reduce-on-local
                      etat
                      (lambda (ignore name) ignore (list name 'function nil))
                      name))
           (new-etat-local (etat-local new-etat)))
      (dolist* ((name name) (lambda-list lambda-list) (fbody fbody))
               (setf (cdr (assoc `(,name . function) new-etat-local :test #'equal))
                     (mini-meval `(lambda ,lambda-list ,@fbody) new-etat)))
      (mini-meval `(progn ,@body) new-etat)))
   ;; Transformation des (let[*] (var1 var2 var3) …) en (let[*] ((var1 nil) (var2 nil) (var3 nil)) …)
   ((:type (? or (eq x 'let) (eq x 'let*)) :bindings (? and consp (find-if #'symbolp x)) :body . _)
    (mini-meval `(,type ,(mapcar (lambda (b) (if (consp b) b `(b nil))) bindings) ,@body)))
   ((let ((:name $ :value _)*) :body _*)
    (let ((new-etat etat)
          (res nil))
      (dolist* ((name name) (value value))
               (setq new-etat (push-local-or-special new-etat name 'variable (mini-meval value etat) nil)))
      (affect-future-specials new-etat etat)
      (setq res (mini-meval `(progn ,@body) new-etat))
      (pop-special-backups new-etat etat)
      res))
   (((? (eq x 'let*)) ((:name $ :value _)*) :body _*)
    (let ((new-etat etat)
          (res nil))
      ;; pour chaque variable
      (dolist* ((name name) (value value))
               (setq new-etat (push-local-or-special new-etat name 'variable (mini-meval value new-etat) t)))
      (setq res (mini-meval `(progn ,@body) new-etat))
      (pop-special-backups new-etat etat)
      res))
   ((macrolet ((:name $ :lambda-list @ :mbody _*)*) :body _*)
    (let ((new-etat
           (reduce-on-local
            etat
            (lambda (ignore name lambda-list mbody) ignore
                    ;; comme le flet sauf nil au lieu de new-etat-local
                    ;; CLTL 7.5 :
                    ;; The precise rule is that the macro-expansion functions defined
                    ;; by macrolet are defined in the global environment; lexically
                    ;; scoped entities that would ordinarily be lexically apparent
                    ;; are not visible within the expansion functions.
                    (list name 'macro
                          (mini-meval `(lambda ,lambda-list ,@mbody) (replace-local etat nil))))
            name lambda-list mbody))
          (get-etat (assoc-etat 'trapdoor 'squash-trapdoor etat)))
      (if (and get-etat (eq (car body) (cdr get-etat)))
          new-etat ;; Trapdoor pour récupérer l'etat avec les définitions du macrolet.
          (mini-meval `(progn ,@body) new-etat))))
   ((progn :body _*)
    (let ((res nil))
      (dolist (expr body res)
        (setq res (mini-meval expr etat)))))
   ((if :condition _ :si-vrai _ :si-faux _?)
    (if (mini-meval condition etat)
        (mini-meval si-vrai etat)
        (if si-faux
            (mini-meval (car si-faux) etat)
            nil)))
   ((lambda :lambda-list @ :body _*)
    (let ((sliced-lambda-list (slice-up-lambda-list lambda-list))
          (old-etat etat))
      (lambda (&rest effective-parameters)
        (let* ((new-etat (mini-meval-get-params-from-real old-etat sliced-lambda-list effective-parameters))
               (res (mini-meval `(progn ,@body) new-etat)))
          (pop-special-backups new-etat etat)
          res))))
   ;; Lorsqu'une fonction "littérale" est présente dans le code, on la renvoie telle qu'elle.
   ((? functionp)
    expr)
   ((defun :name $ :lambda-list @ :body _*)
    (push-global! etat name 'function
                  (mini-meval `(lambda ,lambda-list ,@body) etat))
    name)
   ((defmacro :name $ :lambda-list @ :body _*)
    (push-global! etat name 'macro
                  (mini-meval `(lambda ,lambda-list ,@body) etat))
    name)
   ((defvar :name $ :value _)
    (when (assoc-etat name 'constant etat) (mini-meval-error expr etat "Can't bind ~w : it is a constant." name))
    (let ((definition (assoc-etat name 'variable etat)))
      ;; NOTE : if you do a "defvar" while in a "let" that binds the same variable, the result is gibberish and nonsensical.
      ;; But that case is fairly rare and not worth the effort and run-time cost.
      (push-special! etat name 'variable
                     (if definition
                         (cdr definition)
                         (mini-meval value etat))))
    name)
   ((setq :name $ :value _)
    (let ((definition (assoc-etat name 'variable etat))
          (real-value (mini-meval value etat))) ;; Faut-il vérifier que NAME n'est pas une constante *avant* de calculer la valeur ?
      (if definition
          (setf (cdr definition) real-value)
          (progn
            (when (assoc-etat name 'constant etat) (mini-meval-error expr etat "Can't set ~w : it is a constant." name))
            (push-global! etat name 'variable (mini-meval value etat))))
      real-value))
   ((declaim _*)
    nil)
   ((error :format _ :args _*)
    (error "mini-meval : fonction error appellée par le code, le message est :~&~w" (apply #'format nil format args)))
   ((warn :format _ :args _*)
    (warn "mini-meval : fonction warn appellée par le code, le message est :~&~w" (apply #'format nil format args)))
   ((go :target (? or symbolp numberp))
    (when (null target)
      (mini-meval-error expr etat "nil ne peut pas être une étiquette pour un go."))
    (let ((association (assoc `(,target . tagbody-tag) (etat-local etat) :test #'equal)))
      (if association
          (funcall (cdr association))
          (mini-meval-error expr etat "tentative de faire un go sur une étiquette qui n'existe pas ou plus : ~w.~&~w" target))))
   ((tagbody :body _*)
    (let ((spliced-body (splice-up-tagbody body))
          (next-tag nil)
          (new-etat nil))
      (tagbody
       init
         (setq new-etat
               (reduce-on-local
                etat
                (lambda (ignore tag) ignore
                  (list (car tag) 'tagbody-tag
                        (lambda () (setq next-tag (car tag)) (go go-to-tag))))
                spliced-body))
       go-to-tag
         (mini-meval `(progn ,@(cdr (assoc next-tag spliced-body)))
                     new-etat))))
   ((return-from :block-name $$ :value _)
    (let ((association (assoc `(,block-name . block-name) (etat-local etat) :test #'equal)))
      (if association
          (funcall (cdr association) value)
          (mini-meval-error expr etat "tentative de faire un return-from pour un bloc qui n'existe pas ou plus : ~w." block-name))))
   ((block :block-name $$ :body _*)
    (block block-catcher
      (mini-meval `(progn ,@body)
                  (push-local etat block-name 'block-name (lambda (x) (return-from block-catcher x))))))
   ((quote :val _)
    val)
   ((function :name $$)
    (let ((definition (assoc-etat name 'function etat)))
      (if definition
          (cdr definition)
          (mini-meval-error expr etat "Undefined function : ~w." name))))
   ((function :fun (lambda _ . _))
    (mini-meval fun etat))
   ((funcall :name _ :params _*)
    (apply (mini-meval name etat)
           (mapcar (lambda (x) (mini-meval x etat)) params)))
   ((apply :name _ :p1 _ :params _*)
    (let ((fun (mini-meval name etat))
          (args (mapcar (lambda (x) (mini-meval x etat)) (cons p1 params))))
      (apply fun (append (butlast args) (car (last args))))))
   #| Traitement des appels de fonction |#
   ((:lambda (lambda @ _*) :params _*)
    #| - Si c'est une fonction anonyme, on l'exécute. |#
    (apply (mini-meval lambda etat) params))
   (((function :fun (lambda _ . _)) :params . _)
    (mini-meval `(,fun ,@params) etat))
   ((:name (function $$) :params _*)
    (apply (mini-meval name etat) params))
   ((:name $$ :params _*)
    (let ((definition (assoc-etat name 'function etat)))
      (if definition
          #| - Si on a une fonction de ce nom dans l'état-local ou dans l'etat-global, on l'exécute. |#
          (apply (cdr definition) (mapcar (lambda (x) (mini-meval x etat)) params))
          (mini-meval-error expr etat "Undefined function : ~w." name))))
   ((? or numberp stringp)
    expr)
   ;; TODO : nil et t devraient être des defconst
   (nil
    nil)
   ($$
    (let ((definition (assoc-etat expr 'variable etat)))
      (if definition
          (cdr definition)
          (mini-meval-error expr etat "Undefined variable : ~w." expr))))))

(defun push-functions (etat functions)
  (dolist (f functions)
    (push-global! etat f 'function (fdefinition f)))
  etat)

(defmacro make-etat (&rest functions)
  `(push-functions (list nil nil nil) ',functions))

(defun etat-exemple ()
  (make-etat list + - cons car cdr < > <= >= =))

(require 'test-unitaire "test-unitaire")
(erase-tests mini-meval)

(deftestvar mini-meval etat (make-etat list + - cons car cdr < > <= >= =))

;; La plupart des tests sont dans eqiv-tests.lisp

(deftest (mini-meval lambda extérieur)
    (funcall (mini-meval '(lambda (x) x) etat) 3)
  3)

(deftest (mini-meval lambda extérieur)
    (funcall (mini-meval '(lambda (x) (+ x 3)) etat) 4)
  7)

(deftest (mini-meval defvar)
    (mini-meval '(progn (defvar x 42) x) etat)
  42)

;; Syntaxe supplémentaire non reconnue par le standard : (#'fun param*)
(deftest (mini-meval call-function extérieur)
    (mini-meval '(#'+ 2 3) etat)
  5)

;; Syntaxe supplémentaire non reconnue par le standard : (#'(lambda ...) param*)
(deftest (mini-meval call-function lambda)
    (mini-meval '(#'(lambda (x) (+ x 40)) 2) etat)
  42)

(deftest (mini-meval defvar special)
  (mini-meval '(progn
                (defun foo1 () var)
                (defun foo2 () (let ((var 4)) (list var (foo1))))
                (defvar var 123)
                (list (foo1) (foo2)))
              etat)
  '(123 (4 4)))

(deftest (mini-meval defun)
    (mini-meval '(progn (defun double (x) (+ x x)) (double 3)) etat)
  6)

(deftest (mini-meval defmacro)
    (mini-meval '(progn (defmacro qlist (x y) (list 'list (list 'quote x) (list 'quote y))) (qlist a b)) etat)
  '(a b))

(deftest (mini-meval macrolet)
    (mini-meval '(progn
                  (defun qlist (a b) (list a b))
                  (list
                   (qlist 'a 'b)
                   (macrolet ((qlist (x y) (list 'list (list 'quote x) (list 'quote y))))
                     (qlist 'a 'b))
                   (qlist 'a 'b)))
                etat)
  '((a b) ('a 'b) (a b)))

(deftest (mini-meval setf setq)
    (mini-meval '(list (defvar x 42) x (setq x 123) x) etat)
  '(x 42 123 123))

;; TODO : tests setf

(deftest (mini-meval function internal)
    (funcall (mini-meval '(progn (defun foo (x) (+ x 40)) #'foo) etat) 2)
  '42)

(deftest (mini-meval function internal)
    (mini-meval '(progn (defun foo (x) (+ x 40)) (funcall #'foo 2)) etat)
  '42)

(deftest (mini-meval function internal)
    (mini-meval '(progn (defvar bar (list (lambda (x) (+ x 40)) 1 2 3)) (funcall (car bar) 2)) etat)
  '42)

(deftest (mini-meval call-function internal)
    (mini-meval '(progn (defun foo (x) (+ x 40)) (#'foo 2)) etat)
  42)

(deftest (mini-meval lambda closure single-instance)
    (mini-meval '(progn
                  (defvar foo (let ((y 1)) (cons (lambda (x) (list x y)) (lambda (z) (setq y (+ y z)) nil))))
                  (list (funcall (car foo) 4) (funcall (cdr foo) 5) (funcall (car foo) 4))) etat)
  '((4 1) nil (4 6)))

(deftest (mini-meval lambda closure multiple-instances)
    (mini-meval '(progn
                  (defun counter (&optional (ctr 0)) (cons (lambda () ctr) (lambda (&optional (x 1)) (setq ctr (+ ctr x)) nil)))
                  (defvar foo0  (counter))
                  (defvar foo42 (counter 42))
                  (list
                   (funcall (car foo0))    ;; show 0
                   (funcall (car foo42))   ;; show 42
                   (funcall (cdr foo0))    ;; add 0
                   (funcall (car foo0))    ;; show 0
                   (funcall (cdr foo42))   ;; add 42
                   (funcall (car foo42))   ;; show 42
                   (funcall (car foo0))    ;; shwo 0
                   (funcall (car foo42))   ;; show 42
                   (funcall (cdr foo42) 6) ;; add 42 (+ 6)
                   (funcall (cdr foo0) 5)  ;; add 0  (+ 5)
                   (funcall (car foo42))   ;; show 42
                   (funcall (car foo0))))  ;; show 0
                etat)
  '(0 42 nil 1 nil 43 1 43 nil nil 49 6))

(deftest (mini-meval labels)
    (mini-meval '(list
                  (defun foo (x) (+ x 1))
                  (foo 3)
                  (labels ((foo (x) (+ x 3)))
                    (foo 3)))
                etat)
  '(foo 4 6))

(deftest (mini-meval flet)
    (mini-meval '(list
                  (defun foo (x) (+ x 1))
                  (foo 3)
                  (flet ((foo (x) (+ x 3)))
                    (foo 3)))
                etat)
  '(foo 4 6))

(deftest (mini-meval labels)
    (mini-meval '(list
                  (defun fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
                  (fibo 5)
                  (labels ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
                    (fibo 5)))
                etat)
  '(fibo 8 5))

(deftest (mini-meval flet)
    (mini-meval '(list
                  (defun fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
                  (fibo 5)
                  (flet ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
                    (fibo 5)))
                etat)
  ;; Le flet ne permet pas les définitions récursives, donc le fibo
  ;; de l'extérieur est appellé après le 1er niveau de récursion.
  '(fibo 8 8))

(deftest-error (mini-meval error)
    (mini-meval '(error "Some user error message.")))

(provide 'mini-meval)
