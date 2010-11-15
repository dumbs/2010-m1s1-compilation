(load "match")
(load "util")

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
                   (rest     rest2    (:var . $$) `(,var))
                   (rest2    accept)
                   (rest2    key      &key)
                   (rest2    aux      &aux)
                   (rest2    reject   $&)
                   (key      accept)
                   (key      other    &allow-other-keys t)
                   (key      aux      &aux)
                   (key      reject   $&)
                   (key      key      (:keyword . $k) `(,keyword ,(keyword-to-symbol keyword) nil nil))
                   (key      key      (:var . $$) `(,var ,var nil nil))
                   (key      key      (:keyword $$ :default _? :svar $$?) `(,keyword ,(keyword-to-symbol keyword) ,(car default) ,(car svar)))
                   (key      key      (:var $$ :default _? :svar $$?) `(,var ,var ,(car default) ,(car svar)))
                   (key      key      ((:keyword $k :var $$) :default _? :svar $$?) `(,keyword ,var ,(car default) ,(car svar)))
                   (other    accept)
                   (other    aux      &aux)
                   (other    reject   $&)
                   (aux      accept)
                   (aux      reject   $&)
                   (aux      aux      (:var . $$) `(,var nil))
                   (aux      aux      (:var $$ :default _?) `(,var ,(car default)))
                   (reject (error "slice-up-lambda-list : ~w ne peut être ici." last-element))))

;; Exemples :
;; (slice-up-lambda-list '(a b &optional u &rest c &key ((:foo bar)) :quux (:baz 'glop) &aux (x 1) (y (+ x 2))))
;; (slice-up-lambda-list '(a b &rest))
;; (slice-up-lambda-list '(a b))

(declaim (ftype function mini-meval)) ;; récursion mutuelle mini-meval-params / mini-meval
(defun mini-meval-params (params global local fixed optional rest key other aux)
  (if fixed
      (if (endp params)
          (error "mini-meval-params : not enough parameters !")
          (mini-meval-params (cdr params) global (acons `(,(car fixed) . variable) (car params) local) (cdr fixed) optional rest key other aux))
      (if optional
          (let* ((var (caar optional))
                 (value (if (endp params)
                            (mini-meval (cadar optional) global local)
                            (car params)))
                 (svar (caddar optional))
                 (new-local (acons `(,var . variable) value local))
                 (new-local-2 (if svar
                                  (acons `(,svar . variable) (endp params) new-local)
                                  new-local)))
            (mini-meval-params (cdr params) global new-local-2 nil (cdr optional) rest key other aux))
          (if rest
              (mini-meval-params params global (acons `(,(car rest) . variable) params local) nil nil nil key other aux)
              ;; TODO : finir d'implémenter &key &allow-other-keys &aux &rest (et relire CLTL).
              local))))
;              (if key
;                  (let* ((keyword (first (car key)))
;                         (var (second (car key)))
;                         (maybe-val (member keyword params))
;                         (maybe-val-2 (if maybe-val
;                                          (if (n-consp 2 maybe-val)
;                                              maybe-val
;                                              (error "mini-meval-params : Nombre de paramètres impair alors qu'il y a &key."))))
;                         (svar (fourth (car key)))
;                         (new-local (acons `(,var . variable) (if maybe-val-2
;                                                   (cadr maybe-val-2)
;                                                   (mini-meval (third (car key)) global local))
;                                           local))
;                         (new-local-2 (if svar
;                                          (acons `(,svar . variable) (not (not (maybe-val-2))) new-local)
;                                          new-local)))
;                    (mini-meval-params params global new-local-2 nil nil nil (cdr key) other aux)

(defun mini-meval-get-params-from-real (etat-global etat-local lambda-list effective-parameters)
  "Lambda-list doit être déjà sliced."
  (funcall #'mini-meval-params effective-parameters etat-global etat-local
           (cdr (assoc 'fixed    lambda-list))
           (cdr (assoc 'optional lambda-list))
           (cdr (assoc 'rest     lambda-list))
           (cdr (assoc 'key      lambda-list))
           (cdr (assoc 'other    lambda-list))
           (cdr (assoc 'aux      lambda-list))))

#|
Mini-meval est un meval très simple destiné à évaluer les macros et les autres directives avec eval-when :compile-toplevel.

;; Fonctionnement de mini-meval
Mini-meval sera appellé sur des morceaux spécifiques du fichier source. Il faut donc qu'il puisse maintenir son état entre les appels.
|#
(defun mini-meval (expr &optional (etat-global (cons nil nil)) etat-local)
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
   ((debug :id _?)
    (format t "~&debug :~&  id     = ~w~&  global = ~w~&  local  = ~w" id etat-global etat-local))
   #| 2) Cas des macros |#
   ((:name $$ :params _*)
    (let ((definition (assoc* `(,name . macro) #'equal etat-local (cdr etat-global))))
      (if definition
          #| - Si on a une fonction de ce nom dans l'état-local ou dans l'etat-global, on l'exécute. |#
          (mini-meval (apply (cdr definition) params) etat-global etat-local)
          (else))))
   #| 1) Cas des formes spéciales |#
   ((eval-when :situations ($*) :body _*)
    (if (member :execute situations)
        (mini-meval body etat-global etat-local)
        nil))
   ((flet ((:name $ :lambda-list @ :fbody _*)*) :body _*)
    (mini-meval `(progn ,body)
                etat-global
                (reduce* etat-local (lambda (new-etat-local name lambda-list fbody)
                                      (acons `(,name . function)
                                             (mini-meval `(lamdba ,lambda-list ,@fbody) etat-global etat-local)
                                             new-etat-local))
                         name lambda-list fbody)))
   ((labels ((:name $ :lambda-list @ :fbody _*)*) :body _*)
    (let* ((new-bindings (reduce* nil (lambda (new-bindings name) `(((,name . function) . nil) . ,new-bindings))
                                  name))
           (new-etat-local (append new-bindings etat-local)))
      (mapcar (lambda (name lambda-list fbody)
                ;; On fait un assoc / setf dans new-bindings, qui ne contient que les fonctions qu'on vient juste d'ajouter, pour éviter
                ;; le risque inexistant de faire une mutation dans etat-local.
                ;; TODO : vérifier que ça marche.
                (assoc-set `(,name . function)
                      (mini-meval `(lambda ,lambda-list ,@fbody) new-etat-local)
                      new-bindings
                      #'equal))
              name lambda-list fbody)
      (mini-meval `(progn ,@body) etat-global new-etat-local)))
   ((let ((:name $ :value _)*) :body _*)
    (mini-meval `(progn ,@body)
                etat-global
                (reduce* etat-local (lambda (new-etat-local name value)
                                      (acons `(,name . variable)
                                             (mini-meval value etat-global etat-local)
                                             new-etat-local))
                         name value)))
   (((? (eq x 'let*)) ((:name $ :value _)*) :body _*)
    (mini-meval `(progn ,@body)
                etat-global
                (reduce* etat-local (lambda (new-etat-local name value)
                                      (acons `(,name . variable)
                                             ;; Comme let sauf new-etat-local au lieu de etat-local ici.
                                             (mini-meval value etat-global new-etat-local)
                                             new-etat-local))
                         name value)))
   ((macrolet ((:name $ :lambda-list @ :mbody _*)*) :body _*)
    (mini-meval `(progn ,@body)
                etat-global
                (reduce* etat-local (lambda (new-etat-local name lambda-list mbody)
                                      (acons `(,name . macro)
                                             ;; comme le flet sauf nil au lieu de new-etat-local
                                             ;; CLTL 7.5 :
                                             ;; The precise rule is that the macro-expansion functions defined
                                             ;; by macrolet are defined in the global environment; lexically
                                             ;; scoped entities that would ordinarily be lexically apparent
                                             ;; are not visible within the expansion functions.
                                             (mini-meval `(lambda ,lambda-list ,@mbody) etat-global nil)
                                             new-etat-local))
                         name lambda-list mbody)))
   ((progn :body _*)
    (car (last (mapcar (lambda (expr) (mini-meval expr etat-global etat-local))
                       body))))
   ((if :condition _ :si-vrai _ :si-faux _?)
    (if (mini-meval condition etat-global etat-local)
        (mini-meval si-vrai etat-global etat-local)
        (if si-faux
            (mini-meval (car si-faux) etat-global etat-local)
            nil)))
   ((lambda :lambda-list @ :body _*)
    (let ((sliced-lambda-list (slice-up-lambda-list lambda-list)))
      (lambda (&rest effective-parameters)
        (mini-meval `(progn ,@body)
                    etat-global
                    (mini-meval-get-params-from-real etat-global etat-local sliced-lambda-list effective-parameters)))))
   ((defun :name $ :lambda-list @ :body _*)
    (assoc-set `(,name . function)
          (mini-meval `(lambda ,lambda-list ,@body) etat-global etat-local)
          (cdr etat-global)
          #'equal)
    name)
   ((defmacro :name $ :lambda-list @ :body _*)
    (assoc-set `(,name . macro)
          (mini-meval `(lambda ,lambda-list ,@body) etat-global etat-local)
          (cdr etat-global)
          #'equal)
    name)
   ((defvar :name $ :value _)
    (assoc-set `(,name . variable)
          (mini-meval value etat-global etat-local)
          (cdr etat-global)
          #'equal)
    name)
   ((setf/setq)
    )
   ((quote :val _)
    val)
   #| Traitement des appels de fonction |#
   ((:lambda (lambda @ _*) :params _*)
    #| - Si c'est une fonction anonyme, on l'exécute. |#
    (apply (mini-meval lambda etat-global etat-local) params))
   ((:name $ :params _*)
    (let ((definition (assoc* `(,name . function) #'equal etat-local (cdr etat-global))))
      (if definition
          #| - Si on a une fonction de ce nom dans l'état-local ou dans l'etat-global, on l'exécute. |#
          (apply (cdr definition) (mapcar (lambda (x) (mini-meval x etat-global etat-local)) params))
          (error "mini-meval : undefined function : ~w.~&etat-global = ~w~&etat-local = ~w" name etat-global etat-local))))
   ((:name . $$)
    (let ((definition (assoc* `(,name . variable) #'equal etat-local (cdr etat-global))))
      (if definition
          (cdr definition)
          (error "mini-meval : undefined variable : ~w.~&etat-global = ~w~&etat-local = ~w" name etat-global etat-local))))
   ((:num . (? numberp))
    num)
   ((:str . (? stringp))
    str)
   (()
    nil)))

(defun push-functions (etat-global functions)
  (cons nil (mapcar-append (cdr etat-global) (lambda (x) `((,x .  function) . ,(fdefinition x))) functions)))

(defmacro etat-global-fn (&rest functions)
  `(push-functions '(nil) ',functions))

(load "test-unitaire")
(erase-tests mini-meval)

(deftestvar mini-meval e-global `(nil ((list . function) . ,#'list) ((+ . function) . ,#'+)))

(deftest (mini-meval constante)
    (mini-meval 42 e-global nil)
  42)

(deftest (mini-meval appel-fonction)
    (mini-meval '(+ 2 3) e-global nil)
  5)

(deftest (mini-meval appel-fonction)
    (mini-meval '(+ 2 (+ 3 4)) e-global nil)
  9)

(deftest (mini-meval variable)
    (mini-meval 'x e-global (acons '(x . variable) 42 nil))
  42)

(deftest (mini-meval appel-fonction-et-variable)
    (mini-meval '(+ x x 3) e-global (acons '(x . variable) 42 nil))
  87)

(deftest (mini-meval appel-fonction-et-variable)
    (mini-meval '(+ x (+ 3 x)) e-global (acons '(x . variable) 42 nil))
  87)

(deftest (mini-meval lambda extérieur)
    (funcall (mini-meval '(lambda (x) x) e-global nil) 3)
  3)

(deftest (mini-meval lambda extérieur)
    (funcall (mini-meval '(lambda (x) (+ x 3)) e-global nil) 4)
  7)

(deftest (mini-meval lambda immédiat)
    (mini-meval '((lambda (x) (+ x 3)) 4) e-global nil)
  7)

(deftest (mini-meval let)
    (mini-meval '(let ((x 3) (y 4)) (+ x y)) e-global nil)
  7)

(deftest (mini-meval let)
    (mini-meval '(let ((x 3) (y 4) (z 5)) (let ((z (+ x y)) (w z)) (list x y z w))) e-global nil)
  '(3 4 7 5))

(deftest (mini-meval let*)
    (mini-meval '(let ((x 3) (y 4) (z 5)) (let* ((z (+ x y)) (w z)) (list x y z w))) e-global nil)
  '(3 4 7 7))

(deftest (mini-meval progn)
    (mini-meval '(progn 1 2 3 4) e-global nil)
  4)

(deftest (mini-meval defvar)
    (mini-meval '(progn (defvar x 42) x) e-global nil)
  42)

(deftest (mini-meval defun)
    (mini-meval '(progn (defun double (x) (+ x x)) (double 3)) e-global nil)
  6)

(deftest (mini-meval quote)
    (mini-meval ''x e-global nil)
  'x)

(deftest (mini-meval defmacro)
    (mini-meval '(progn (defmacro qlist (x y) (list 'list (list 'quote x) (list 'quote y))) (qlist a b)) e-global nil)
  '(a b))

(deftest (mini-meval macrolet)
    (mini-meval '(progn
                  (defun qlist (a b) (list a b))
                  (list
                   (qlist 'a 'b)
                   (macrolet ((qlist (x y) (list 'list (list 'quote x) (list 'quote y))))
                     (qlist 'a 'b))
                   (qlist 'a 'b)))
                e-global nil)
  '((a b) ('a 'b) (a b)))
