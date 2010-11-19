(require 'match "match")
(require 'util "util")

;; TODO (dans mini-meval et/ou compilateur) :
;; - match-automaton(tagbody+block)
;; - declaim
;; - format
;; - ` (quasiquote)
;; - setf (écrire la macro)
;; - fdefinition, funcctionp, …
;; - symbolp, keywordp, keywords non mutables + nil et t, intern, make-symbol
;; - car / cdr, replaca replacad, cons, list (fonction), listp, consp, atom, null (ou eq nil), …
;; - and / or (macros => if)
;; - &rest
;; - eq (vérifier qu'on préserve bien l'égalité de pointeurs là où il faut) / = / eql / equal / equalp
;; - load / open / read / close
;; - defvar (gestion correcte des variables spéciales)
;; - loop
;; - dolist / dotimes
;; - array support (array-total-size, row-major-aref, copy-seq)
;; - string support (char=, map, string (symbol => string), format, print)
;; - warn
;; - coder un reverse rapide.
;; - transformation de la récursion terminale.

;; - vérifier qu'on a pas transformé certaines fonctions en formes spéciales (il faut qu'on puisse toujours les récupérer avec #').

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

(defun splice-up-tagbody-1 (todo-body body result)
  (if (endp todo-body)
      (acons nil body result)
    (if (symbolp (car todo-body))
        (splice-up-tagbody-1 (cdr todo-body)
                             body
                             (acons (car todo-body) body result))
      (splice-up-tagbody-1 (cdr todo-body)
                           (cons (car todo-body) body)
                           result))))

(defun splice-up-tagbody (body)
  (splice-up-tagbody-1 (reverse body) nil nil))

(defun mini-meval-error (expr etat-global etat-local &rest message)
  (error "~w~&expression = ~w~&etat-global = ~w~&etat-local = ~w"
         (apply #'format nil message)
         expr
         etat-global
         etat-local))

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
    (mini-meval `(progn ,@body)
                etat-global
                (reduce* etat-local (lambda (new-etat-local name lambda-list fbody)
                                      (acons `(,name . function)
                                             (mini-meval `(lambda ,lambda-list ,@fbody) etat-global etat-local)
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
                (setf (cdr (assoc `(,name . function) new-bindings :test #'equal))
                      (mini-meval `(lambda ,lambda-list ,@fbody) etat-global new-etat-local)))
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
   ((setq :name $ :value _)
    (let ((definition (assoc* `(,name . variable) #'equal etat-local (cdr etat-global))))
      (if definition
          (let ((real-value (mini-meval value etat-global etat-local)))
            (setf (cdr definition) real-value)
            real-value)
          (mini-meval `(defvar ,name ,value) etat-global etat-local))))
   ((function :name $$)
    (let ((definition (assoc* `(,name . function) #'equal etat-local (cdr etat-global))))
      (if definition
          (cdr definition)
          (mini-meval-error expr etat-global etat-local "mini-meval : undefined function : ~w." name))))
   ((funcall :name _ :params _*)
    (apply (mini-meval name etat-global etat-local)
           (mapcar (lambda (x) (mini-meval x etat-global etat-local)) params)))
   ((apply :name _ :p1 _ :params _*)
    (let ((fun (mini-meval name etat-global etat-local))
          (args (mapcar (lambda (x) (mini-meval x etat-global etat-local)) (cons p1 params))))
      (apply fun (append (butlast args) (car (last args))))))
   ((declaim _*)
    nil)
   ((error :format _ :args _*)
    (error "mini-meval : fonction error appellée par le code, le message est :~&~w" (apply #'format nil format args)))
   ((go :target $$)
    (when (null target)
      (min-meval-error expr etat-global etat-local "mini-meval : nil ne peut pas être une étiquette pour un return-from."))
    (let ((association (assoc* `(,target . tagbody-tag) #'equal etat-local etat-global)))
      (if association
          (funcall (cdr association))
        (mini-meval-error expr etat-global etat-local "mini-meval : tentative de faire un go sur une étiquette qui n'existe pas ou plus : ~w.~&~w" target))))
   ((tagbody :body _*)
    (let ((spliced-body (splice-up-tagbody body))
          (next-tag nil)
          (new-etat-local nil))
      (tagbody
       init
       (setq new-etat-local
             (reduce* etat-local
                      (lambda (new-etat-local tag)
                        (acons `(,(car tag) . tagbody-tag)
                               (lambda () (setq next-tag (car tag)) (go go-to-tag))
                               new-etat-local))
                      spliced-body))
       go-to-tag
       (mini-meval `(progn ,@(cdr (assoc next-tag spliced-body)))
                   etat-global
                   new-etat-local))))
   ((return-from :block-name $$ :value _)
    (let ((association (assoc* `(,block-name . block-name) #'equal etat-local etat-global)))
      (if association
          (funcall (cdr association) value)
        (mini-meval-error expr etat-global etat-local "mini-meval : tentative de faire un return-from pour un bloc qui n'existe pas ou plus : ~w." block-name))))
   ((block :block-name $$ :body _*)
    (block block-catcher
      (mini-meval `(progn ,@body) etat-global (acons `(,block-name . block-name)
                                                     (lambda (x) (return-from block-catcher x))
                                                     etat-local))))
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
          (mini-meval-error expr etat-global etat-local "mini-meval : undefined function : ~w." name))))
   ((:num . (? numberp))
    num)
   ((:str . (? stringp))
    str)
   (()
    nil)
   ((:name . $$)
    (let ((definition (assoc* `(,name . variable) #'equal etat-local (cdr etat-global))))
      (if definition
          (cdr definition)
          (mini-meval-error expr etat-global etat-local "mini-meval : undefined variable : ~w." name))))))

(defun push-functions (etat-global functions)
  (cons nil (mapcar-append (cdr etat-global) (lambda (x) `((,x .  function) . ,(fdefinition x))) functions)))

(defmacro etat-global-fn (&rest functions)
  `(push-functions '(nil) ',functions))

(load "test-unitaire")
(erase-tests mini-meval)

(deftestvar mini-meval e-global (etat-global-fn list + - cons car cdr < > <= >= =))

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

(deftest (mini-meval setf setq)
    (mini-meval '(list (defvar x 42) x (setq x 123) x) e-global nil)
  '(x 42 123 123))

(deftest (mini-meval funcall)
    (mini-meval '(funcall #'+ 1 2 3) e-global nil)
  '6)

(deftest (mini-meval apply)
    (mini-meval '(apply #'+ 1 2 (list (+ 1 2) 4)) e-global nil)
  '10)

(deftest (mini-meval function external)
    (mini-meval '#'+ e-global nil)
  #'+)

(deftest (mini-meval function external)
    (mini-meval '(funcall #'+ 1 2 3) e-global nil)
  '6)

(deftest (mini-meval function internal)
    (funcall (mini-meval '(progn (defun foo (x) (+ x 40)) #'foo) e-global nil) 2)
  '42)

(deftest (mini-meval function internal)
    (mini-meval '(progn (defun foo (x) (+ x 40)) (funcall #'foo 2)) e-global nil)
  '42)

(deftest (mini-meval function internal)
    (mini-meval '(progn (defvar bar (list (lambda (x) (+ x 40)) 1 2 3)) (funcall (car bar) 2)) e-global nil)
  '42)

(deftest (mini-meval lambda optional)
    (mini-meval '((lambda (x &optional (y 2)) (list x y)) 1) e-global nil)
  '(1 2))

(deftest (mini-meval lambda closure single-instance)
    (mini-meval '(progn
                  (defvar foo (let ((y 1)) (cons (lambda (x) (list x y)) (lambda (z) (setq y (+ y z)) nil))))
                  (list (funcall (car foo) 4) (funcall (cdr foo) 5) (funcall (car foo) 4))) e-global nil)
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
                e-global nil)
  '(0 42 nil 1 nil 43 1 43 nil nil 49 6))

(deftest (mini-meval labels)
    (mini-meval '(list
                  (defun foo (x) (+ x 1))
                  (foo 3)
                  (labels ((foo (x) (+ x 3)))
                    (foo 3)))
                e-global nil)
  '(foo 4 6))

(deftest (mini-meval flet)
    (mini-meval '(list
                  (defun foo (x) (+ x 1))
                  (foo 3)
                  (flet ((foo (x) (+ x 3)))
                    (foo 3)))
                e-global nil)
  '(foo 4 6))

(deftest (mini-meval labels)
    (mini-meval '(< 2 3) e-global nil)
  t)
 
(deftest (mini-meval labels)
    (mini-meval '(list
                  (defun fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
                  (fibo 5)
                  (labels ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
                    (fibo 5)))
                e-global nil)
  '(fibo 8 5))

(deftest (mini-meval flet)
    (mini-meval '(list
                  (defun fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
                  (fibo 5)
                  (flet ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
                    (fibo 5)))
                e-global nil)
  ;; Le flet ne permet pas les définitions récursives, donc le fibo
  ;; de l'extérieur est appellé après le 1er niveau de récursion.
  '(fibo 8 8))

(deftest-error (mini-meval error)
    (mini-meval '(error "Some user error message.") (cons nil nil) nil))

(deftest (mini-meval tagbody)
  (mini-meval '(let ((x 0)) (tagbody foo (setq x 1) (go baz) bar (setq x 2) baz) x))
  1)

(deftest (mini-meval tagbody)
  (mini-meval '(tagbody foo 1 bar 2 baz 3))
  nil)

(deftest (mini-meval block)
  (mini-meval '(block foo 1 (return-from foo 4) 2))
  4)

(deftest (mini-meval block)
  (mini-meval '(block foo 1 2))
  2)

(provide 'mini-meval)