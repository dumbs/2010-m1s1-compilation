(require 'mini-meval "mini-meval") ;; slice-up-lambda-list, macro-expansion, eval-when
(require 'match "match")
(require 'util "match") ;; derived-symbol, assoc-or, assoc-or-push

;; À la fin du fichier se trouvent des notes sur la compilation d'unwind & co.

;; TODO !!! Utiliser une pile descendante,
;; TODO !!! donc adapter les calculs pour unwind,
;; TODO !!! sinon on n'aura pas la compatibilité x86



;; TODO : transformer les if (cond?) en simple-tagbody
;; TODO : mini-meval + squash-lisp-1 : les defmacro doivent définir une macro-fonction, pour qu'on puisse utiliser macroexpand dans le
;;        source passé en paramètre. donc (defmacro foo (params) body*) -> (setf (fdefinition 'foo) (lambda (params) (block foo body*)))

(defun simple-splice-up-tagbody (body)
  "Découpe le body d'un tagbody en une liste associative ordonnée toute simple :
   (tagbody a b (foo bar) c (baz) (quux) d) => '((a) (b (foo bar)) (c (baz) (quux)) (d))"
  (let ((all-res nil) (res (list (make-symbol "START"))))
    (tagbody
     start
       (when (endp body)
         (push (reverse res) all-res)
         (go end))
       (when (and (car body) (or (symbolp (car body)) (numberp (car body))))
         (push (reverse res) all-res)
         (setq res (list (car body)))
         (setq body (cdr body))
         (go start))
       (push (car body) res)
       (setq  body (cdr body))
       (go start)
     end)
    (reverse all-res)))

(defun squash-lisp-1 (expr &optional (at-toplevel t) (etat (list nil nil nil)) env-var env-fun (globals (cons nil nil)))
  "Transformation 1 :
   
   Supprime les macros, eval-when, tagbody/go, throw/catch, block/return-from,
   transforme les appels de fonction en funcall, les constantes en quote
   et simplifie pas mal d'autres choses.

   Transformation 2 :
   
   Transforme les let, let*, flet, labels, lambda en super-let et simple-lambda,
   détecte les variables et fonctions globales et stocke leurs noms dans le
   paramètre globals, qui est une paire (noms-variables . noms-fonctions), et
   rend tous les noms locaux de fonction (flet/labels) et de
   variables (let/let*/lambda) uniques, mais pas les globaux.

   `super-let' est lui-même transformé dans la foulée en simple-let qui ne fait
   que déclarer les noms de variables, mais n'affecte pas de valeur
   lui-même (les affectations sont faites avec des setq)

   `at-toplevel' permet de déterminer si une expression est considérée comme
   étant au top-level (pour les defmacro, eval-when, etc).

   `etat' est l'état du compilateur (macro-expansion, eval-when avec
   compile-time) env-var et env-fun et globals sont les environnements du code
   compilé, utilisés pour rendre uniques tous les symboles."
  (macrolet ((transform (expr &optional at-toplevel (etat 'etat)) `(squash-lisp-1 ,expr ,at-toplevel ,etat env-var env-fun globals)))
    (cond-match
     expr
     ;; - Si on rencontre une macro définie dans l'environnement de compiler-meval,
     ;;   1) On demande à compiler-meval d'expanser la macro sur un niveau.
     ;;   2) On re-lance la transformation (eval-when / defmacro / appel de macro / ...) sur le résultat s'il y a a eu expansion.
     ((:name $$ :params _*)
      (let ((definition (assoc-etat name 'macro etat)))
        (if definition
            (transform (apply (cdr definition) params) at-toplevel)
            (else))))
     
     ;; - Si on rencontre EVAL-WHEN,
     ;;   - Au top-level,
     ;;     - Pour chaque form du body,
     ;;       - Si la situation contient :compile-toplevel, le form est évalué dans compiler-meval.
     ;;       - Si la situation contient :load-toplevel, le form est compilé (après évaluation dans compiler-meval s'il y a lieu).
     ;;       - Lorsqu'un eval-when au top-level contient des eval-when directement sous lui, ils sont traités comme s'ils étaient directement au top-level.
     ;;   - Ailleurs
     ;;     - Si la situation contient :load-toplevel, le form est compilé
     ((eval-when :situations ($*) :body _*)
      (when (and at-toplevel (member :compile-toplevel situations))
        (mini-meval `(progn ,@body) etat))
      (if (member :load-toplevel situations)
          (transform body at-toplevel)
          (transform 'nil))) ;; on renvoie nil
     
     ;; - Si on rencontre un defmacro (au toplevel ou ailleurs).
     ;;   - On demande à compiler-meval de l'exécuter.
     ((defmacro :name $ :lambda-list @ :body _*)
      (mini-meval expr etat)
      (transform `',name)) ;; on renvoie le nom
     
     ;; - Si on rencontre un macrolet
     ;;   - On fait une copie de l'état de compiler-meval
     ;;   - On lui demande d'exécuter les définitions
     ;;   - On évalue le body avec ce nouvel état
     ;;   - On continue avec l'ancien état
     ((macrolet :definitions ((:name $ :lambda-list @ :mbody _*)*) :body _*)
      (let ((get-etat (make-symbol "GET-ETAT")))
        (transform
         `(progn ,@body)
         at-toplevel
         (mini-meval `(macrolet ,definitions ,get-etat) (push-local etat 'trapdoor 'squash-trapdoor get-etat)))))
     
     ;; - Si on gère le symbol-macrolet
     ;;   - Le fonctionnement est le même que pour le macrolet
     ;;   - Lorsqu'on rencontre un symbole, on regarde s'il a une définition de type symbol-macrolet
     ((symbol-macrolet . _)
      (error "squash-lisp-1 : Symbol-macrolet n'est pas implémenté."))
     
     ((progn :body _*)
      (cons 'progn (mapcar (lambda (form) (transform form at-toplevel)) body)))
     
     ((if :condition _ :si-vrai _ :si-faux _?)
      `(if ,(transform condition)
           ,(transform si-vrai)
           ,(transform (car si-faux))))
     
     ;; Lorsqu'on rentre dans un block, on met sur la pile un marqueur spécial avec un pointeur vers un objet créé à l'exécution.
     ((block :block-name $$ :body _*)
      (let ((retval-sym (make-symbol "RETVAL"))
            (block-id-sym (make-symbol "BLOCK-ID")))
        (transform
         `(let ((,retval-sym nil)
                ;; Il y a un peu de redondance, car block-id-sym
                ;; stocké dans le let et dans le unwind-catch
                (,block-id-sym (cons nil nil)))
            (unwind-catch ,block-id-sym
                          (progn ,@body)
                          ,retval-sym))
         nil
         (push-local etat block-name 'squash-block-catch (cons block-id-sym retval-sym)))))
     
     ;; Les return-from <nom> qui sont accessibles lexicalement sont remplacés par un (unwind <l'objet>)
     ;; Unwind remonte la pile jusqu'à trouver le marqueur spécial, tout en exécutant les unwind-protect éventuels.
     ;; Si unwind ne trouve pas le marqueur et arrive en haut de la pile, il signale une erreur et termine le programme.
     ;; Sinon, l'exécution reprend après le block.
     ((return-from :block-name $$ :value _)
      (let ((association (assoc-etat block-name 'squash-block-catch etat)))
        (unless association (error "Squash-Lisp-1 : Can't return from block ~w, it is inexistant or not lexically apparent." block-name))
        (transform `(progn (setq ,(cddr association) ,value)
                           (unwind ,(cadr association))))))
     
     ;; Le traitement de tagbody/go est similaire pour sortir d'un tag, puis on jmp directement sur le tag de destination (vu qu'il est au même niveau).
     ((tagbody :body _*)
      (let ((spliced-body (simple-splice-up-tagbody body))
            (res nil)
            (unwind-catch-marker-sym (make-symbol "UNWIND-CATCH-MARKER-SYM"))
            (new-etat etat)
            (unique-label-sym nil)
            (tagbody-id-sym (make-symbol "TAGBODY-ID")))
        (dolist (zone spliced-body)
          (setq unique-label-sym (make-symbol (format nil "~a" (car zone))))
          (setq new-etat (push-local new-etat (car zone) 'squash-tagbody-catch (cons unwind-catch-marker-sym unique-label-sym)))
          (setf (car zone) unique-label-sym))
        (transform
         `(let ((,tagbody-id-sym (cons nil nil)))
            (tagbody-unwind-catch ,tagbody-id-sym
                                  (progn
                                    ,@(progn (dolist (zone spliced-body)
                                               (push `(jump-label ,(car zone)) res) 
                                               (push `(progn ,@(cdr zone)) res))
                                             ;; (cdr (reverse …)) pour zapper le tout premier (jump-label …)
                                             (cdr (reverse res))))
                                  nil)
            nil)
         nil
         new-etat)))
     
     ((go :target (? or symbolp numberp))
      (let ((association (assoc-etat target 'squash-tagbody-catch etat)))
        (unless association (error "Squash-Lisp-1 : Can't go to label ~w, it is inexistant or not lexically apparent." target))
        (transform `(progn (unwind-for-tagbody ,(cadr association)
                                               (jump ,(cddr association)))))))
     
     ;; Le traitement de catch/throw est similaire, sauf que le pointeur est simplement un pointeur vers l'objet utilisé pour le catch / throw.
     ((catch :tag _ :body _*)
      (transform `(unwind-catch ,tag (progn ,@body) singleton-catch-retval)))

     ((throw :tag _ :result _)
      (transform `(progn (setq singleton-catch-retval ,result)
               (unwind ,tag))))
     
     ;; Simplification du unwind-protect
     ((unwind-protect :body _ :a-cleanup _ :other-cleanups _+)
      `(unwind-protect ,(transform body)
         ,(transform `(progn ,a-cleanup ,@other-cleanups))))
     
     ((unwind-protect :body _ :a-cleanup _)
      `(unwind-protect ,(transform body)
         ,(transform a-cleanup)))
     
     ((:type (? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body _ :catch-code _)
      `(,type ,(transform object)
              ,(transform body)
              ,(transform catch-code)))
     
     ((unwind :object _)
      `(unwind ,(transform object)))
     
     ((unwind-for-tagbody :object _ :post-unwind-code _)
      `(unwind-for-tagbody ,(transform object) ,(transform post-unwind-code)))
     
     ((jump-label :name $$)
      expr)
     
     ((jump :dest $$)
      expr)

     ;; Transformation des (let[*] (var1 var2 var3) …) en (let[*] ((var1 nil) (var2 nil) (var3 nil)) …)
     ((:type (? or (eq x 'let) (eq x 'let*)) :bindings (? and consp (find-if #'symbolp x)) :body . _)
      (transform `(,type ,(mapcar (lambda (b) (if (consp b) b `(b nil))) bindings) ,@body)))
     
     ((super-let :name ($$*) :stuff _*)
      (setq name (mapcar (lambda (x) (cons x (derived-symbol x))) name))
      (labels ((transform-super-let (expr)
                 `(progn
                    ,@(loop
                         for (type . clause) in expr
                         when (eq type 'set)
                         collect `(setq ,(cdr (assoc (car clause) name)) (transform (cadr clause)))
                         when (eq type 'use-var)
                         do (push (assoc (car clause) name) env-var)
                         when (eq type 'use-fun)
                         do (push (assoc (car clause) name) env-fun)
                         when (eq type 'if)
                         do `(if ,(transform (car clause))
                                 (progn ,(mapcar #'transform-super-let (cadr clause)))
                                 (progn ,(mapcar #'transform-super-let (caddr clause))))
                         when (eq type 'progn)
                         collect `(progn ,(mapcar (lambda (x) (transform x)) clause))))))
        ;; Note : ce <let> ne sera pas re-transformé (sinon boucle infinie).
        `(let ,(mapcar #'cdr name)
           ,(transform-super-let expr))))
     
     ((let ((:name $$ :value _)*) :body _*)
      (transform
       `(super-let ,name
                   ,@(mapcar (lambda (n v) `(set ,n ,v)) name value)
                   ,@(mapcar (lambda (n) `(use-var ,n)) name)
                   (progn ,@body))))
     
     (((? (eq x 'let*)) ((:name $$ :value _)*) :body _*)
      (transform
       `(super-let ,name
                   ,@(loop
                        for n in name
                        for v in value
                        collect `(set ,n ,v)
                        collect `(use-var ,n))
                   (progn ,@body))))
     
     ((simple-flet ((:name $$ :params @ :fbody _*)*) :body _*)
      (transform
       `(super-let ,name
                   ,@(mapcar (lambda (n params fbody) `(set ,n (lambda ,params (progn ,@fbody)))) name params fbody)
                   ,@(mapcar (lambda (n) `(use-fun ,n)) name)
                   (progn ,@body))))
     
     ((simple-labels ((:name $$ :params @ :fbody _*)*) :body _*)
      (transform
       `(super-let ,name
                   ,@(mapcar (lambda (n) `(use-fun ,n)) name)
                   ,@(mapcar (lambda (n params fbody) `(set ,n (lambda ,params (progn ,@fbody)))) name params fbody)
                   (progn ,@body))))
     
     ;; TODO : defun
     ;; TODO : defvar
     ;; => TODO : global-setq
     ;; => TODO : global-setfun
     ;; => TODO : proclaim
     
     ;; TODO: simplifier la lambda-list.
     ((lambda :params _ :body _*)
      (let* ((sliced-lambda-list (slice-up-lambda-list params))
             (whole-sym    (make-symbol "LAMBDA-PARAMETERS"))
             (temp-key-sym (make-symbol "TEMP-KEY-SYM"))
             (fixed        (cdr (assoc 'fixed    sliced-lambda-list)))
             (optional     (cdr (assoc 'optional sliced-lambda-list)))
             (rest         (cdr (assoc 'rest     sliced-lambda-list)))
             (key          (cdr (assoc 'key      sliced-lambda-list)))
             (other        (cdr (assoc 'other    sliced-lambda-list)))
             (aux          (cdr (assoc 'aux      sliced-lambda-list))))
        `(lambda (&rest ,whole-sym)
           ,(transform
             `(super-let (,@fixed
                          ,@(mapcar #'car optional)
                          ,@(remove nil (mapcar #'third optional))
                          ,@rest
                          ,@(mapcar #'car key)
                          ,@(remove nil (mapcar #'fourth key))
                          ,@(mapcar #'car aux)
                          ,@(if (and key (not other)) `(,temp-key-sym) nil))
                         ,@(loop
                              for param in fixed
                              collect `(set ,param (car ,whole-sym))
                              collect `(use ,param)
                              collect `(progn (setq ,whole-sym (cdr ,whole-sym))))
                         ,@(loop
                              for (param default predicate) in optional
                              collect `(if ,whole-sym
                                           ((set ,param (car whole-sym))
                                            (progn (setq ,whole-sym (cdr ,whole-sym)))
                                            (use ,param)
                                            ,@(if predicate `((set ,predicate t)   (use predicate)) nil))
                                           ((set ,param ,default)
                                            (use ,param)
                                            ,@(if predicate `((set ,predicate nil) (use predicate)) nil))))
                         ,@(if rest
                               `((set ,(car rest) ,whole-sym)
                                 (use ,(car rest)))
                               nil)
                         ,@(if key
                               `(progn (if (evenp (length ,whole-sym)) nil (error "Odd number of arguments, but function has &key.")))
                               nil)
                         ,@(if key
                               (loop
                                  for (keyword param default predicate) in key
                                  ;; TODO : quand on a trouvé, pouvoir faire set et use (support de simple-tagbody & jump-label super-let)
                                  collect (let ((search-key (make-symbol "SEARCH-KEY")))
                                            `((progn (simple-tagbody
                                                      (jump-label ,search-key)
                                                      (if ,temp-key-sym
                                                          (if (eq (car ,search-key) ,keyword)
                                                              ;; trouvé
                                                              nil
                                                              ;; chercher encore
                                                              (progn
                                                                (setq ,temp-key-sym (cddr ,search-key))
                                                                (jump ,search-key)))
                                                          ;; pas trouvé
                                                          nil)))
                                              (if ,temp-key-sym
                                                  ((set ,param (car ,temp-key-sym))
                                                   (use ,param)
                                                   ,@(if predicate `((set predicate t) (use predicate)) nil))
                                                  ((set ,param ,default)
                                                   (use ,param)
                                                   ,@(if predicate `((set predicate nil) (use predicate)) nil))))))
                               nil)
                         ;; TODO : not implemented yet : vérifier s'il y a des key non autorisées.
                         ,@(loop
                              for (param val) in aux
                              collect `(set ,param ,val)
                              collect `(use ,param))
                         (progn ,@body))))))
     
     ((function :fun (lambda . _))
      (transform fun))
     
     ((function :fun $$)
      `(get-var ,(assoc-or fun env-fun (assoc-or-push fun (derived-symbol (string fun)) (cdr globals)))))

     ((funcall :fun _ :params _*)
      `(funcall ,(transform fun) ,@(mapcar (lambda (x) (transform x)) params)))

     ;; TODO : apply
     ;; => TODO : définir la fonction funcall : (funcall #'funcall #'cons 1 2)
     ;; => TODO : définir la fonction apply   : (funcall #'apply #'cons '(1 2))
     
     ((setq :name $$ :value _)
      `(setq ,(assoc-or name env-var (assoc-or-push name (derived-symbol name) (car globals)))
             ,(transform value)))
     
     ((quote _)
      expr)
     
     ((? or numberp stringp)
      `(quote ,expr))
     
     ;; TODO : nil et t devraient être des defconst
     ;; Doit être avant les symboles
     (nil
      ''nil)
     
     ($$
      (print `(get-var ,(assoc-or expr env-var (assoc-or-push expr (derived-symbol expr) (car globals))))))
     
     ;; Appels de fonction
     ;; Doivent être après tout le monde.
     ((:fun $$ :params _*)
      (transform `(funcall (function ,fun) ,@params)))
     
     ((:lambda (lambda . _) :params _*)
      (transform `(funcall ,lambda ,@params)))
     
     (((function :lambda (lambda . _)) :params . _)
      (transform `(funcall ,lambda ,@params)))
     
     (((function :name $$) :params _*)
      (transform `(funcall (function ,name) ,@params)))
     
     (_
      (error "squash-lisp-1: Not implemented yet : ~a" expr)))))

(defun squash-lisp-1-wrap (expr)
  `(macrolet ((unwind-catch (object body catch-code)
                (let ((bname (make-symbol "block")))
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
              ;;Les macros ne sont pas expansées à la racine d'un tagbody, donc on expanse à la main
              ;;  les jump-label lorsqu'on rencontre un tagbody-unwind-catch.
              ;;(jump-label (name)
              ;;  name)
              (jump (dest)
                `(go ,dest))
              (simple-let (spec body)
                `(let spec body))
              (get-var (x)
                x))
     ,expr))

(defun squash-lisp-1-check (expr)
  "Vérifie si expr est bien un résultat valable de squash-lisp-1.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 1.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (cond-match
   expr
   ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
   (((? (member x '(progn simple-tagbody))) :body _*)
    (every #'squash-lisp-1-check body))
   ((if :condition _ :si-vrai _ :si-faux _)
    (and (squash-lisp-1-check condition)
         (squash-lisp-1-check si-vrai)
         (squash-lisp-1-check si-faux)))
   ((unwind-protect :body _ :cleanup _)
    (and (squash-lisp-1-check body)
         (squash-lisp-1-check cleanup)))
   ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
   (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
    (and (squash-lisp-1-check object)
         (squash-lisp-1-check body)
         (squash-lisp-1-check catch-code)))
   ((unwind :object _)
    (squash-lisp-1-check object))
   ((unwind-for-tagbody :object _ :post-unwind-code _)
    (and (squash-lisp-1-check object)
         (squash-lisp-1-check post-unwind-code)))
   ((jump-label :name $$)
    t)
   ((jump :dest $$)
    t)
   ((let ($$*) :body _)
    (squash-lisp-1-check body))
   ((lambda (&rest $$) :body _)
    (squash-lisp-1-check body))
   ((function :fun $$)
    t)
   ((funcall :fun _ :params _*)
    (every #'squash-lisp-1-check (cons fun params)))
   ((quote _)
    t)
   ((get-var $$)
    t)
   ((setq :name $$ :value _)
    (squash-lisp-1-check value))
   (_
    (warn "squash-lisp-1-check: Assertion failed ! This should not be here : ~a" expr)
    nil)))

(require 'test-unitaire "test-unitaire")
(erase-tests squash-lisp-1)
(deftest (squash-lisp-1 wrap unwind)
    (eval (squash-lisp-1-wrap
           '(let ((foo nil))
             (unwind-catch 'foo
              (progn (push 1 foo)
                     (unwind 'foo)
                     (push 2 foo))
              (push 3 foo))
             foo)))
  '(3 1))

#|
Notes sur l'implémentation d'unwind.
Tous les lets qui aparaissent dans un appel de fonction sont regrouppés en un seul. Donc à un appel de fonction correspond un "gros" segment de pile.
Après la mise en place de ce segment de pile, le code est exécuté.

TODO / NOTE pour la suite : on peut se passer des marker-* si on peut s'assurer qu'entre un end-frame et le begin-frame qui suit, il n'y a QUE des unwind-protect, unwind-catch etc.
Donc uniquement des adresses de portions de code généré par le compilateur, pas de "vrais" objets, donc on est sûr qu'il n'y aura pas de confusion entre un "vrai" objet
et une cible mise en place par unwind-catch.

Lorsqu'on rencontre une structure de contrôle comme la suivante :
(unwind-catch object body catch-code)

Elle est compilée ainsi :

push @catch-code
[compile object]
push r0
push @marker-unwind-destination
[compile body]
pop r2
pop r2
pop r2
jmp @after-catch-code
@catch-code
[compile catch-code]
@after-catch-code

De plus, un (unwind-protect body protect-code) est compilé ainsi :
push @protect-code
push @marker-unwind-protect
[compile body]
pop r2
pop r2
jmp @after-protect-code
@protect-code
[compile protect-code]
jmp @start-unwind
@after-protect-code

(unwind-for-tagbody object post-unwind-code) est compilé ainsi :
jsr @find-unwind-destination
mov [immediate]@post-unwind-code @singleton-post-unwind-code
add 3 sp                ;; On "re-push" l'adresse de la cible, l'objet et le marqueur, mettre 2 au lieu de 3 si on n'a pas de marqueur.
mov sp @singleton-unwind-destination
mov r1 sp ;; On remonte en haut de la pile
jmp @start-unwind
@post-unwind-code
[compile post-unwind-code] ;; DOIT contenir un jump !
halt ;; Sinon, on quite "brutalement"

Et enfin, (unwind object) est compilé ainsi :
[compile object]
jsr @find-unwind-destination
mov sp @singleton-unwind-destination
mov r1 sp ;; On remonte en haut de la pile
jmp @start-unwind

Et une fonction (lambda nb-let-vars body) est compilée ainsi
<jsr @function> => push ip; jmp @function

@function
mov sp, r0 ;; begin-frame : Va avec le marker-end-frame
push bp
mov sp, bp ;; sp -> bp
add [nb-let-vars], sp
push r0                  ;; Permet à unwind de sauter directement jusqu'au begin-frame.
push @marker-end-frame   ;; On peut l'ommetre pour accélérer les appels de fonction et/ou
                         ;; quand il n'y a pas de marker-unwind-* à la suite.
;; IMPORTANT : effacer tout le segment de pile _SI_ on n'utilise pas begin/end-frame
;; (car sinon il peut y avoir des destinations / protect d'unwind qui traînent encore).
[body]
sub sp, [nb-let-vars + 2] ;; ERREUR !
pop bp

Les "fonctions" find-unwind-destination et start-unwind :

db 0 ;; 0 = (type-number placeholder) ;; devrait être autre chose, pointeur par ex
@singleton-unwind-destination
db4 0
db 0 ;; 0 = (type-number placeholder) ;; devrait être autre chose, pointeur par ex
@singleton-post-unwind-code
db4 0
@marker-unwind-destination
db 0
db 0 ;; Toujours au moins deux octets (ou au moins 5 ? j'ai oublié…).
@marker-unwind-protect
db 0
db 0 ;; Toujours au moins deux octets (ou au moins 5 ? j'ai oublié…).
@marker-end-frame
db 0
db 0 ;; Toujours au moins deux octets (ou au moins 5 ? j'ai oublié…).

;; fud == find-unwind-destination
@find-unwind-destination
mov sp r1
@fud-loop
cmp sp 2 ;; ???             ;; Ne pas passer en-dessous de 0.
jpe @unwind-not-found-error ;; Ne pas passer en-dessous de 0.
pop r2
cmp r2 @marker-end-frame
jeq @fud-skip-frame
cmp r2 @marker-unwind-destination
jne @fud-loop
pop r2
cmp r2 r0
pop r2        ;; Récupérer l'adresse de retour
mov @nil *sp  ;; écraser l'adresse de retour avec @nil pour désactiver la cible.
jne @fud-loop
;; fud-found
cmp r2 @nil  ;; Cible désactivée ?
jeq @fud-loop
mov r2 @singleton-post-unwind-code
ret

@fud-skip-frame
pop r2
mov r2 sp
jmp @fud-loop

@unwind-not-found-error
;; error : cant unwind to this object, the return point doesn't exist anymore.
;; TODO : mettre un code d'erreur dans r2 (par exemple)
halt

@start-unwind
;; su == start-unwind
@su-loop
cmp sp *@singleton-unwind-destination
jeq *@singleton-post-unwind-code ;; Fin de l'unwind, tout le monde descend !
pop r0
cmp r0 @marker-end-frame
jeq @su-skip-frame
cmp r0 @marker-unwind-protect
jne @su-loop
pop r0
jmp r0

@su-skip-frame
pop r0
mov r0 sp
jmp @su-loop

On a donc une pile de cette forme (les vieilles données sont en haut) :
** [old bp] ;; adresse = 987
== BP ==
   [variable foo]
   [variable bar]
   [variable ...]
   [begin-frame à l'adresse 987]
** [end-frame]
   [protect-code à l'adresse 1234]
** [unwind-protect]
   [code unwind-catch à l'adresse 1111]
   [objet]
** [unwind-catch]
   [protect-code à l'adresse 2222]
** [unwind-protect]
   [protect-code à l'adresse 3333]
** [unwind-protect]
   [code unwind-catch à l'adresse 4444]
   [objet]
** [unwind-catch]
   [protect-code à l'adresse 5555]
** [unwind-protect]
== SP ==

|#

(provide 'squash-lisp-1)
