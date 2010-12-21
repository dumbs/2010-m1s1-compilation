(require 'mini-meval "mini-meval")
(require 'match "match")

;; TODO : emballer le résultat de squash-lisp dans un (macrolet ...) pour les "special-operator" qu'on rajoute.

;; TODO !!!
;; TODO !!! Utiliser une pile descendante (donc adapter les calculs pour unwind), sinon on n'aura pas la compatibilité x86
;; TODO !!!


;; lisp2li simpliste pour le compilateur. On fusionnera les deux plus tard.

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

;; (defmatch squash-lisp)

;; (defmatch squash-lisp (:num . (? numberp)) `(:const . ,num))
;; (defmatch squash-lisp (:str . (? stringp)) `(:const . ,str))
;; (defmatch squash-lisp (quote :val _)       `(:const . ,val))
;; (defmatch squash-lisp ()                   `(:const . nil))
;; (defmatch squash-lisp (let ((:name $ :value _)*) :body _*)
;;   `(:let ,name ,value ,body))
;; (defmatch squash-lisp (:name _ :params _*) `(:call ,name ,@(mapcar #'squash-lisp params)))
;; (defmatch squash-lisp (:x . _)             (error "Squash-Lisp ne sait pas gérer : ~w" x))

#|
Notes sur l'implémentation d'unwind.
Tous les lets qui aparaissent dans un appel de fonction sont regrouppés en un seul. Donc à un appel de fonction correspond un "gros" segment de pile.
Après la mise en place de ce segment de pile, le code est exécuté.

TODO / NOTE pour la suite : on peut se passer des marker-* si on peut s'assurer qu'entre un end-frame et le begin-frame qui suit, il n'y a QUE des unwind-protect, unwind-catch etc.
Donc uniquement des adresses de portions de code généré par le compilateur, pas de "vrais" objets, donc on est sûr qu'il n'y aura pas de confusion entre un "vrai" objet
et une cible mise en place par unwind-catch.

Lorsqu'on rencontre une structure de contrôle comme la suivante :
(unwind-catch object body [catch-code]?)

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
[compile catch-code]   ;; seulement si catch-code est présent
@after-catch-code      ;; seulement si catch-code est présent

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

(half-unwind object post-unwind-code) est compilé ainsi :
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

(defun squash-lisp-1 (expr &optional (at-toplevel t) (etat (list nil nil nil)))
  (cond-match
   expr
   ;; - Si on rencontre une macro définie dans l'environnement de compiler-meval,
   ;;   1) On demande à compiler-meval d'expanser la macro sur un niveau.
   ;;   2) On re-lance la transformation (eval-when / defmacro / appel de macro / ...) sur le résultat s'il y a a eu expansion.
   ((:name $$ :params _*)
    (let ((definition (assoc-etat name 'macro etat)))
      (if definition
          (squash-lisp-1 (apply (cdr definition) params) at-toplevel etat)
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
        (squash-lisp-1 body at-toplevel etat)
        (squash-lisp-1 nil at-toplevel etat))) ;; on renvoie nil
   
   ;; - Si on rencontre un defmacro (au toplevel ou ailleurs).
   ;;   - On demande à compiler-meval de l'exécuter.
   ((defmacro :name $ :lambda-list @ :body _*)
    (mini-meval expr etat)
    (squash-lisp-1 nil at-toplevel etat)) ;; on renvoie nil
   
   ;; - Si on rencontre un macrolet
   ;;   - On fait une copie de l'état de compiler-meval
   ;;   - On lui demande d'exécuter les définitions
   ;;   - On évalue le body avec ce nouvel état
   ;;   - On continue avec l'ancien état
   ((macrolet :definitions ((:name $ :lambda-list @ :mbody _*)*) :body _*)
    (let ((get-etat (make-symbol "GET-ETAT")))
      (squash-lisp-1
       `(progn ,@body)
       at-toplevel
       (mini-meval `(macrolet ,definitions ,get-etat) (push-local etat 'trapdoor 'squash-trapdoor get-etat)))))
   
   ;; - Si on gère le symbol-macrolet
   ;;   - Le fonctionnement est le même que pour le macrolet
   ;;   - Lorsqu'on rencontre un symbole, on regarde s'il a une définition de type symbol-macrolet
   ((symbol-macrolet . _)
    (error "squash-lisp-1 : Symbol-macrolet n'est pas implémenté."))
   
   ((progn :body _*)
    (cons 'progn (mapcar (lambda (form) (squash-lisp-1 form at-toplevel etat)) body)))
   
   ;; Lorsqu'on rentre dans un block, on met sur la pile un marqueur spécial avec un pointeur vers un objet créé à l'exécution.
   ((block :block-name $$ :body _*)
    (let ((retval-sym (make-symbol "RETVAL"))
          (block-id-sym (make-symbol "BLOCK-ID")))
      (squash-lisp-1
       `(let ((,retval-sym nil)
              ;; Il y a un peu de redondance, car block-id-sym
              ;; stocké dans le let et dans le unwind-catch
              (,block-id-sym (cons nil nil)))
          (unwind-catch ,block-id-sym
                        (progn ,@body))
          ,retval-sym)
       nil
       (push-local etat block-name 'squash-block-catch (cons block-id-sym retval-sym)))))
   
   ;; Les return-from <nom> qui sont accessibles lexicalement sont remplacés par un (unwind <l'objet>)
   ;; Unwind remonte la pile jusqu'à trouver le marqueur spécial, tout en exécutant les unwind-protect éventuels.
   ;; Si unwind ne trouve pas le marqueur et arrive en haut de la pile, il signale une erreur et termine le programme.
   ;; Sinon, l'exécution reprend après le block.
   ((return-from :block-name $$ :value _)
    (let ((association (assoc-etat block-name 'squash-block-catch etat)))
      (unless association (error "Squash-Lisp-1 : Can't return from block ~w, it is inexistant or not lexically apparent." block-name))
      (squash-lisp-1 `(progn (setq ,(cddr association) value)
                           (unwind ,(cadr association)))
                   nil etat)))
   
   
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
      (squash-lisp-1
       `(let ((,tagbody-id-sym (cons nil nil)))
          (unwind-catch ,tagbody-id-sym
                        (progn
                          ,@(progn (dolist (zone spliced-body)
                                     (push `(jump-label ,(car zone)) res) 
                                     (push `(progn ,@(cdr zone)) res))
                                   ;; (cdr (reverse …)) pour zapper le tout premier (jump-label …)
                                   (cdr (reverse res)))))
          nil)
       nil
       new-etat)))
   
   ((go :target $$)
    (let ((association (assoc-etat target 'squash-tagbody-catch etat)))
      (unless association (error "Squash-Lisp-1 : Can't go to label ~w, it is inexistant or not lexically apparent." target))
      (squash-lisp-1 `(progn (half-unwind ,(cadr association)
                                        (jump ,(cddr association))))
                   nil etat)))
   
   ;; Le traitement de catch/throw est similaire, sauf que le pointeur est simplement un pointeur vers l'objet utilisé pour le catch / throw.
   ((catch :tag _ :body _*)
    (squash-lisp-1
     ;; TODO : ajouter une variable globale singleton-catch-retval
     `(unwind-catch ,tag (progn ,@body) singleton-catch-retval)
     nil etat))

   ((throw :tag _ :result _)
    (squash-lisp-1
     `(progn (setq singleton-catch-retval value)
             (unwind ,tag (progn ,@result)))
     nil etat))
   
   ;; Simplification du unwind-protect
   ((unwind-protect :body _ :a-cleanup _ :other-cleanups _+)
    `(unwind-protect ,(squash-lisp-1 body nil etat)
       ,(squash-lisp-1 `(progn ,a-cleanup ,@other-cleanups) nil etat)))
   
   ((unwind-protect :body _ :a-cleanup _)
    `(unwind-protect ,(squash-lisp-1 body nil etat)
       ,(squash-lisp-1 a-cleanup nil etat)))
    
   ((unwind-catch :object _ :body _ :catch-code _?)
    (if catch-code
        `(unwind-catch ,(squash-lisp-1 object nil etat)
                       ,(squash-lisp-1 body nil etat)
                       ,(squash-lisp-1 (car catch-code) nil etat))
        `(unwind-catch ,(squash-lisp-1 object nil etat)
                       ,(squash-lisp-1 body nil etat))))
   
   ((unwind :object _)
    `(unwind ,(squash-lisp-1 object nil etat)))
   
   ((half-unwind :object _ :post-unwind-code _)
    `(half-unwind ,(squash-lisp-1 object nil etat) ,(squash-lisp-1 post-unwind-code nil etat)))
   
   ((jump-label :name _)
    expr)
   
   ((jump :dest _)
    expr)

   ;; Transformation des (let[*] (var1 var2 var3) …) en (let[*] ((var1 nil) (var2 nil) (var3 nil)) …)
   ((:type (? or (eq x 'let) (eq x 'let*)) :bindings (? and consp (find-if #'symbolp x)) :body . _)
    (squash-lisp-1 `(,type ,(mapcar (lambda (b) (if (consp b) b `(b nil))) bindings) ,@body)))
   
   ((let ((:name $$ :value _)*) :body _*)
    `(let ,(mapcar (lambda (n v) `(,n ,(squash-lisp-1 v nil etat))) name value)
       ,(squash-lisp-1 `(progn ,@body) nil etat)))
   
   ((let* ((:name $$ :value _)*) :body _*)
    `(let* ,(mapcar (lambda (n v) `(,n ,(squash-lisp-1 v nil etat))) name value)
       ,(squash-lisp-1 `(progn ,@body) nil etat)))
   
   ((flet ((:name $$ :params @ :fbody _*)*) :body _*)
    `(simple-flet ,@(mapcar (lambda (name params fbody)
                              (cons name (squash-lisp-1 `(lambda ,params (progn ,@fbody)) nil etat)))
                            name params fbody)
                  ,(squash-lisp-1 `(progn ,@body) nil etat)))
   
   ((labels ((:name $$ :params @ :fbody _*)*) :body _*)
    `(simple-labels ,@(mapcar (lambda (name params fbody)
                                (cons name (squash-lisp-1 `(lambda ,params (progn ,@fbody)) nil etat)))
                              name params fbody)
                    ,(squash-lisp-1 `(progn ,@body) nil etat)))
   
   ;; TODO : defun
   
   ;; TODO : simplifier la lambda-list.
   ((lambda :params _ :body _)
    `(lambda ,params ,(squash-lisp-1 body nil etat)))
   
   ((lambda :params _ :body _*)
    (squash-lisp-1 `(lambda ,params (progn ,@body)) nil etat))
   
   ((function :fun $$)
    expr)

   ((funcall :fun _ :params _*)
    `(funcall ,@(mapcar (lambda (x) (squash-lisp-1 x nil etat)) (cons fun params))))
   
   ;; TODO : apply
   
   ((:fun $$ :params _*)
    `(funcall (function ,fun) ,@(mapcar (lambda (x) (squash-lisp-1 x nil etat)) params)))
   
   ((quote _)
    expr)
   
   ((? or numberp stringp)
    `(quote ,expr))
   
   ((? symbolp)
    `(get-var ,expr))
   
   ;; TODO : nil et t devraient être des defconst
   (nil
    (quote nil))
   (_
    (error "squash-lisp-1: Not implemented yet : ~a" expr))))

(defun squash-lisp-1-check (expr)
  "Vérifie si expr est bien un résultat valable de squash-lisp-1.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 1.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (cond-match
   expr
   ((progn :body _*)
    (every #'squash-lisp-1-check body))
   ((unwind-protect :body _ :cleanup _)
    (and (squash-lisp-1-check body)
         (squash-lisp-1-check cleanup)))
   ((unwind-catch :object _ :body _ :catch-code _?)
    (and (squash-lisp-1-check object)
         (squash-lisp-1-check body)
         (if catch-code
             (squash-lisp-1-check (car catch-code))
             t)))
   ((unwind :object _)
    (squash-lisp-1-check object))
   ((half-unwind :object _ :post-unwind-code _)
    (and (squash-lisp-1-check object)
         (squash-lisp-1-check post-unwind-code)))
   ((jump-label :name _) ;; TODO : être plus précis que "_"
    t) ;; TODO : t ? ou récursion ?
   ((jump :dest _) ;; TODO : être plus précis que "_"
    t) ;; TODO : t ? ou récursion ?
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
    (error "squash-lisp-1-check: Assertion failed ! This should not be here : ~a" expr))))
;; captures = ((capture*)*)
;; env-var = (((nom-variable symbole-unique état (référence-lecture*) (référence-écriture*))*)*)
;; état = [nil == variable normale] || ['captured == variable capturée] || ['special == variable spéciale]
;; env-fun = ((nom-fonction . symbole-unique)*)

(defun squash-lisp-3 (expr &optional (captures (list nil)) (env-var (list nil)) env-fun special-vars)
  "Détecte les variables capturées, supprime les let, let*, flet, labels, lambda en les transformant en simple-let et simple-lambda."
  (cond-match
   expr
   
   ;; let et let*
   ((:type (? or (eq x 'let) (eq x 'let*)) ((:names $$ :values _)*) :body _)
    ;; => new-env-var := env-var
    (let ((new-env-var env-var)
          (simple-let-vars nil)
          (simple-let-backups nil)
          (simple-let-pre-body nil)
          (simple-let-body nil)
          (simple-let-restore nil)
          (set-expression)
          (unique-sym nil)
          (let* (eq type 'let*)))
      ;; => Pour chaque binding
      (dolist* ((n names) (v values))
               ;; => On crée un symbole unique pour représenter cette liaison
               (setq unique-sym (make-symbol (string n)))
               ;; => ajouter unique-sym dans le simple-let qu'on crée
               (push unique-sym simple-let-vars)
               
               (if (member n special-vars)
                   ;; => Si c'est une variable spéciale,
                   (progn
                     ;; => On garde le nom d'origine comme nom de variable, et on utilise le nom unique comme symbole de sauvegarde.
                     ;; => au tout début du body, avant les autres set, sauvegarder la variable
                     (push `(setq ,unique-sym ,n) simple-let-backups)
                     ;; => au début du body, set la variable avec (transform valeur (new- si let*)env-var env-fun)
                     (push `(setq ,n ,(squash-lisp-3 v captures (if let* new-env-var env-var) env-fun)) simple-let-pre-body)
                     ;; => à la fin du body (dans un unwind-protect), restaurer la variable
                     (push `(setq ,n ,unique-sym) simple-let-restore))
                   ;; => Sinon (variable "normale" ou futurement capturée),
                   (progn
                     ;; => au début du body, set la variable unique-sym avec (transform valeur (new- si let*)env-var env-fun)
                     (setq set-expression `(setq ,unique-sym ,(squash-lisp-3 v captures (if let* new-env-var env-var) env-fun)))
                     (push set-expression simple-let-pre-body)
                     ;; => push (nom unique-sym nil <pas-de-get> <set-expression>) sur new-env-var
                     (push `(,n ,unique-sym nil nil (,set-expression)) (car new-env-var)))))
      ;; => transforme le body dans new-env-var env-fun
      (setq simple-let-body (squash-lisp-3 body captures new-env-var env-fun))
      ;; => construit et renvoie le simple-let
      (if simple-let-restore
          `(simple-let ,simple-let-vars
                       (unwind-protect
                            (progn ,@simple-let-backups  ;; Ne peut / doit pas déclenger d'unwind
                                   ,@simple-let-pre-body ;; À partir d'ici on peut
                                   ,simple-let-body)
                         (progn ,@simple-let-restore)))
          `(simple-let ,simple-let-vars
                       (progn ,@simple-let-pre-body
                              ,simple-let-body)))))
   
   ;; flet et labels
   ((:type (? or (eq x 'flet) (eq x 'labels)) ((:names $ :values _)*) :body _)
    ;; => new-env-var := env-var
    ;; => new-env-fun := env-fun
    (let ((new-env-var env-var)
          (new-env-fun env-fun)
          (simple-let-vars nil)
          (simple-let-pre-body nil)
          (simple-let-body nil)
          (set-expression)
          (unique-sym nil)
          (labels (eq type 'labels)))
      ;; => Pour chaque binding
      (dolist* ((n names) (v values))
               ;; => On crée un symbole unique pour représenter cette liaison dans l'environnement des variables
               (setq unique-sym (make-symbol (string n)))
               ;; => ajouter unique-sym dans le simple-let qu'on crée
               (push unique-sym simple-let-vars)
               ;; => On push le unique-sym dans les variables : (unique-sym unique-sym nil <pas-de-get> <set-expression qui sera déterminé plus tard>)
               (setq set-expression (list 'setq unique-sym 'not-yet-defined))
               (push `(,unique-sym ,unique-sym nil nil (,set-expression)) (car new-env-var))
               ;; => push (nom . unique-sym) sur new-env-fun
               (push `(,n . ,unique-sym) new-env-fun)
               ;; => au début du body, set la variable unique-sym avec (transform <lambda> (new- si labels)env-var (new- si labels)env-fun)
               ;; + set sur le champ "valeur" du set-expression
               ;; Note : on marche sur de l'ether…
               (setf (third set-expression) (squash-lisp-3 v captures (if labels new-env-var env-var) (if labels new-env-fun env-fun)))
               (push set-expression simple-let-pre-body))
      ;; => On transforme le body dans new-env-var new-env-fun
      (setq simple-let-body (squash-lisp-3 body captures new-env-var new-env-fun))
      ;; => construit et renvoie le simple-let
      `(simple-let ,simple-let-vars
                   (progn ,@simple-let-pre-body
                          ,simple-let-body))))
   
   ;; lambda
   ;; Beaucoup de code dupliqué entre les let[*] / lambda / flet / labels
   ;; TODO : gérer le &rest
   ((lambda :params ($$*) :body _)
    (let ((simple-lambda-captures (list nil)))
      ;; Shift l'environnement courant en le remplaçant par un tout nouveau tout bô.
      (setq env-var (cons nil env-var))
      (push simple-lambda-captures captures)
      ;; Quand on capture, on ne sait pas si la variable sera déclarée spéciale plus tard.
      ;; Mais on a décidé (cf. les notes plus bas) de ne pas supporter la re-déclaration d'une variable comme spéciale.
      ;; Création du simple-lambda
      ;; TODO : insérer du code pour avoir les captures.
      ;; TODO : closure ? make-closure ? ???
      `(simple-lambda
        ,(length params)
        ,(squash-lisp-3
          `(let ,(loop
                    for i upfrom 1
                    for var in params
                    collect `(,var (get-param ,i)))
             ,body)))))
   
   ;; Appel de fonction
   ((funcall :fun _ :args _*)
    (cons 'funcall (mapcar (lambda (x) (squash-lisp-3 x captures env-var env-fun)) (cons fun args))))

   ;; TODO : apply ?
   
   ;; Référence à une fonction
   ((function :fun $$)
    (let ((association (assoc fun env-fun)))
      (unless association
        (setq association `(,fun . ,(make-symbol (string fun))))
        (push association env-fun))
      (squash-lisp-3 `(get-var ,(cdr association)) captures env-var env-fun)))

   ;; Progn
   ((progn :exprs _*)
    (cons 'progn (mapcar (lambda (x) (squash-lisp-3 x captures env-var env-fun)) exprs)))
   
   ;; Récupération d'un paramètre
   ((get-param (? numberp))
    expr)
   
   ;; Référence à une variable
   ;; (get-var var)
   ((:type (? or (eq x 'get-var) (eq x 'setq)) :var $$)
    (let ((resultat nil)
          (search-env-var env-var)
          (envs nil)
          (through-captures captures)
          (is-global nil)
          (variable nil)
          (setq (eq type 'setq)))
      ;; => resultat := (get-var var) ou (setq var (transform val …))
      (setq resultat (if setq
                         (list 'setq var (squash-lisp-3 val captures env-var env-fun))
                         (list 'get-var var)))
      ;; => chercher la définition de la variable.
      (tagbody
       search-loop
         (push (car search-env-var) envs)
       start
         (when (endp (cdr search-env-var))
           (setq is-global t))
         (setq variable (assoc expr (car search-env-var)))
         (unless variable
           (when (endp (cdr search-env-var))
             (go end))
           (setq search-env-var (cdr search-env-var))
           (go search-loop))
       end)
      ;; => Si la variable n'existe pas (globale donc)
      (when (not variable)
        (when (not is-global) (error "Assertion failed !!! La variable devrait être marquée comme globale apr le tagbody qu'on vient de passer.")) ;; DEBUG
        ;; => la pusher dans l'env-var le plus haut (last …) == search-env-var
        (if setq 
            (push (setq variable `(,var ,var nil nil resultat)) (car search-env-var))
            (push (setq variable `(,var ,var nil resultat nil)) (car search-env-var))))
      ;; => Si elle ne se trouve ni dans l'env-var local (car) ni dans l'env-var global (last), alors c'est une capture
      ;; => Autre possibilité : la variable est spéciale, on la traite alors comme si elle était non capturée.
      (if (not (or (length=1 envs) is-global (eq 'special (third variable))))
          (progn
            (if setq
                (setf (car resultat) 'setq-indirection)
                (setf (car resultat) 'get-var-indirection))
            ;; => si c'est une nouvelle capture
            (unless (eq (third variable) 'captured)
              ;; => Pour chaque environnement intermédiaire + l'env-var local,
              (dolist (e envs)
                ;; => On marque la variable comme capturée sur tous les niveaux entre sa déclaration et son utilisation
                (push-new var (car through-captures))
                (setq through-captures (cdr through-captures))
                ;; => On transforme tous les (get-var var) en (get-var-indirection var)
                (dolist (reference-get (fourth variable))
                  (setf (car reference-get) 'get-var-indirection))
                (setf (fourth variable) nil)
                ;; => On transforme tous les (setq var val) en (setq-indirection var val)
                (dolist (reference-set (fifth variable))
                  (setf (car reference-set) 'setq-indirection))
                (setf (fifth variable) nil))))
          ;; => Sinon, ce n'est pas (encore) une capture
          ;; => push resultat sur l'entrée de la variable dans env-var.
          (if setq
              (push resultat (fifth variable))
              (push resultat (fourth variable))))
      ;; renvoyer resultat
      resultat))
   ((quote _)
    expr)
   (_
    (error "squash-lisp-3: not implemented yet: ~a" expr)))) ;; end squash-lisp-3

(defun squash-lisp-3-check (expr)
  "Vérifie si expr est bien un résultat valable de squash-lisp-3.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 3.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (cond-match
   expr
   ((simple-let :vars ($$*) :body _)
    (every #'squash-lisp-3-check body))
   ((simple-lambda :nb-params (? numberp) :body _)
    ;; nb-params = sans compter le paramètre de closure.
    (every #' squash-lisp-3-check body))
   ((funcall :function _ :args _*)
    (every #'squash-lisp-3-check (cons function args)))
   ((progn :body _*)
    (every #'squash-lisp-3-check body))
   ((get-param (? numberp))
    t)
   ((setq :var $$ :val _)
    (squash-lisp-3-check val))
   ((get-var :var $$)
    t)
   ((setq-indirection :var $$ :val _)
    (squash-lisp-3-check val))
   ((get-var-indirection $$)
    t)
   ((quote :val _)
    t)
   (_
    (error "squash-lisp-3-check: Assertion failed ! This should not be here : ~a" expr))))

;; TODO : pouquoi les let de squash-lisp-3 sont à l'envers ?

(defun make-sql4-lambda (name nbargs slet-vars slet-body)
  ;; TODO reverse et append les slet-body et slet-vars
  `(named-lambda ,name ,nbargs (simple-let ,slet-vars (progn ,@slet-body))))

;; TODO : où mettre les globales ?
(defun squash-lisp-4 (expr &optional (main-fun (make-symbol "main")))
  (let ((stack nil)
        (slet-vars nil)
        (flat nil))
    (labels ((rec (expr)
               (cond-match
                expr
                ((simple-let :vars ($$*) :body _*)
                 (push vars slet-vars)
                 (rec body))
                ((simple-lambda :nb-params (? numberp) :body _*)
                 (let ((fun-name (make-symbol "a-function")))
                   ;; nb-params = sans compter le paramètre de closure.
                   ;; On push tout le monde
                   (push slet-vars stack)
                   ;; On raz pour un nouveau lambda
                   (setq slet-vars nil)
                   ;; On transforme le body : (rec body) ci-dessous,
                   ;; et on crée la lambda et on l'ajoute au grand flat.
                   ;; TODO : ajouter la liste de captures si nécessaire (?)
                   (push (make-sql4-lambda fun-name nb-params slet-vars (rec body)) flat)
                   ;; On réstaure tout le monde
                   (setq slet-vars (pop stack))))
                ((funcall :function _ :args _*)
                 `(funcall ,(rec function) ,@(mapcar #'rec args)))
                ((progn :body _*)
                 (every #'squash-lisp-3-check body))
                ((get-param (? numberp))
                 expr)
                ((setq :var $$ :val _)
                 `(setq ,var ,(rec val)))
                ((get-var :var $$)
                 `(get-var ,var))
                ((setq-indirection :var $$ :val _)
                 `(setq-indirection ,var ,(rec val)))
                ((get-var-indirection $$)
                 `(get-var-indirection ,var))
                ((quote :val _)
                 expr)
                (_
                 (error "squash-lisp-4: Not implemented yet : ~a" expr)))))
      (rec expr)
      flat)))

#|

;; Formes pouvant créer des variables capturables :
lambda
let
let* // let imbriqués
// progv // compliqué, pas très utile
flet // let pour les fonctions
labels // letrec pour les fonctions
macrolet // letrec pour les macros
// symbol-macrolet // compliqué, pas très utile

;; Formes pouvant capturer des variables :
lambda
defun => lambda

;; Comportement des variables globales et spéciales
- une variable qui n'est pas attachée lexicalement est globale
- une variable qui est déclarée speciale dans le cadre d'un let, defun, etc., est
  modifiée globallement par sa nouvelle valeur (celle du let / paramètre), puis
  sa valeur est restaurée à la fin du let / defun / ...
- une variable qui est globalement spéciale (comme c'est le cas pour les variables defvar)
  a une seule valeur partagée entre toutes ses utilisations. Autrement dit, partout
  où cette variable est lue ou modifiée, c'est la même valeur qui est utilisée.

(defvar x val)
=> (progn (proclaim '(special x))
          (unless (boundp 'x)
            (setq x val)))
(boundp var)
=> t si var _globale_ est bound (on s'en fiche de son état lexical).

;; Comportement des closures
Lorsqu'on fait une closure (à l'exécution donc), elle capture *toutes* les variables capturables de l'environnement.
Les variables capturées ont ensuite leur valeur partagée entre les différentes closures qui les utilisent et "l'extérieur" (le lieu de déclaration initial des variables).
Exemple :
(defun introspect () nil)
(defun make-closure (initial)
  (let ((a 1) (b 2) (c 3))
    (let ((closure-incf (lambda () (incf initial)))
          (closure-return (lambda () (introspect) initial)))
      (print initial)
      (funcall closure-incf)
      (print initial) ;; l'extérieur partage la même valeur
      (cons closure-incf closure-return))))
(setq cl1 (make-closure 1))
=> 1
=> 2
=> (#<closure...> . #<closure...>)
(setq cl42 (make-closure 42))
=> 42
=> 43
=> (#<closure...> . #<closure...>)
;; les valeurs sont partagées entre les closures créées sur les mêmes instances de variables
(funcall (car cl1))
=> 3
(funcall (cdr cl1))
=> 3
;; mais pas entre des closures créées à différents moments
(funcall (cdr cl42))
=> 43

Le comportement des fonctions et des macros expliqué ci-dessous permet de prouver que la capture s'effectue sur toutes les variables et non pas seulement celles qui paraissent être accessibles :
(defmacro introspect-closure (get-variable closure)
  `(progn (defmacro introspect () '(print ,get-variable))
          (funcall ,closure)
          (defun introspect () nil)))
(introspect-closure a (cdr cl1))
=> 1 ;; (print a)
=> 3
(introspect-closure b (cdr cl1))
=> 2 ;; (print b)
=> 3
(introspect-closure c (cdr cl1))
=> 3 ;; (print c)
=> 3
(introspect-closure initial (cdr cl1))
=> 3 ;; (print intitial)
=> 3

Un autre moyen de le vérifier est de mettre dans le let ((a 1) (b 2) (c 3)) un autre variable, non utilisée, qui est associée à une très grosse liste (un million d'éléments).
Après avoir créé une vingtaine de closures, on voit dans "top" que clisp occupe environ 90 Mo de RAM, alors qu'auparavent il n'en occupait que très peu.
Pourtant ces listes d'un million d'éléments semblent inaccessibles, sauf par notre trucage introspect-closure.

;; Comportement des fonctions et des macros
Si une macro est rencontrée, elle est expansée
Si un appel de fonction est rencontré, la fonction est appellée telle qu'elle
Si une fonction est redéfinie en tant que macro, tous les appels de fonction qui lui correspondent sont transformés en appels de macro (expansion à la volée).
On peut alors redéfinir la macro en macro ou en fonction, au choix, plusieurs fois, les appels suivent "intuitivement". (Ça existe encore ça l'intuition ?)
Si une macro "rencontrée initialement" est redéfinie en tant que fonction, les appels qui ont déjà été "expansés initialement" ne sont pas redéfinis.
Dans la structure suivante, la règle du "rencontrée initialement" est bien appliquée, la macro n'est pas ré-expansée :
(defmacro mcr (x) `(list ',x 'y))
(defun bar-maker () (defun bar () (mcr a)))
(bar-maker)
(bar)
=> (a y)
(defmacro mcr (x) `(list ',x 'z))
(bar)
=> (a y)
(bar-maker)
(bar)
=> (a y)

;; Décision

Pour des raisons de santé mentale, d'efficacité et d'alignement des planètes, nous ne supporterons pas la redéfinition de fonctions en tant que macros.
De même, si une macro est utilisée avant sa définition, elle ne sera pas expansée, et déclenchera probablement une erreur "undefined function".
Et pour simplifier la compilation, toutes les définitions de macros seront prises en compte,
qu'elles soient à l'intérieur d'un if, d'un defun, d'un let... sans prendre en compte leur environnement.

;; Fonctionnement des block et tagbody
Les noms de blocs sont un peu comme des variables, ils sont capturés par les closures.
Ainsi, dans l'exemple suivant, lorsque le (return-from a) est apellé, on sort directement
du premier bloc a dans la pile d'appels.

(defun foo (fn n)
  (format t "~&foo ~a ~a" n fn)
  (block a
    (bar (if fn fn (lambda (x)
                     (format t "~&lambda ~a" x)
                     (if (<= x 0) (return-from a 42))))
         n))
  (format t "~&foo2"))

(defun bar (fn n)
  (format t "~&bar ~a ~a" n fn)
  (funcall fn n)
  (foo fn (- n 1))
  (format t "~&bar2"))

;; Choix d'implémentation des block / return-from, tagbody / go, catch / throw

Lorsqu'on rentre dans un block, on met sur la pile un marqueur spécial avec un pointeur vers un objet créé à l'exécution.
Les return-from <nom> qui sont accessibles lexicalement sont remplacés par un (unwind <l'objet>)
Unwind remonte la pile jusqu'à trouver le marqueur spécial, tout en exécutant les unwind-protect éventuels.
Si unwind ne trouve pas le marqueur et arrive en haut de la pile, il signale une erreur et termine le programme.
Sinon, l'exécution reprend après le block.
Le traitement de tagbody/go est similaire pour sortir d'un tag, puis on jmp directement sur le tag de destination (vu qu'il est au même niveau).
Le traitement de catch/throw est similaire, sauf que le pointeur est simplement un pointeur vers l'objet utilisé pour le catch / throw.
À noter que la liaison lexicale pour le block et le tagbody est à effectuer avant de sortir éventuellement des lambdas anonymes de leur fonction englobante.
(comme la liaison lexicale ne s'effectue pas sur des variables, cette transformation ne présèrverait pas la liaison).
Cette étape doit s'effectuer après l'expansion de macros, sinon on risque de louper des block / return-from / ... .

;; Choix d'implémentation des closures :
Lorsqu'une variable est capturée, on ne peut pas copier directement l'adresse dans le tas de sa valeur pour pouvoir la manipuler,
car il se peut que l'on doive déplacer la valeur si on souhaite la remplacer par quelque chose de plus gros (par ex. remplacer un nombre par une string).
On est donc obligé d'avoir un pointeur d'indirection supplémentaire, de taille fixe, qui ne sera pas déplacé.
Ce pointeur est ce qu'on appellera la closure-cell.

Lorsqu'on rencontre un binding d'une variable (let, labels, lambda, ...), on regarde si elle est capturée à l'intérieur
du corps du special-form qui effectue la liaison.
Si c'est le cas, on crée une closure-cell, qui contient un pointeur vers l'endroit où est stockée la vraie valeur de la variable,
et à chaque lecture / écriture de la variable, on utilise (get-closure-cell-value <cl-cell>) à la place.
Ceci doit s'effectuer après l'expansion de macros, sinon on risque de louper des noms de variable capturées écrits par les macros.
Chaque lambda capturant des variables est ensuite modifié de manière à prendre les closure-cell des variables capturées en paramètre.
Il est "emballé" par une sorte de forme spéciale "closure", qui contient la liste des closure-cell à passer en paramètres à la lambda.
Il n'y a alors plus de closures dans le sens où toutes les variables capturées le sont explicitement, et sont passées en paramètre.
On peut donc "sortir" toutes les closures de leur environnement englobant, en les transformant en defuns nommés avec un symbole unique
généré avec make-symbol. La lambda elle-même est alors remplacée par (closure symbole-unique-du-defun closure-cell*).

TODO : revoir les choix d'implémentation des closures après une nuit de someil...
Le but est de ne plus avoir aucun lambda imbriqué dans quoi que ce soit
(tous les lambdas doivent être au top-level, juste emballés par un truc qui les nomme).

;; Implémentation des let, let*, flet, labels, macrolet, ...
Vu que tous les lambda ont été ramenés au top-level, il n'y a plus de capture de variables.
Les let sont donc :
 - À l'intérieur d'un lambda, quelque part
 - À l'intérieur d'un autre let qui est au top-level
 - À l'intérieur d'un progn qui est au top-level
 - Directement au top-level
Les trois derniers cas peuvent être ramenés au premier en les emballant avec un lambda sans paramètres
On peut alors "applatir" tous les let imbriqués dans un lambda dans la liste de paramètres du lambda.

Au top-level, on aura donc uniquement des lambda nommés, avec ou sans paramètres, qui ne contiendront ni lambda ni aucune forme de let.
Il n'y aura plus de macros.
Plus de block ni tagbody, donc pas de liaison lexicale à ce niveau.

Voici la liste des special-form.
block                   OK
catch                   OK
declare                 -- ?
eval-when              *OK Avant/pendant macro-expansion
flet                   *OK
function                -- ?
generic-flet            ~~ Non implémenté
generic-labels          ~~ Non implémenté
go                      OK
if                      -- À compiler
labels                 *OK
let                    *OK
let*                   *OK
macrolet               *OK
multiple-value-call     ~~ Non implémenté
multiple-value-prog1    ~~ Non implémenté (mais le serait avec une macro)
progn                  *OK (un seul géant qui représente le top-level)
progv                   ~~ Non implémenté
quote                   -- À compiler
return-from             OK
setq                    -- À compiler
symbol-macrolet         ~~ Non implémenté (peut-être ?)
tagbody                 OK
the                     ~~ Non implémenté, transformé ainsi : (the type form) => form
throw                   OK
unwind-protect          -- À compiler (ou bien macro-expansé en termes de "asm")
with-added-methors      ~~ Non implémenté

Les "formes spéciales du compilo" suivantes ont été rajoutées :
asm                     -- À compiler
unwind                  -- À compiler (ou bien macro-expansé en termes de "asm")
closure                 -- À compiler
+ les appels de lambdas nommés. -- À compiler

;; Implémentation des macros et de eval-when
Lors de la compilation d'un fichier, son top-level est traversé de la manière suivante :
On crée une instance de compiler-meval, un mini-meval qui renvoie toujours
un cons de la valeur de retour et de son état, pour qu'on puisse le rappeler.
compiler-meval transforme le eval-when en progn si sa situation contient :execute, en nil sinon.

NOTE : lorsqu'on rencontre la macro declaim au top-level, la proclamation est prise en compte.

| - Si on rencontre un defmacro (au toplevel ou ailleurs).
|   - On demande à compiler-meval de l'exécuter.
| - Si on rencontre EVAL-WHEN,
|   - Au top-level,
|     - Pour chaque form du body,
|       - Si la situation contient :compile-toplevel, le form est évalué dans compiler-meval.
|       - Si la situation contient :load-toplevel, le form est compilé (après évaluation dans compiler-meval s'il y a lieu).
|       - Lorsqu'un eval-when au top-level contient des eval-when directement sous lui, ils sont traités comme s'ils étaient directement au top-level.
|   - Ailleurs
|     - Si la situation contient :load-toplevel, le eval-when est remplacé par son body (TODO : À VÉRIVFIER !).
| - Si on rencontre un macrolet
|   - On fait une copie de l'état de compiler-meval
|   - On lui demande d'exécuter les définitions
|   - On évalue le body avec ce nouvel état
|   - On continue avec l'ancien état
| - Si on gère le symbol-macrolet
|   - Le fonctionnement est le même que pour le macrolet
|   - Lorsqu'on rencontre un symbole, on regarde s'il a une définition de type symbol-macrolet
| - Si on rencontre une macro définie dans l'environnement de compiler-meval,
|   1) On demande à compiler-meval d'expanser la macro sur un niveau.
|   2) On re-lance la transformation (eval-when / defmacro / appel de macro / ...) sur le résultat s'il y a a eu expansion.
| - S'occuper du cas du lambda et des autres mot-clés bizarres (ne pas faire de macro-expansion dessus).
| - Dans les autres cas, on transforme récursivement l'expression.

;; Comportement des variables globales et spéciales
Lorsqu'une variable est utilisée mais ne correspond à aucune liaison (établie par let, …), cette utilisation fait référence
à une liaison "globale" de cette variable (autrement dit, la valeur est partagée entre toutes les utilisations sans liaison).
Par défaut, une variable globale est "unbound", et n'a donc pas de valeur. La lecture d'une variable unbound est une erreur.
x
=> erreur
(defun bar () x)
(bar)
=> erreur
(setq x 3)
(bar)
=> 3

Lorsqu'une variable est déclarée localement spéciale, avec (declare (special nom-de-variable)) au début d'un defun, d'un let etc.,
la valeur de la variable est alors la même que la valeur globale.
Ce comportement spécial s'applique là où la variable est dans la portée lexicale de la forme spéciale englobant le declare (donc uniquement dans le defun, let, …).

(defun baz () y)
(let ((y 3)) (let ((z 1)) (declare (special y)) (setq y 42) (baz)))
=> 42 ;; Bien que y soit une liaison lexicale à cause du (let ((y 3)), le (special y) le transforme en variable globale.
y
=> 42

Si la forme spéciale englobante devait créer une liaison lecicale pour cette variable, elle ne le fait pas, mais à la place,
 - la valeur globale d'origine de la variable est sauvegardée,
 - sa valeur globale est modifiée avec la nouvelle valeur (paramètre effectif pour un defun, valeur de droite pour un let, ...)
 - La variable devient boundp si elle ne l'était pas déjà
 - le corps est exécuté, avec la variable partageant sa valeur avec la varaible globale
 - la valeur d'origine de la variable globale est restaurée.
 - Si la valeur était unbound, elle redevient unbound.

(defun quux () (print (boundp 'w)) w)
(boundp 'w)
=> NIL
(quux)
=> erreur
(let ((w 3)) (declare (special w)) (print (boundp 'w)) (quux))
=> T ;; boundp
=> T ;; boundp
=> 3
(boundp 'w)
(quux)
=> erreur ;; La valeur est bien restaurée.

Lorsqu'on effectue un (defvar var val), var devient globalement spéciale : toutes les utilisations de var pointent vers la même valeur,
y compris les utilisations effectuées avant le defvar.
(defun foo1 () var)
(defun foo2 () (let ((var 4)) (print var) (foo1)))
var
=> erreur
(foo1)
=> erreur
(foo2)
=> 4
=> erreur
(defvar var 123)
var
=> 123
(foo1)
=> 123
(foo2)
=> 4
=> 4

Lors du defvar, si la variable est boundp, sa valeur était conservée, sinon sa valeur globale devient la valeur spécifiée par le defvar.
Notemment, si le defvar apparaît à l'intérieur d'un let-special qui rend la variable boundp locallement, sa valeur globale sera restaurée à unbound à la sortie du let.
(defun get-1 () not-boundp)
(defun get-2 () is-boundp)
(defun get-3 () locally-boundp)
(defun getlet-1 () (let ((not-boundp 123))     (get-1)))
(defun getlet-2 () (let ((is-boundp 123))      (get-2)))
(defun getlet-3 () (let ((locally-boundp 123)) (get-3)))
(setq is-boundp 42)
(get-1)    => error ;; not-boundp
(get-2)    => 42    ;; is-boundp
(get-3)    => error ;; locally-boundp
(getlet-1) => error ;; not-boundp
(getlet-2) => 42    ;; is-boundp
(getlet-3) => error ;; locally-boundp

(defvar not-boundp 3)
(defvar is-boundp  3)
(let ((locally-boundp 42))
  (declare (special locally-boundp))
  (defvar locally-boundp 3))
(get-1)    => 3     ;; not-boundp
(get-2)    => 42    ;; is-boundp
(get-3)    => error ;; locally-boundp
;; La variable est maintenant spéciale partout :
(getlet-1) => 123   ;; not-boundp
(getlet-2) => 123   ;; is-boundp
(getlet-3) => 123   ;; locally-boundp

;; Implémentation des variables globales et spéciales

Pour les mêmes raisons de santé d'esprit et d'efficacité et d'alignement des planètes que pour la redéclaration de fonctions en macros, nous ne supporterons pas la redéclaration
de variables non spéciales en spéciales. Ainsi, si un defvar apparaît *après* des utilisations non spéciales de la variable, ces utilisations resteront non spéciales.

Lorsqu'une variable est détectée comme étant spéciale (soit globalement, avec defvar, soit localement, avec declare), sa valeur est stockée dans une global-cell,
qui ressemble comme deux goutes d'eau à une closure-cell, et toutes ces utilisations passent par la globla-cell, comme pour les variables capturées.
On est obligé d'avoir ce niveau d'indirection pour les mêmes raisons que pour le closure-cell.
La différence avec une closure-cell est qu'il n'y a qu'une seule global-cell par variable, qui est créée à la compilation.
De même, l'utilisation globale d'une variable de manière globale est remplacée par une référence à sa global-cell.
De plus, les formes spéciales qui devaient créer une liaison locale sont transformées comme suit :
(let ((x 3)) (declare (special x)) (setq x 42) x)
Est transformé en :
== En-tête
[global-cell x]
#<unbound>
== Code
[push [get-global-cell-value x]]
[set-global-cell-value x 3]
[set-global-cell-value x 42]
[get-global-cell-value x]
[set-global-cell-value x [pop]]

|#

(provide 'squash-lisp)