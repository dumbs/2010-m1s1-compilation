(require 'mini-meval "mini-meval")
(require 'match "match")

;; À la fin du fichier se trouvent des notes sur la compilation d'unwind & co.

;; TODO !!! Utiliser une pile descendante,
;; TODO !!! donc adapter les calculs pour unwind,
;; TODO !!! sinon on n'aura pas la compatibilité x86

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

(defun squash-lisp-1 (expr &optional (at-toplevel t) (etat (list nil nil nil)))
  "Supprime les macros, eval-when, tagbody/go, throw/catch, block/return-from,
   transforme les appels de fonction en funcall, les constantes en quote
   et simplifie pas mal d'autres choses."
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
   
   ((if :condition _ :si-vrai _ :si-faux _?)
    `(if ,(squash-lisp-1 condition nil etat)
         ,(squash-lisp-1 si-vrai nil etat)
         ,(squash-lisp-1 (car si-faux) nil etat)))
   
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
                        (progn ,@body)
                        nil)
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
      (squash-lisp-1 `(progn (unwind-for-tagbody ,(cadr association)
                                        (jump ,(cddr association))))
                   nil etat)))
   
   ;; Le traitement de catch/throw est similaire, sauf que le pointeur est simplement un pointeur vers l'objet utilisé pour le catch / throw.
   ((catch :tag _ :body _*)
    (squash-lisp-1
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
    
   ((:type (? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body _ :catch-code _)
    `(,type ,(squash-lisp-1 object nil etat)
            ,(squash-lisp-1 body nil etat)
            ,(squash-lisp-1 catch-code nil etat)))
   
   ((unwind :object _)
    `(unwind ,(squash-lisp-1 object nil etat)))
   
   ((unwind-for-tagbody :object _ :post-unwind-code _)
    `(unwind-for-tagbody ,(squash-lisp-1 object nil etat) ,(squash-lisp-1 post-unwind-code nil etat)))
   
   ((jump-label :name $$)
    expr)
   
   ((jump :dest $$)
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
    `(simple-flet ,(mapcar (lambda (name params fbody)
                             (list name (squash-lisp-1 `(lambda ,params (progn ,@fbody)) nil etat)))
                           name params fbody)
                  ,(squash-lisp-1 `(progn ,@body) nil etat)))
   
   ((labels ((:name $$ :params @ :fbody _*)*) :body _*)
    `(simple-labels ,(mapcar (lambda (name params fbody)
                               (list name (squash-lisp-1 `(lambda ,params (progn ,@fbody)) nil etat)))
                             name params fbody)
                    ,(squash-lisp-1 `(progn ,@body) nil etat)))
   
   ;; TODO : defun
   ;; TODO : defvar
   ;; => TODO : global-setq
   ;; => TODO : global-setfun
   ;; => TODO : proclaim
   
   ;; TODO: simplifier la lambda-list.
   ((lambda :params _ :body _)
    `(lambda ,params ,(squash-lisp-1 body nil etat)))
   
   ((lambda :params _ :body _*)
    (squash-lisp-1 `(lambda ,params (progn ,@body)) nil etat))
   
   ((function :fun (lambda . _))
    (squash-lisp-1 fun nil etat))
   
   ((function :fun $$)
    expr)

   ((funcall :fun _ :params _*)
    `(funcall ,@(mapcar (lambda (x) (squash-lisp-1 x nil etat)) (cons fun params))))
   
   ;; TODO : apply
   ;; => TODO : définir la fonction funcall : (funcall #'funcall #'cons 1 2)
   ;; => TODO : définir la fonction apply   : (funcall #'apply #'cons '(1 2))
   
   ((setq :name $$ :value _)
    `(setq ,name ,(squash-lisp-1 value)))
   
   ((quote _)
    expr)
   
   ((? or numberp stringp)
    `(quote ,expr))
   
   ;; TODO : nil et t devraient être des defconst
   ;; Doit être avant les symboles
   (nil
    (quote nil))
   
   ($$
    `(get-var ,expr))
   
   ;; Appels de fonction
   ;; Doivent être après tout le monde.
   ((:fun $$ :params _*)
    (squash-lisp-1 `(funcall (function ,fun) ,@params)))
   
   ((:lambda (lambda . _) :params _*)
    (squash-lisp-1 `(funcall ,lambda ,@params)))
   
   (((function :lambda (lambda . _)) :params . _)
    (squash-lisp-1 `(funcall ,lambda ,@params)))
   
   (((function :name $$) :params _*)
    (squash-lisp-1 `(funcall (function ,name) ,@params)))
   
   (_
    (error "squash-lisp-1: Not implemented yet : ~a" expr))))

(defun squash-lisp-1-wrap (expr)
  `(macrolet ((unwind-catch (object body catch-code)
                (let ((bname (make-symbol "block")))
                  `(block ,bname
                     (catch ,object (return-from ,bname ,body))
                     ,catch-code
                     nil)))
              (tagbody-unwind-catch (object body catch-code)
                catch-code ;; unused variable
                ;; les (progn object) et (progn x) sert à permettre l'expansion des macros sur x
                ;; (il est possible qu'elles ne soient pas expansées à la racine du tagbody)
                `(tagbody (progn ,object) ,@(mapcar (lambda (x) (if (eq (car x) 'jump-label) (cadr x) (progn x))) (cdr body))))
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
              (simple-flet (spec &rest body)
                `(flet ,(mapcar (lambda (x) (list (car x) (second (second x)) (third (second x)))) spec) ;; nom, lambda-list, fbody
                   ,@body))
              (simple-labels (spec &rest body)
                `(labels ,(mapcar (lambda (x) (list (car x) (second (second x)) (third (second x)))) spec) ;; nom, lambda-list, fbody
                   ,@body))
              (get-var (x)
                x))
     ,expr))

(eval (squash-lisp-1-wrap
       '(unwind-catch 'foo
         (progn (print 1)
                (unwind 'foo)
                (print 2))
         (print 3))))

(eval (squash-lisp-1-wrap (squash-lisp-1 '(flet ((foo (x) (+ x 1)) (bar (y z) (cons y z))) (list (foo 3) (bar 4 5))))))

(eval (squash-lisp-1-wrap (squash-lisp-1 '(tagbody a 1 (print 'x) b (print 'y) (go 3) d (print 'z) 3 (print 'f)))))

;; 
;; (squash-lisp-1 '(flet ((foo (x) (+ x 1)) (bar (y z) (cons y z))) (list (foo 3) (bar 4 5))))


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
   ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
   (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body _ :catch-code _)
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
   (((? (member x '(let let* simple-flet simple-labels))) ((:name $$ :value _)*) :body _)
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
   ((setq :name $$ :value _)
    (squash-lisp-1-check value))
   (_
    (warn "squash-lisp-1-check: Assertion failed ! This should not be here : ~a" expr)
    nil)))

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
