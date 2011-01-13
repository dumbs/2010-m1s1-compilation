(require 'match "match")
(require 'util "util")
(require 'squash-lisp "implementation/squash-lisp")

;; TODO !! ATTENTION !! Quand onc récupère des données qui font 1 octet de large, en fait on récupère 4 octets !

;; TODO !! ATTENTION !! Tout multiplier par 4 (octets)

;; TODO ! chercher "sens de la soustraction" dans ce fichier

(defvar *asm-sections* '(code data)) ;; Note : l'assembleur sera produit dans cet ordre

(defvar *label-ctr* 0)

(defvar *sys-labels*)
(defun syslabel (label)
  `(label ,@(assoc-or-push label (list (derived-symbol label) (incf *label-ctr*)) *sys-labels*)))

(defvar *code-labels*)
(defun code-label (label)
  `(label ,@(assoc-or-push label (list (derived-symbol label) (incf *label-ctr*)) *code-labels*)))

(defvar *global-labels*)
(defun global-label (label)
  (assoc-or-push label (list (list (derived-symbol label) (incf *label-ctr*))
                             (list (derived-symbol label) (incf *label-ctr*))
                             (list (derived-symbol label) (incf *label-ctr*)))
                 *global-labels*))

(defun global-label-symbol   (label) `(label ,@(car (global-label label))))
(defun global-label-variable (label) `(label ,@(cadr (global-label label))))
(defun global-label-function (label) `(label ,@(caddr (global-label label))))

(defvar *res-asm-constants* nil)

(defun type-number (type)
  (position type '(captured-var fixnum bignum symbol string cons nil)))

(defun error-code (err)
  (position err '(normal-exit
                  unwind-for-tagbody--doit-contenir-un-jump)))

(defun assembly-label-or-number-p (expr)
  (or (numberp expr)
      (match (label $$ $n) expr)))

(defun immutable-assembly-place-p (expr)
  (match (constant     (? assembly-label-or-number-p)) expr))

(defun mutable-assembly-place-p (expr)
  (cond-match
   expr
   ((register          (? (member x '(r0 r1 r2 sp bp fp))))    t)
   ((constant          (? assembly-label-or-number-p))         t)
   ((memory            (? assembly-label-or-number-p))         t)
   ((indexed           (? assembly-label-or-number-p))         t)
   ((indirect-register (? (member x '(r0 r1 r2 sp bp fp))))    t)
   ((indirect-constant (? assembly-label-or-number-p))         t)
   ((indirect-indexed  $n (? (member x '(r0 r1 r2 sp bp fp)))) t)))

(defun assembly-place-p (expr)
  (or (immutable-assembly-place-p expr)
      (mutable-assembly-place-p expr)))

(defun assembly-instruction-p (expr)
  (cond-match
   expr
   ((mov :src $ap :dst $map) t)
   ((push :src $ap) t)
   ((pop :dst $map) t)
   ((jmp :to $ap) t)
   ((add :src $ap :dst $map) t)
   ((sub :src $ap :dst $map) t)
   ((call :fun $ap) t)))

(defun compilo-check (asm)
  (let ((non-empty nil)
        (etiquettes nil)
        (res nil))
    ;; TODO : vérification de l'existance des étiquettes
    (labels ((compilo-check-1 (asm)
               (cond-match
                asm
                ((section (? $$ (member x *asm-sections*)) :body . @)
                 (every #'compilo-check-1 body))
                ((label :l $$ :n $n)
                 (push n etiquettes)
                 t)
                (_
                 (if (assembly-instruction-p asm)
                     (progn
                       (setq non-empty t)
                       t)
                     (progn
                       (warn "compilo-check : this should not be here : ~a" asm)
                       nil))))))
      (setq res (compilo-check-1 asm)))
    (unless non-empty
      (warn "compilo-check : Le code assembleur est vide ! Il n'y a aucune instruction.")
      (setq res nil))
    (unless (match (section (? $$ (member x *asm-sections*)) . @) asm)
      (warn "compilo-check : malformed top-level assembly structure.")
      (setq res nil))
    res))

(defun flatten-asm (asm)
  (let ((res (mapcar #'list *asm-sections*))
        (current nil))
    (labels ((flatten-asm-1 (asm)
               (cond-match
                asm
                ((section :sec $$ :body)
                 (setq current (assoc sec res))
                 (unless current
                   (error "flatten-asm : invalid section : ~a" sec)))
                ((:mnemonique $$ :args $ap*)
                 (push asm current)))))
      (flatten-asm-1 asm))
    (apply #'append (mapcar (lambda (x) (cons (list 'section (car x)) (reverse (cdr x)))) res))))

(defmacro with-label (l* &rest body)
  `(let ,(mapcar (lambda (l) `(,(car l) (code-label (make-symbol (string ,(cdr l)))))) l*)
     ,@body))

(defun compilo-init (main)
  `(section code
            ;; TODO : variables : top-heap max-top-heap bottom-stack min-bottom-stack nil
            ;; démarrer avec un bottom-stack = 1k (on se réserve une petite marge pour les fuites mémoire :) + push & pop temporaires).
            ;; TODO : fonctions : (do-gc param;r0=taille heap nécessaire, doit conserver tous les registres à l'identique.)
            ;; TODO : fonctions : (do-gc-redim-heap param:r0=supplément stack à allouer (au minimum). Doit préserver r1 (ou bien on peut affecter bottom-stack dans cette fonction))
            ;; TODO : root-set-gc (la liste de symboles, principalement).
            (push ,(syslabel nil)) ;; closure
            (push ,(syslabel nil)) ;; paramètres
            (push (register ip))
            (jmp ,(code-label main))
            ,(syslabel 'end-halt)
            (mov (constant ,(error-code 'normal-exit)) (register r0))
            (halt))) ;; TODO : dépendant de vm / os

(defun compilo-alloc-tas (size)
  "param:r1=taille à allouer, «returns» allocated adress in r0, clobbers r1"
  (with-label ((l-do-gc 'jump-dest-do-gc)
               (l-alloc))
    `(section code
              ,(syslabel 'alloc-tas)
              (push (register r2))
              (mov (memory ,(syslabel 'top-heap)) (register r0))
              (add (register r1) (register r0))
              (mov (memory ,(syslabel 'max-top-heap)) (register r2))
              (cmp (register r0) (register r2))
              (jpp (constant ,l-alloc))
              ,l-do-gc
              (push ip)
              (jmp (constant ,(syslabel do-gc)))
              ,l-alloc
              (mov (register r0) (memory ,(syslabel 'top-heap)))
              (sub (register r1) (register r0)) ;; sens de la soustraction
              (pop (register r2))
              (pop (register r1))
              (jmp (register r1)))))

(defun compilo-alloc-pile (size)
  "param:r0=size, «returns» nothing, clobbers r0"
  (with-label ((l-do-gc 'jump-dest-do-gc)
               (l-alloc))
    `(section code
              ,(syslabel 'alloc-pile)
              (push (register r1))
              (push (register r2))
              (mov (memory ,(syslabel 'bottom-stack)) (register r1))
              (sub (register r0) (register r1)) ;; sens de la soustraction
              (mov (memory ,(syslabel 'min-bottom-stack)) (register r2))
              (cmp (register r1) (register r2))
              (jpg (constant ,l-alloc))
              ,l-do-gc
              (push ip)
              (jmp (constant ,(syslabel do-gc-redim-heap)))
              ,l-alloc
              (mov (register r1) (memory ,(syslabel 'bottom-stack)))
              (pop (register r2))
              (pop (register r1))
              (pop (register r0))
              (jmp (register r0)))))

(defun db-type (type)
  `(db (constant ,(type-number type))))

(defvar *asm-fixnum-size* 32)
(defvar *asm-max-fixnum* (- (expt 2 *asm-fixnum-size*) 1))
(defun compilo-encode-constant (val)
  ;; TODO !
  (cond
   ;; fixnum
    ((and (numberp val) (<= val *asm-max-fixnum*))
     (with-label ((l 'fixnum-constant))
       (push (section data
                      ,l
                      (db-type 'fixnum)
                      (dl (constant num)))
             *res-asm-constants*)
       l))
   
   ;; bignum
   ((and (numberp val) (> val *asm-max-fixnum*))
    (with-label ((l 'bignum-constant))
      (push (section data
                     ,l
                     (db-type 'bignum)
                     ,@(let ((lst (split-bytes val (+ 1 *asm-fixnum-size*))))
                            (mapcar (lambda (x) `(dl (constant ,x)))
                                    (cons (length lst) lst))))
            *res-asm-constants*)
      l))
   
   ;; string
   ((stringp val)
    (with-label ((l 'string-constant))
      (push (section data
                     ,l
                     (db-type 'string)
                     (dl (constant ,(length val)))
                     ,@(map 'list (lambda (x) `(dl (constant ,(char-code x)))) val))
            *res-asm-constants*)
      l))
   
   ;; nil
   ((null val)
    (syslabel nil))
   
   ;; symbol
   ((symbolp val)
    (let ((l (global-label-symbol val)))
      (push (section data
                     ,l
                     (db-type 'symbol)
                     (dl (constant ,(compilo-encode-constant (string val)))) ;; pointeur vers nom du symbole
                     (dl (constant ,(syslabel nil))) ;; intern ? ;; TODO !!!!!!!
                     (dl (constant ,(syslabel nil))) ;; fdefinition ;; TODO
                     (dl (constant ,(syslabel nil))) ;; global value
                     (dl (constant ,(syslabel nil)))) ;; plist
            *res-asm-constants*)
      l))
   
   ;; array
   ((arrayp val)
    (with-label ((l 'cons-cell-constant))
      (push (section data
                     ,l
                     (db-type 'array)
                     (dl (constant ,(length val)))
                     ,@(map 'list (lambda (x) `(dl (constant ,(compilo-encode-constant x)))) val))
            *res-asm-constants*)
      l))
   
   ;; cons
   ((consp val)
    (with-label ((l 'cons-cell-constant))
      (push (section data
                     ,l
                     (db-type 'cons)
                     (dl (constant ,(compilo-encode-constant (car val))))
                     (dl (constant ,(compilo-encode-constant (cdr val)))))
            *res-asm-constants*)
      l))))

(defun compilo-2 (expr variables)
  "Vérifie si expr est bien un résultat valable de squash-lisp-1.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 1.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (labels ((compilo-3 (expr)
             (cond-match
              expr
              ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
              (((? (member x '(progn simple-tagbody))) :body _*)
               `(section code ,@(mapcar #'compilo-3 body)))
              ((if :condition _ :si-vrai _ :si-faux _)
               (with-label ((after-if 'after-if) (after-else 'after-else))
                 `(section code
                           ,(compilo-3 condition)
                           (cmp (register r0) (constant ,(syslabel nil)))
                           (jeq ,after-if)
                           ,(compilo-3 si-vrai)
                           (jmp ,after-else)
                           ,after-if
                           ,(compilo-3 si-faux)
                           ,after-else)))
              ((unwind-protect :body _ :cleanup _)
               (with-label ((l-protect-code 'protect-code) (l-after-protect-code 'after-protect-code))
                 `(section code
                           (push (constant ,l-protect-code))
                           (push (constant ,(syslabel 'marker-unwind-protect)))
                           ,(compilo-3 body)
                           (pop (register r2))
                           (pop (register r2))
                           (jmp (constant ,l-after-protect-code))
                           ,l-protect-code
                           ,(compilo-3 cleanup)
                           (jmp (constant ,(syslabel 'start-unwind)))
                           ,l-after-protect-code)))
              ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
              (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
               (with-label ((l-catch-code 'catch-code) (l-after-catch-code 'after-catch-code))
                 `(section code
                           ;; TODO !!! prendre en compte ces push dans la taille de l'environnement !
                           (push (constant ,l-catch-code))
                           ,(compilo-3 object)
                           (push (register r0))
                           (push (constant ,(syslabel 'marker-unwind-destination)))
                           ,(compilo-3 body)
                           (pop (register r2))
                           (pop (register r2))
                           (pop (register r2))
                           (jmp (constant ,l-after-catch-code))
                           ,l-catch-code
                           ,(compilo-3 catch-code)
                           ,l-after-catch-code)))
              ((unwind :object _)
               `(section code
                         ,(compilo-3 object)
                         (push (register ip))
                         (jmp (constant ,(syslabel 'find-unwind-destination))) ;; renvoie le haut de la pile dans r1. TODO ATTENTION À LA PLACE PRISE PAR LE PUSH
                         (mov (register sp) (memory ,(syslabel 'singleton-unwind-destination)))
                         (mov (register r1) (register sp)) ;; On remonte en haut de la pile
                         (jmp (constant ,(syslabel start-unwind)))))
              
              ((unwind-for-tagbody :object _ :post-unwind-code _)
               (with-label ((l-post-unwind-code 'post-unwind-code))
                 `(section code
                           (compilo-3 object)
                           (push (register ip))
                           (jmp (constant ,(syslabel 'find-unwind-destination))) ;; renvoie le haut de la pile dans r1. TODO ATTENTION À LA PLACE PRISE PAR LE PUSH
                           (mov (constant ,l-post-unwind-code) (memory ,(syslabel 'singleton-post-unwind-code)))
                           (add (constant 3) (register sp)) ;; On "re-push" l'adresse de la cible, l'objet et le marqueur, mettre 2 au lieu de 3 si on n'a pas de marqueur.
                           (mov (register sp) (memory ,(syslabel 'singleton-unwind-destination)))
                           (mov (register r1) (register sp) ;; On remonte en haut de la pile
                           (jmp (constant ,(syslabel 'start-unwind)))
                           ,l-post-unwind-code
                           ,(compilo-3 post-unwind-code) ;; DOIT contenir un jump !
                           (mov (constant ,(error-code 'unwind-for-tagbody--doit-contenir-un-jump)) r0) ;; valeur de retour pour la vm
                           (halt))))) ;; Sinon contenait pas de jump, on quite "brutalement"
              ((jump-label :name $$)
               `(section code ,(code-label name)))
              ((jump :dest $$)
               `(section code (jmp ,(code-label name))))
              ;; TODO : cas particulier funcall car
              ;; TODO : cas particulier funcall cdr
              ((funcall :fun _ :params _*)
               `(section code
                         ;; Sommaire :
                         ;; - calculer les paramètres un à un
                         ;; - à chaque fois, ajouter leur valeur dans une liste, par chirurgie svp.
                         ;; - maintenir quelque part dans la pile un pointeur vers le premier paramètre
                         ;; - et un pointeur vers le dernier cons de la liste de paramètres
                         ;; - calculer la fonction
                         ;; - push ip
                         ;; - jmp r0

                         ;; Note : on pourrait tout allouer d'un coup, et setter tout la liste avec des nil immédiatement après.
                         ;; TODO : si aucun paramètre
                         
                         ;; premier paramètre :
                         ,(compilo-3 (car params))
                         (push (register r0)) ;; (r0 vaut la valeur du paramètre)
                         ;; alloc 1+4+4 bytes of memory for a cons
                         (mov (constant ,(+ 1 4 4)) (register r1))
                         (push (register ip))
                         (jmp (constant ,(syslabel 'alloc-tas)))
                         ;; On push la liste des paramètres (encore incomplète)
                         (push (register r0))
                         ;; set cons type byte :
                         (movb (constant ,(type-number 'cons)) (indirect-register r0))
                         ;; set car of new cons to value :
                         (pop (register r2)) ;; r2 := value
                         (mov (register r2) (indexed 1 r0))
                         ;; allways set cdr to nil, in case the gc came by :
                         (mov (constant ,(syslabel nil)) (indexed 5 r0))
                         ;; le cons courant devient l'ancien
                         (mov (register r0) (register r1))

                         ,@(loop
                              for i in (cdr params)
                              append `((push (register r1)) ;; (r1 vaut old-cons)
                                       (compilo-3 param-i)
                                       (push (register r0)) ;; (r0 vaut la valeur du paramètre)
                                       ;; alloc-tas 1+4+4 bytes of memory for a cons
                                       (mov (constant ,(+ 1 4 4)) r1)
                                       (push (register ip))
                                       (jmp (constant ,(syslabel 'alloc-tas)))
                                       ;; set cons type byte :
                                       (movb (constant ,(type-number 'cons)) (indirect-register r0))
                                       ;; set cdr of old last to new cons :
                                       (pop (register r2)) ;; r2 := value
                                       (pop (register r1)) ;; r1 := old-cons
                                       (mov (register r0) (indexed 5 r2))
                                       ;; set car of new cons to value :
                                       (mov (register r2) (indexed 1 r0))
                                       ;; allways set cdr to nil, in case the gc came by :
                                       (mov (constant ,(syslabel 'nil)) (indexed 5 r0))
                                       ;; le cons courant devient l'ancien
                                       (mov (register r0) (register r1))))
                                       
                         ;; On calcule la fonction :
                         (compilo-3 fun)

                         ;; Facultatif :
                         ;; On teste si le premier octet de *r0 est bien closure-object (sait-on jamais…)
                         (mov (indirect-register r0) (register r1))
                         (cmp (constant ,(type-number 'closure-object)) (register r1))
                         (jneq (constant ,(syslabel 'invalid-closure-object)))
                         ;; Fin facultatif
                         
                         ;; On récupère la closure
                         (mov (indexed 5 r0) (register r1))
                         (push (register r1)) ;; on push la closure (2e paramètre)
                         ;;   TODO !!! la closure et les paramètres sont dans le mauvais ordre ! corriger ça dans le préambule de la fonction
                         ;; On récupère la fonction
                         (mov (indexed 1 r0) (register r0))
                         ;; On appelle la fonction
                         (push (register ip))
                         (jmp (register r0))))
               (every #'compilo-2 (cons fun params)))
              ((quote :val _)
               (compilo-encode-constant val))
              ((get-var :name $$)
               `(section code (mov (indexed ,(cdr (assoc name variables)) (register bp)) (register r0))))
              ((setq :name $$ :value _)
               `(section code
                         ,(compilo-3 value)
                         (mov (register r0) (indexed ,(cdr (assoc name variables)) (register bp)))))
              ((fdefinition (quote :name $$))
               `(section code (mov (memory ,(global-label-function name)) (register r0))))
              ((symbol-value (quote :name $$))
               `(section code (mov (memory ,(global-label-variable name)) (register r0))))
              ((set (quote :name $$) :value _)
               `(section code
                         ,(compilo-3 value)
                         (mov (register r0) (memory ,(global-label-variable name)))))
              ((make-closure :fun $$ :vars $$*)
               ;; On alloue 1+4+4 octets pour un objet closure
               ;; set type = closure-object
               ;; set mot1 = adresse de la fonction
               ;; set mot2 = on construit une liste de longueur (length vars) en la remplissant avec les valeurs des vars.
               t)
              ((make-captured-var :name $$)
               `(section code
                         ;; allouer 5 octets
                         (mov (constant 5) (register r0))
                         (push ip)
                         (jmp (constant ,(syslabel 'alloc-tas))) ;; => adresse dans r0
                         ;; affecter le pointeur à la variable
                         (mov (register r0) (indexed ,(cdr (assoc name variables)) (register bp)))
                         ;; affecter le type captured-var au premier
                         (movb (constant ,(type-number 'captured-var)) (indirect-register r0))
                         ;; affecter le pointeur nil aux 4 suivants
                         (mov (constant ,(global-label-symbol nil)) (indirect-register r0))))
              ((get-captured-var :name $$)
               `(section code
                         (mov (indexed ,(cdr (assoc name variables)) (register bp)) (register r0))
                         (mov (indexed 1 (register r0)) (register r0)))) ;; Pas de test de type
              ((set-captured-var :name $$ :value _)
               `(section code
                         ,(compilo-3 value)
                         (mov (indexed ,(cdr (assoc name variables)) (register bp)) (register r1))
                         (mov (register r0) (indexed 1 (register r1)))))
              (_
               (warn "compilo-3: Assertion failed ! This should not be here : ~w" expr)
               nil))))
    (compilo-3 expr)))

(defun compilo-1 (expr)
  (match
   (top-level :main $$ (progn (set :names $$ (lambda (:closure-names $$ &rest :params-names $$) (get-var $$) (get-var $$) (let :vars ($$*) :bodys _)))*))
   expr
   (loop
      for name in names
      and closure-name in closure-names
      and params-name in params-names
      and var in vars
      and body in bodys
      for nbvars = (length var)
      collect `(section code
                        ,(code-label name)
                        ;; +1 pour les paramètres (non)
                        ;; +1 pour la closure     (non)
                        ;; TODO : + autant que nécessaire pour les différents funcall et unwind
                        ;; +1 pour le bp
                        ;; +1 pour le begin-frame
                        ;; +1 pour le marker-end-frame
                        (mov (constant ,(+ 3 nbvars)) (register r0))
                        (push (register ip)) ;; call
                        (mov (register sp) (register r0)) ;; begin-frame : Va avec le marker-end-frame
                        (push (register bp))
                        (mov (register sp) (register bp))
                        (add (constant ,nbvars) (register sp))
                        (push (register r0)) ;; Permet à unwind de sauter directement jusqu'au begin-frame.
                        (push (constant ,(syslabel 'marker-end-frame)))
                        (jmp (constant ,(syslabel 'reserve-stack))) ;; lance le gc pour re-dimensionner le tas si nécessaire / ou bien erreur si dépassement.
                        (sub (constant ,nbvars) (register sp)) ;; TODO ! vérifier le sens du sub
                        ,(compilo-2 body (loop
                                            with hole = (make-symbol "HOLE")
                                            for v in (append (list params-name closure-name hole hole hole) var)
                                            for i upfrom -2
                                            collect `(,var . ,i)))
                        (add (constant ,nbvars) (register sp))
                        (pop (register bp))
                        (pop (register r1)) ;; params
                        (pop (register r1)) ;; closure
                        (pop (register r1)) ;; ip
                        (jmp (register r1))) ;; ret
      into res
      finally (return
                `(section code
                          ,(compilo-init main)
                          ,res
                          ,@(reverse *res-asm-constants*))))))

(defun compilo (expr)
  (setq *label-ctr* 0)
  (setq *sys-labels* nil)
  (setq *global-labels* nil)
  (setq *code-labels* nil)
  (flatten-asm (compilo-1 (squash-lisp-1+3 expr))))

#|

La pile (en bas = le plus récent) :

========== xx
closure
params
old-bp <--------------------- bp here
begin-frame = addr xx
marker-end-frame
[var0]
[var1]
[var2]
[var3]
...
[var (- nb-vars 1)] <-------- sp here when body executed (next push will be underneath).

(squash-lisp-1+3 '(+ 2 3))

# |
;;; Exemples

(my-compile '(1 2 3))

(my-compile 3)
;; section .data
;; fixnum-constant-1
;;   db 0
;;   db 3

(my-compile (+ 2 3))
;; =>
;; section .data
;; fixnum-constant-1:
;;   db 1
;;   db 2
;; section .data
;; fixnum-constant-2:
;;   db 1
;;   db 3
;; section .code
;; code-1:
;;   load @global-1 r0
;;   push r0
;;   load @global-2 r0
;;   push r0
;;   push 2
;;   jsr @fn-+
;;   retn
;; section .text
;; :fn-+
;;   pop r1
;;   pop r0
;;   add r1 r0
;;   retn
|#

(provide 'compilation)

















#|
(defmacro fasm (&rest stuff)
  `(format nil ,@stuff))
(defun db-type (type)
  (fasm "db ~a" (type-number type)))

;; My-compile

(defvar *result-asm* nil)
(defvar *asm-sections* '(code data)) ;; Note : l'assembleur sera produit dans cet ordre

(defun real-asm-block (section label body)
  (when (not (member section *sections*))
    (error "Section assembleur inconnue : ~w" section))
  (push (format nil "section .~w" section) *result-asm*)
  (push (format nil "~a:" label) *result-asm*)
  (mapcar (lambda (x) (push x *result-asm*)) body)
  label)

(defun asm-block (section label-base &rest body)
    (real-asm-block
     section
     (format nil "~a-~a" label-base (incf *label-ctr*))
     body))

(defvar *asm-once* nil)
(defun asm-once (section label &rest body)
  (unless (member label asm-once :test #'string-equal)
    (push label asm-once)
    (real-asm-block section label body))
  label)

(defmacro my-compile (expr)
  `(progn (setq *result-asm* nil)
          (setq asm-once nil)
          (my-compile-1 `(:main ,(lisp2cli ',expr)))
          (format nil "~&~{~%~a~}" (flatten (reverse *result-asm*)))))

;;; Règles de compilation

(defmatch my-compile-1)

(defun compile-get-val (cli)
  (if (match (:nil :const . _) cli)
      (list (fasm "load @~a r0" (my-compile-1 cli))
            (fasm "push r0"))
      (list (my-compile-1 cli)
            (fasm "push r0"))))

;; call
(defmatch my-compile-1 (:nil :call :name _ :params . _)
  (list
   (mapcar #'compile-get-val params)
   (fasm "push ~a" (length params))
   (fasm "jsr function-~a" name)))

;; main
(defmatch my-compile-1 (:nil :main :body _*)
  (asm-once 'code "main"
            (mapcar #'my-compile-1 body)))


;; if
((if :condition _ :si-vrai _ :si-faux _)
 (let ((else-label (gen-label "else"))
       (end-if-label (gen-label "end-if")))
   (compile condition)
   (fasm "cmp r0 @nil")
   (fasm "jeq @~a" else-label)
   (compile si-vrai)
   (fasm "jmp @~a" end-if-label)
   (fasm "label @~a" else-label)
   (compile si-faux)
   (fasm "label @~a" end-if-label)))

|#
