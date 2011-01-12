(require 'match "match")
(require 'util "util")
(require 'squash-lisp "implementation/squash-lisp")

;; TODO !! ATTENTION !! Quand onc récupère des données qui font 1 octet de large, en fait on récupère 4 octets !

(defvar *asm-sections* '(code data)) ;; Note : l'assembleur sera produit dans cet ordre

(defvar *sys-labels*)
(defun syslabel (label)
  (assoc-or-push label (derived-symbol label) *sys-labels*))

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
                ((label :l $$)
                 (push l etiquettes)
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


(defun compilo-2 (expr variables)
  "Vérifie si expr est bien un résultat valable de squash-lisp-1.
Permet aussi d'avoir une «grammaire» du simple-lisp niveau 1.
Attention : il y a quelques invariants qui ne sont pas présents dans cette vérification."
  (labels ((compilo-3 (expr)
             (cond-match
              expr
              ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
              (((? (member x '(progn simple-tagbody))) :body _*)
               (every #'compilo-2 body))
              ((if :condition _ :si-vrai _ :si-faux _)
               (and (compilo-2 condition)
                    (compilo-2 si-vrai)
                    (compilo-2 si-faux)))
              ((unwind-protect :body _ :cleanup _)
               (and (compilo-2 body)
                    (compilo-2 cleanup)))
              ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
              (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
               (and (compilo-2 object)
                    (compilo-2 body)
                    (compilo-2 catch-code)))
              ((unwind :object _)
               (compilo-2 object))
              ((unwind-for-tagbody :object _ :post-unwind-code _)
               (and (compilo-2 object)
                    (compilo-2 post-unwind-code)))
              ((jump-label :name $$)
               t)
              ((jump :dest $$)
               t)
              ;; ((let ($$*) :body _)
              ;;  (compilo-2 body))
              ;; ((lambda (&rest $$) :unused _ :body (let ($$*) _*))
              ;;  (compilo-2 body))
              ((funcall :fun _ :params _*)
               (every #'compilo-2 (cons fun params)))
              ((quote _)
               t)
              ((get-var :var $$)
               (cdr (assoc var variables)))
              ((setq :name $$ :value _)
               (compilo-2 value))
              ((fdefinition (quote $$))
               t)
              ((symbol-value (quote $$))
               t)
              ((set (quote $$) :value _)
               (compilo-2 value))
              ((make-captured-var $$)
               t)
              ((get-captured-var $$)
               t)
              ((set-captured-var $$ :value _)
               (compilo-2 value))
              (_
               (warn "compilo-2: Assertion failed ! This should not be here : ~w" expr)
               nil))))
    (compilo-3 expr)))

(defun compilo-1 (expr &aux res)
  (match
   (top-level :main $$ (progn (set :names $$ (lambda (:closure-names $$ &rest :params-names $$) (get-var $$) (get-var $$) (let :vars ($$*) :bodys _*)))*))
   expr
   (setq res (loop
                for name in names
                and closure-name in closure-names
                and params-name in params-names
                and var in vars
                and body in bodys
                collect `(label name)
                collect `(mov (constant ,(+ 2 (length var))) (register r0)) ;; +1 pour les paramètres (nécessaire?)
                collect `(push (register ip))
                collect `(jmp (constant ,(syslabel 'reserve-stack))) ;; lance le gc pour re-dimensionner le tas si nécessaire / ou bien erreur si dépassement.
                collect (compilo-2 `(progn body) (loop
                                                    for v in (cons closure-name (cons params-name var))
                                                    for i upfrom 0
                                                    collect `(,var . ,i)))))
   `(section code (jmp main) ,@res)))

(defun compilo (expr)
  (flatten-asm (compilo-1 (squash-lisp-1+3 expr))))

(squash-lisp-1+3 '(+ 2 3))

#|
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
(defvar *asm-fixnum-size* 32)
(defvar *asm-max-fixnum* (expt 2 *asm-fixnum-size*))
(defun type-number (type)
  (position type '(placeholder fixnum bignum symbol string cons nil)))
(defvar *label-ctr* 0)

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

;; fixnum
(defmatch my-compile-1 (:nil :const :num . (? numberp (< x *asm-max-fixnum*)))
  (asm-block 'data "fixnum-constant"
             (db-type 'fixnum)
             (fasm "db ~a" num)))

;; bignum
(defmatch my-compile-1 (:nil :const :num . (? numberp (>= x *asm-max-fixnum*)))
  (asm-block 'data "bignum-constant"
             (db-type 'bignum)
             (let ((lst (split-bytes num *asm-fixnum-size*)))
               (fasm "~{~&db ~a~}" (cons (length lst) lst)))))

;; string
(defmatch my-compile-1 (:nil :const :str . (? stringp))
  (asm-block 'data "string-constant"
             (db-type 'string)
             (fasm "db ~a" (length str))
             (fasm "~{~&db ~a~}" (map 'list #'char-code str))))

;; symbol
(defmatch my-compile-1 (:nil :const :sym . (? symbolp))
  (asm-once 'data (format nil "symbol-~w" sym)
             (db-type 'symbol)
             (fasm "db @~a" (my-compile-1 (string sym)))))

;; cons
(defmatch my-compile-1 (:nil :const . (:car _ :cdr . _))
  (asm-block 'data "cons-cell-constant"
             (db-type 'cons)
             (fasm "db @~a" (my-compile-1 `(:const . ,car)))
             (fasm "db @~a" (my-compile-1 `(:const . ,cdr)))))

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
