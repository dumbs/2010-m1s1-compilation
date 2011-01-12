;; 
;; TODO !! ATTENTION !! Quand onc récupère des données qui font 1 octet de large, en fait on récupère 4 octets !
;; 
(require 'match "match")
(require 'util "util")
(require 'squash-lisp "implementation/squash-lisp")

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
(defvar *sections* '(data code))

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



                                        ;===========================
                                        ;        ((( V2 *)))
                                        ;===========================

(defun compilo (expr)
  (match
   (top-level :main $$
              (progn (set :name $$ (lambda :params (&rest $$) :unused (get-var $$)
                                           (let :vars ($$*) :body _*)))*))
   expr
   (setq res
         (loop
            for n in name
              and p in params
              and v in vars
              and b in body
              collect `(label n)
              collect `(mov ,(+ 1 (length vars)) r0)
              collect `(call ,(syslabel reserve-stack))
   (setq res (append `((jmp ,main)) (flatten-asm res)))
   res))

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

(provide 'compilation)