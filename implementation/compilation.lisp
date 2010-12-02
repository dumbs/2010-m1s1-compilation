(require 'match "match")
(require 'util "util")
(require 'squash-lisp "implementation/squash-lisp")

(defvar asm-fixnum-size 32)
(defvar asm-max-fixnum (expt 2 asm-fixnum-size))
(defun type-number (type)
  (position type '(placeholder fixnum bignum symbol string cons nil)))
(defvar label-ctr 0)

(defmacro fasm (&rest stuff)
  `(format nil ,@stuff))
(defun db-type (type)
  (fasm "db ~a" (type-number type)))

;; My-compile

(defvar result-asm nil)
(defvar sections '(data code))

(defun real-asm-block (section label body)
  (when (not (member section sections))
    (error "Section assembleur inconnue : ~w" section))
  (push (format nil "section .~w" section) result-asm)
  (push (format nil "~a:" label) result-asm)
  (mapcar (lambda (x) (push x result-asm)) body)
  label)

(defun asm-block (section label-base &rest body)
    (real-asm-block
     section
     (format nil "~a-~a" label-base (incf label-ctr))
     body))

(defvar asm-once nil)
(defun asm-once (section label &rest body)
  (unless (member label asm-once :test #'string-equal)
    (push label asm-once)
    (real-asm-block section label body))
  label)

(defmacro my-compile (expr)
  `(progn (setq result-asm nil)
          (setq asm-once nil)
          (my-compile-1 `(:main ,(lisp2cli ',expr)))
          (format nil "~&~{~%~a~}" (flatten (reverse result-asm)))))

;;; Règles de compilation

(defmatch my-compile-1)

;; fixnum
(defmatch my-compile-1 (:nil :const :num . (? numberp (< x asm-max-fixnum)))
  (asm-block 'data "fixnum-constant"
             (db-type 'fixnum)
             (fasm "db ~a" num)))

;; bignum
(defmatch my-compile-1 (:nil :const :num . (? numberp (>= x asm-max-fixnum)))
  (asm-block 'data "bignum-constant"
             (db-type 'bignum)
             (let ((lst (split-bytes num asm-fixnum-size)))
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