(load "util")
(load "lisp2lic")

(defvar asm-fixnum-size 8)
(defvar asm-max-fixnum (expt 2 asm-fixnum-size))
(defun type-number (type)
  (position type '(fixnum bignum symbol string cons)))
(defvar label-ctr 0)

;; My-compile

(defvar result-data nil)
(defvar result-asm nil)
(defun data (&rest args)
  (push (apply #'format nil args) result-data))
(defun asm (&rest args)
  (push (apply #'format nil args) result-asm))

(defmacro my-compile (expr)
  `(progn (setq result-data nil)
          (setq result-asm nil)
          (my-compile1 (lisp2cli ',expr))
          (format nil
                  "section .data~%~
                   ~{~%~a~}~%~
                   ~%section .text~%~
                   ~{~%~a~}"
                  (reverse result-data)
                  (reverse result-asm)
                  )))

(defvar compile-rules-conditions '())
(defvar compile-rules-functions '())

(defun my-compile1 (expr)
  (some (lambda (condition rule)
          (if (funcall (cdr condition) expr)
              (funcall (cdr rule) expr)))
        compile-rules-conditions
        compile-rules-functions))

(defmacro defcompile-rule (name condition &rest body)
  `(progn (aset ',name (lambda (expr) ,condition) compile-rules-conditions)
          (aset ',name (lambda (expr) ,@body) compile-rules-functions)))

;;; Règles de compilation

(defcompile-rule fixnum (and (numberp expr) (< expr asm-max-fixnum))
  (data "fixnum-constant-~a:" (incf label-ctr))
  (data "db ~a" (type-number 'fixnum))
  (data "db ~a" expr)
  (format nil "fixnum-constant-~a" label-ctr))

(defcompile-rule bignum (and (numberp expr) (>= expr asm-max-fixnum))
  (data "bignum-constant-~a:" (incf label-ctr))
  (data "db ~a" (type-number 'bignum))
  (let ((lst (split-bytes expr asm-fixnum-size)))
    (data "~{~&db ~a~}" (cons (length lst) lst)))
  (format nil "bignum-constant-~a" label-ctr))

(defcompile-rule string (stringp expr)
  (data "string-constant-~a:" (incf label-ctr))
  (data "db ~a" (type-number 'string))
  (data "db ~a" (length expr))
  (data "~{~&db ~a~}" (map 'list #'char-code expr))
  (format nil "string-constant-~a" label-ctr))

(defcompile-rule symbol (symbolp expr)
  (let ((name (my-compile1 (string expr))))
    (data "symbol-~a:" (incf label-ctr))
    (data "db ~a" (type-number 'symbol))
    (data "db @~a" name)
    (format nil "symbol-~a" label-ctr)))

(defcompile-rule cons (and (consp expr) (eq 'quote (car expr)) (consp (cdr expr)) (consp (cadr expr)))
  (print "")
  (print expr)
  (print (list 'quote (caadr expr)))
  (print (list 'quote (cdadr expr)))
  (let ((left (my-compile1 (list 'quote (caadr expr))))
        (right (my-compile1 (list 'quote (cdadr expr)))))
;  (let ((left "foo")
;        (right "bar"))
    (data "cons-~a:" (incf label-ctr))
    (data "db ~a" (type-number 'cons))
    (data "db @~a" left)
    (data "db @~a" right)
    (format nil "cons-~a" label-ctr)))

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
;; :global-1
;;   db 1
;;   db 2
;; section .data
;; :global-2
;;   db 1
;;   db 3
;; section .text
;; :fn-main
;;   load @global-1 r0
;;   push r0
;;   load @global-2
;;   push r0
;;   jsr @fn-+
;;   retn
;; section .text
;; :fn-+
;;   pop r1
;;   pop r0
;;   add r1 r0
;;   retn