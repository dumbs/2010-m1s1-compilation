(defvar out '())

(defmacro asm-section (section &rest asm)
  `(progn (push `(begin section ,,section) out)
          ,@asm
          (push `(end   section ,,section) out)))

(defmacro asm-label (label &rest asm)
  `(progn (push `(begin label ,,label) out)
          ,@asm
          (push `(end   label ,,label) out)))

(defmacro .data (&rest asm) `(asm-section '.data ,@asm))
(defmacro .text (&rest asm) `(asm-section '.text ,@asm))

(defmacro global-variable (&rest asm)
  `(asm-label 'global ,@asm))

(defmacro db (val)
  `(push `(instruction db ,,val) out))

(setq out nil)
(.data (global-variable (db 1)))

(defun asm2asm (asm out)
  (let ((stack '()))
    (mapcar (lambda (x)
              (when (eq (car x) 'begin)
                (push (cdr x) stack))
              (when (eq (car x) 'end)
                (unless (equal (pop stack) (cdr x))
                  (error "Mauvaise imbrication des instructions assembleur !")))
              (cond ((eq (car x) 'end)
                     nil)
                    (and (eq (car x) 'begin) 
            (reverse asm))))
(asm2asm out t)

(defmacro my-compile (expr)
  `(my-compile1 ',expr))

(defun my-compile1 (expr)
  (cond
    ((numberp expr)
     (.data
      (global-variable
       (db 1)
       (db expr))))
    ((consp expr)
     (car expr))
    (t
     'niy)))

(my-compile 3)
;; section .data
;; :global-1
;;   db 1
;;   db 3

(my-compile '(+ 2 3))
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