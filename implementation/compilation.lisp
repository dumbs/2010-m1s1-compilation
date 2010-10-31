(defvar asm-max-int (expt 2 32))

(defun .data (&rest asm) `(section .data ,asm))
(defun .text (&rest asm) `(section .text ,asm))

(defvar global-variable-ctr 0)
(defun global-variable (&rest asm)
  (incf global-variable-ctr)
  (let ((name (intern (concatenate 'string "GLOBAL-"
                                   (format nil "~s" global-variable-ctr)))))
    `((label ,name ,asm)
      ,(asm-load name 'r0))))

(defun asm-db (val)
  (when (>= val asm-max-int)
    (warn "db : value too big : ~a" val))
  `(instruction db ,val))

(defun asm-load (from to)
  `(instruction load ,from ,to))

(defun asm2asm-r (asm out)
  (mapcar (lambda (x) (asm2asm x out))
          asm))

(defun asm2asm (ctx &optional (out t))
  (loop
     for (instruction . params) in (cdr (assoc 'asm ctx))
     do (cond ((eq instruction 'section)
               (format out "~&section .data"))
              (t
               (format out "~&~a ~a" instruction params)))))

(defun symbol-concat (s1 s2)
  (intern (concatenate 'string
                       (format nil "~:@(~a~)" s1)
                       (format nil "~:@(~a~)" s2))))

(defun append-to-section (ctx section asm)
  (let ((asm (assoc 'asm ctx))
        (old-section (assoc 'section ctx)))
    (push `(section ,section) (cdr asm))
    (setf (assoc 'section ctx) section)
    (loop
       for x in asm
       do (push x (cdr asm)))
    (push `(section ,oldsection) (cdr asm))
    (setf (assoc 'section ctx) old-section)))

(defun .data (ctx asm)
  (append-to-section ctx '.data asm))

(defvar asm-types '(fixnum))
(defvar asm-constant-counter 0)
(defun asm-constant (ctx type value)
  (let ((name (symbol-concat "constant-" asm-constant-counter)))
    (.data ctx
           `((label ,name)
             (db ,(position type asm-types))
             (db ,value)))
    name))

(defmacro my-compile (expr)
  `(let ((ctx (copy-tree '((asm)))))
     (my-compile1 ',expr ctx)
     ctx))

(defun my-compile1 (expr ctx)
  (cond
    ;; Numbers
    ((numberp expr)
     (asm-constant ctx 'fixnum expr))
    (t
     'niy)))

(asm2asm (my-compile 3))
;; section .data
;; :global-1
;;   db 1
;;   db 3

(asm2asm (my-compile (+ 2 3))
         )
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