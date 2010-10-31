(defvar asm-fixnum-size 32)
(defvar asm-max-fixnum (expt 2 asm-fixnum-size))
(defvar asm-symbols '())
(defvar asm-types '(fixnum symbol string))

;;; Utilitaires

(defun symbol-concat (&rest compounds)
  (intern (format nil "~{~a~}" compounds)))

(defmacro aset (k v alist)
  `(let ((my-k ,k)
         (my-v ,v))
     (let ((association (assoc my-k ,alist)))
       (if association
           (setf (cdr association) my-v)
           (push (cons my-k my-v) ,alist)))))

(defun split-bytes (n byte-size)
  "Découpe N en plusieurs valeurs inférieures à 2^(byte-size),
   les mots de poids faible en premier.
   (split-bytes 0 byte-size) renvoie nil."
  (if (= n 0)
      '()
      (cons (ldb (byte byte-size 0) n)
            (split-bytes (ash n (- byte-size)) byte-size))))

;;; ASM

(defun get-label (category ending)
  (symbol-concat category "-" ending))

(defvar label-ctr '())
(defun new-label-ctr (category)
  (when (not (assoc category label-ctr))
    (setf label-ctr (acons category 0 label-ctr)))
  (get-label category (incf (cdr (assoc category label-ctr)))))

(defvar asm '())
(defmacro asm (section isn &rest params)
  `(push (list ',section ',isn ,@params) asm))

(defmacro asm-db (value)
  `(asm .data db ,value))

(defun asm2asm1 (asm out)
  (loop
     for (section instruction . params) in (reverse asm)
     and old-section = nil then section
     when (not (eq section old-section))
       do (format out "~&section~8T~a" section)
     if (eq instruction 'label)
       do (format out "~&~a:" (car params))
     else
       do (format out "~&~8T~a~{~8,8T~a~}" instruction params)))

(defun asm2asm (asm)
  (let ((out (make-string-output-stream)))
    (asm2asm1 asm out)
    (get-output-stream-string out)))

;; My-compile

(defmacro my-compile (expr)
  `(progn
     (setq asm '())
     (setq label-ctr '())
     (setq asm-symbols '())
     (my-compile1 ',expr)
     asm))

(defun my-compile1 (expr)
  (some (lambda (condition rule)
          (if (not (funcall (cdr condition) expr))
              (funcall (cdr rule) expr)))
        compile-rules-conditions
        compile-rules-functions))

(cond
    ;; Symbol
    ((symbolp expr)
     (progn 
       (unless (member expr asm-symbols)
         (let ((name (new-label-ctr 'constant))
               (str-label (my-compile1 (string expr))))
           (asm .data label name)
           (asm .data db (position 'symbol asm-types))
           (asm .data db `(@ ,str-label))
           name))
       (get-label 'symbol expr)))))

(defvar compile-rules-conditions '())
(defvar compile-rules-functions '())

(defmacro defcompile-rule (name condition body)
  `(progn (aset ',name (lambda (expr) ,condition) compile-rules-conditions)
          (aset ',name (lambda (expr) ,body) compile-rules-functions)))

;;; Règles de compilation

(defcompile-rule fixnum (and (numberp expr) (< expr asm-max-fixnum))
  (let ((lab (mk-label 'constant)))
    (asm-label lab)
    (info-byte 'fixnum)
    (asm-db expr)
    lab))

(defcompile-rule bignum (and (numberp expr) (>= expr asm-max-fixnum))
  (let ((lab (mk-label 'constant)))
    (asm-label lab)
    (info-byte 'bignum)
    (let ((bytelist (split-bytes asm-fixnum-size)))
      (asm-db (length bytelist))
      (loop for i in bytelist
         do (asm-db i)))
    lab))

(defcompile-rule string (stringp expr)
  (let ((name (new-label-ctr 'constant))
        (str (string expr)))
    (asm .data label name)
    (asm-db (position 'string asm-types))
    (asm-db (length (string expr)))
    (loop
       for i from 0 below (length str)
       do (asm .data db (char-code (char str i))))
    name))

(asm2asm (my-compile "foo"))

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