(defvar asm-fixnum-size 32)
(defvar asm-max-fixnum (expt 2 asm-fixnum-size))
(defvar asm-symbols '())
(defvar asm-types '(fixnum symbol string))

(defun symbol-concat (&rest compounds)
  (intern (format nil "~{~a~}" compounds)))

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

(defmacro my-compile (expr)
  `(progn
     (setq asm '())
     (setq label-ctr '())
     (setq asm-symbols '())
     (my-compile1 ',expr)
     asm))

(defun my-compile1 (expr)
  (cond
    ;; Fixnum
    ((and (numberp expr) (< expr asm-max-int))
     (let ((name (new-label-ctr 'constant)))
       (asm .data label name)
       (asm .data db (position 'fixnum asm-types))
       (asm .data db expr)
       name))
    ;; Bignum
    ((and (numberp expr) (< expr asm-max-int))
     (error "Not implemented yet : bignum"))
    ;; String
    ((stringp expr)
     (let ((name (new-label-ctr 'constant)))
       (asm .data label name)
       (asm .data db (position 'string asm-types))
       (asm .data db (length (string expr)))
       (db-string (string expr))
       name))
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
(setq compile-rules-conditions '())

(defvar compile-rules-functions '())
(setq compile-rules-functions '())

(defmacro defcompile-rule (name condition body)
  `(progn (push (cons ',name (lambda (expr) ,condition)) compile-rules-conditions)
          (push (cons ',name (lambda (expr) ,body)) compile-rules-functions)))

;;; Règles de compilation

(defun db-string (str)
  (loop
     for i from 0 below (length str)
     do (asm .data db (char-code (char str i)))))

(defcompile-rule fixnum (and (numberp expr) (< expr asm-max-fixnum))
  (let ((lab (mk-label 'constant)))
    (asm-label lab)
    (info-byte 'fixnum)
    (asm-db expr)
    lab))

(defun split-bytes (n byte-size)
  "Découpe N en plusieurs valeurs inférieures à 2^(byte-size),
   les mots de poids faible en premier.
   (split-bytes 0 byte-size) renvoie nil."
  (if (= n 0)
      '()
      (cons (ldb (byte byte-size 0) n)
            (split-bytes (ash n (- byte-size)) byte-size))))

(defcompile-rule bignum (and (numberp expr) (>= expr asm-max-fixnum))
  (let ((lab (mk-label 'constant)))
    (asm-label lab)
    (info-byte 'bignum)
    (let ((bytelist (split-bytes asm-fixnum-size)))
      (asm-db (length bytelist))
      (loop for i in bytelist
         do (asm-db i)))
    lab))

(asm2asm (my-compile foo))

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