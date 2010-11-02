;; lisp2li simpliste pour le compilateur. On fusionnera les deux plus tard.

(defvar lisp2cli-rules-conditions '())
(defvar lisp2cli-rules-functions '())

(defun lisp2cli (expr)
  (some (lambda (condition rule)
          (if (funcall (cdr condition) expr)
              (funcall (cdr rule) expr)))
        lisp2cli-rules-conditions
        lisp2cli-rules-functions))

(defmacro deflisp2cli-rule (name condition &rest body)
  `(progn (aset ',name (lambda (expr) ,condition) lisp2cli-rules-conditions)
          (aset ',name (lambda (expr) ,@body) lisp2cli-rules-functions)))

(deflisp2cli-rule quote (match (quote (_ . _)) expr)
