(load "../bootstrap/1.2.7-read.lisp")
(load "mini-meval")

(defvar tmm nil)
(setq tmm (my-read (open "tmm.lisp")))

(defvar e-tmm nil)
(setq e-tmm (make-etat list + - cons car cdr < > <= >= = make-symbol))
