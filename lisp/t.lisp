(load "../bootstrap/1.2.7-read.lisp")
(load "mini-meval")

(defvar tmm nil)
(setq tmm (read (open "tmm.lisp")))

(defvar e-tmm nil)
;(setq e-tmm (make-etat list + - cons car cdr < > <= >= = make-symbol))

(setf e-tmm (make-etat
		   car
		   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
		   caaar
		   caadr
		   cadar
		   caddr
		   cdaar
		   cdadr
		   cddar
		   cdddr
		   caar
		   cadr
		   cdar
		   cddr
		   first
		   second
		   third
		   fourth
		   fifth
		   sixth
		   seventh
		   eighth
		   ninth
		   tenth
		   tree-equal
		   char
		   schar
		   string
		   string= 
		   make-string
		   equal
		   eql
		   eq
		   cdr
		   cons
		   list
		   oddp
		   symbolp
		   numberp
		   stringp
		   equalp
		   +
		   -
		   *
		   /
		   =
		   <
		   >
		   <=
		   >=
		   read
		   funcall
		   assoc
		   mload
		   mini-meval
           make-symbol
           print
           endp
           keywordp
           consp
           null
           atom
           length
           map
           identity
           last
           char=
           member
           intern
           format
           mapcar
           reverse
           acons
           append
           butlast
           not
           listp
))
