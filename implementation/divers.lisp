;; variables "locales" : documentation
(defvar documentation '(function variable struct)) ;; TODO

;; TODO : décider de quelles "primitives" on a besoin.
;; "Primitives" :
;; - (%asm in-values out-values clobber-registers instructions)
;; - (%eval expr env)
;; - (%push-new-env "description")
;; - (%add-top-level-fun-binding name value)
;; - (%add-top-level-var-binding name value)
;; - (%add-fun-binding name value)
;; - (%add-var-binding name value)
;; - (%ref-fun name)
;; Les ref-xxx renvoient un bout de code ASM comme ci-dessous :
;; - Pour une valeur dans la pile :
;;   (%asm () (r0) (r0) "load X(sp) r0;") 
;;   où X est la position dans la pile de name
;; - Pour une valeur dans le top-level :
;;   (%asm () (r0) (r0) "load X(bp) r0;")
;; - Pour une valeur dans le tas (si on en a un)
;;   (%asm () (r0) (r0) "load X r0;")

(defmacro defun (name args &rest body)
  (let ((has-docstring
		 (and (stringp (car body))
			  (cdr body))))
	`(progn
	   (when ,has-docstring
		 (push (car body) documentation)) ;; TODO
	   (%top-level-fun-bind
		,name
		(lambda ,args
		  ,@(if has-docstring
				(cdr body)
			  body))))))

(defmacro setf (place value)
  (cond ((eq (car place) 'car)
		 `(%set-car ,place ,value))
		((eq (car place) 'cdr)
		 `(%set-cdr ,place ,value))
		;; TODO
		(t (error 'setf-invalid-place "setf : invalid place ~a" place))))

(defmacro cond (&rest conditions)
  (if (atom conditions)
	  nil
	`(if ,(caar conditions)
		 ,(if (atom (cdr (cdar conditions))) ;; Si une seule instruction dans la partie droite
			  (car (cdar conditions)) ;; On la met telle qu'elle
			'(progn ,@(cdar conditions))) ;; Sinon, on met un progn autour.
	   (cond ,@(cdr conditions)))))


(defmacro car (list)
  (%asm )) ;; TODO : list dans rX, résultat dans rY => move [indirect rX], rY

(defmacro cdr (list)
  (%asm )) ;; TODO : list dans rX, résultat dans rY => move rX, rY; incr rY; move [indirect rY], rY;

;; Les alias c*r ont été générés par un script (plus facile que de les méta-programmer...).
(defmacro caaaar (list) `(car (car (car (car ,list)))))
(defmacro caaadr (list) `(car (car (car (cdr ,list)))))
(defmacro caadar (list) `(car (car (cdr (car ,list)))))
(defmacro caaddr (list) `(car (car (cdr (cdr ,list)))))
(defmacro cadaar (list) `(car (cdr (car (car ,list)))))
(defmacro cadadr (list) `(car (cdr (car (cdr ,list)))))
(defmacro caddar (list) `(car (cdr (cdr (car ,list)))))
(defmacro cadddr (list) `(car (cdr (cdr (cdr ,list)))))
(defmacro cdaaar (list) `(cdr (car (car (car ,list)))))
(defmacro cdaadr (list) `(cdr (car (car (cdr ,list)))))
(defmacro cdadar (list) `(cdr (car (cdr (car ,list)))))
(defmacro cdaddr (list) `(cdr (car (cdr (cdr ,list)))))
(defmacro cddaar (list) `(cdr (cdr (car (car ,list)))))
(defmacro cddadr (list) `(cdr (cdr (car (cdr ,list)))))
(defmacro cdddar (list) `(cdr (cdr (cdr (car ,list)))))
(defmacro cddddr (list) `(cdr (cdr (cdr (cdr ,list)))))
(defmacro caaar  (list)      `(car (car (car ,list)))))
(defmacro caadr  (list)      `(car (car (cdr ,list)))))
(defmacro cadar  (list)      `(car (cdr (car ,list)))))
(defmacro caddr  (list)      `(car (cdr (cdr ,list)))))
(defmacro cdaar  (list)      `(cdr (car (car ,list)))))
(defmacro cdadr  (list)      `(cdr (car (cdr ,list)))))
(defmacro cddar  (list)      `(cdr (cdr (car ,list)))))
(defmacro cdddr  (list)      `(cdr (cdr (cdr ,list)))))
(defmacro caar   (list)           `(car (car ,list)))))
(defmacro cadr   (list)           `(car (cdr ,list)))))
(defmacro cdar   (list)           `(cdr (car ,list)))))
(defmacro cddr   (list)           `(cdr (cdr ,list)))))

;; Comptez les "d" :)
(defmacro first   (list)               `(car    ,list))
(defmacro second  (list)               `(cadr   ,list))
(defmacro third   (list)               `(caddr  ,list))
(defmacro fourth  (list)               `(cadddr ,list))
(defmacro fifth   (list)       `(car    (cddddr ,list)))
(defmacro sixth   (list)       `(cadr   (cddddr ,list)))
(defmacro seventh (list)       `(caddr  (cddddr ,list)))
(defmacro eighth  (list)       `(cadddr (cddddr ,list)))
(defmacro ninth   (list) `(car  (cddddr (cddddr ,list))))
(defmacro tenth   (list) `(cadr (cddddr (cddddr ,list))))

(defmacro let (bindings &rest body)
  `((lambda ,(mapcar #'car bindings)
	  ,body)
	,(mapcar #'cdar bindings)))

(defmacro let* (bindings &rest body)
  `(let (,(car bindings))
	 (let* ,(cdr bindings)
	   ,body)))

(defmacro labels (f-bindings &rest body)
  ;; TODO
  )

(defmacro funcall (function &rest args)
  ;; TODO
  )

(defmacro apply (function &rest args)
  ;; TODO
  ;; (last args) est la liste des arguments, les précédents sont des arguments "fixes".
  )

(defun mapcar (fun &rest lists)
  (if (atom list)
	  nil
	(cons (if (atom (cdr lists))
			  (apply fun (caar lists))
			(apply fun (mapcar #'car lists))
			(mapcar fun (mapcar #'cdr lists))))))

(defun last (list)
  (if (atom (cdr list))
	  list
	(last (cdr list))))
