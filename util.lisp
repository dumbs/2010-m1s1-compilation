;; Fonctions utiles
;; Liste de quelques fonctions pratiques de LISP :
;; (rplacd x val) = (setf (cdr x) val)
;; (rplaca x val) = (setf (car x) val)
;; (intersection l1 l2) = évident
;; (acons clé valeur liste-associative) = (cons (cons clé valeur) liste-associative) ;; Ne gère pas les doublons et ne fait pas de setf.
;; (push x liste) = (setf liste (cons x liste))
;; (remove-if-not predicate list) filtre la liste en fonction de predicate.
;; (incf x) incrémente x, (decf x) décrémente x.
;; (loop ......) lire la doc...
;; (subst new old tree) remplace old par new dans tree.

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

(defun n-consp (n l)
  "Détermine s'il y a au moins n cellules dans la liste l."
  (if (<= n 0)
      t
      (and (consp l)
           (n-consp (- n 1) (cdr l)))))

(defun propper-list-p (l)
  (or (null l)
         (and (consp l)
                  (propper-list-p (cdr l)))))

(defun range (a &optional b)
  (cond ((null b) (range 0 a))
        ((> a b) (loop for i from a above b collect i))
        (T (loop for i from a below b collect i))))

(defun shift (n l)
  (if (<= n 0)
      l
    (shift (- n 1) (cdr l))))

(defmacro curry (fun &rest params)
  `(lambda (&rest actual-params)
     (apply ,fun
            ,@(mapcar (lambda (x n)
                        (if (eq :skip x)
                            `(nth ,(- n 1) actual-params)
                          x))
                      params
                      (range (length params)))
            (shift ,(count-if (lambda (x) (eq x :skip)) params)
                   actual-params))))

(defun mload (name)
  (let ((fd (open name)))
    (cons 'progn
          (loop
           for line = (read fd nil 'eof)
           while (not (eq line 'eof))
           collect line
           finally (close fd)
           ))))

(defun m-macroexpand-1 (macro)
  ())

(defmacro get-defun (symb)
  `(get ,symb :defun))

(defun set-defun (symb expr)
  (setf (get-defun (cdaddr li))
        (cdddr li)))

(defmacro get-defmacro (symb)
  `(get ,symb :defmacro))

(defun set-defmacro (li)
  (setf (get-defmacro (cdaddr li))
        (cdddr li)))

(defun mposition (symb list)
  (defun mposition-t (symb list counter)
    (cond ((endp list) nil)
          ((eq symb (car list)) counter)
          ((or (eq (car list) '&optional)
              (eq (car list) '&rest))
          (mposition-t symb (cdr list) counter))
          (T
           (mposition-t symb (cdr list) (+ 1 counter)))))
  (mposition-t symb list 0))

;; TODO : ne copie pas les listes de propriétés des symboles.
;; Vu que ce n'est techniquement pas réalisable, il faut en tenir
;; compte dans les tests unitaires etc.
(defun copy-all (data)
  "Copie récursivement un arbre de listes et de tableaux."
  (print data)
  (cond 
    ((consp data)
     (cons (copy-all (car data))
           (copy-all (cdr data))))
    ((arrayp data)
     (let ((res (make-array (array-dimensions data))))
       (dotimes (i (array-total-size data)) 
         (setf (row-major-aref res i) (copy-all (row-major-aref data i))))
       res))
    ((stringp data)
     (copy-seq data))
    ((null data)
     nil)
    ((symbolp data)
     data)
    ((numberp data)
     data)
    ((characterp data)
     data)
    (t
     (warn "copy-all : Je ne sais pas copier ~w" data))))
