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

(defmacro assoc-set (k v alist &optional (compare #'eq))
  `(let ((my-k ,k)
         (my-v ,v))
     (let ((association (assoc my-k ,alist :test ,compare)))
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
           finally (close fd)))))

(defun m-macroexpand-1 (macro)
  ;; TODO : not implemented yet m-macroexpand-1
  macro ;; Pour éviter le unused variable.
  ())

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
    ((or (null data) (symbolp data) (numberp data) (characterp data) (functionp data))
     data)
    (t
     (warn "copy-all : Je ne sais pas copier ~w" data)
     data)))

(defun flatten (lst &optional rest result)
  (if (endp lst)
      (if (endp rest)
          (reverse result)
          (flatten (car rest) (cdr rest) result))
      (if (listp (car lst))
          (flatten (car lst) (cons (cdr lst) rest) result)
          (flatten (cdr lst) rest (cons (car lst) result)))))

(defun mapcar-append (append function &rest lists)
  (cond ((null lists)
         append)
        ((member nil lists)
         append)
        (t
         (cons (apply function (mapcar #'car lists))
               (apply #'mapcar-append append function (mapcar #'cdr lists))))))

(defun reduce* (initial function &rest lists)
  (if (or (null lists) (member nil lists))
      initial
      (apply #'reduce* (apply function initial (mapcar #'car lists))
             function
             (mapcar #'cdr lists))))

(defun assoc* (item compare &rest alists)
  (if (not (functionp compare))
      (apply #'assoc* item #'eq compare alists)
      (if (endp alists)
          nil
          (or (assoc item (car alists) :test compare)
              (apply #'assoc* item compare (cdr alists))))))

(defun reverse-alist (alist)
  (mapcar (lambda (x) (cons (car x) (reverse (cdr x))))
          alist))

(defun group-1 (lst &optional result)
  "Groupe les éléments d'une lste en fonction de leur premier élément, et renvoie une lste associative"
  (if (endp lst)
      result
      (let ((association (assoc (caar lst) result)))
        (if association
            (push (cdar lst) (cdr association))
            (push (cons (caar lst) (list (cdar lst))) result))
        (group-1 (cdr lst) result))))

(defun group (lst)
  (reverse-alist (group-1 lst)))

(defun find-what-is-used-1 (expr)
  (if (propper-list-p expr)
      (apply #'append (if (symbolp (car expr))
                          (list (car expr))
                          nil)
             (mapcar #'find-what-is-used (cdr expr)))))

(defun find-what-is-used (expr)
  (remove-duplicates (find-what-is-used-1 expr)))
