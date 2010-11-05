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

(defun range (a &optional b)
  (cond ((null b) (range 0 a))
        ((> a b) (loop for i from a above (- b 1) collect i))
        (T (loop for i from a to b collect i))))

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

(defun foo (x y z) (list x y z))
(defvar cfoo nil)
(setq cfoo (curry #'foo 1 2))
(defvar cfoo2 nil)
(setq cfoo2 (curry #'foo :skip 2))