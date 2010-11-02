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
