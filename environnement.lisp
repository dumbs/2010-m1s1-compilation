;; Gestion de l'environnement

;; Attention :
;;  - Lorsqu'on fait un setf, ça renvoie la valeur affectée, pas
;;    l'objet modifié. Si on veut le renvoyer, il faut explicitement
;;    le mettre après le setf.
;;  - Les environnements sont partagés entre toutes les clôtures et
;;    autres qui les utilisent. Par ex. :
;;      (let ((x 0))
;;         (lambda () (setf (x) (+ x 1)))
;;         (lambda () x))
;;    Dans ce cas, les deux lambdas ont accès au même x, dans le même
;;    environnement. La modification par l'une des fonctions se
;;    répercute sur la valeur accédée par l'autre.
;;  - Lorsqu'on définit une fonction, il faut mettre juste après la
;;    liste des paramètres une chaîne de caractères qui documente la
;;    fonction (une docstring).


;; Exemple de la structure env-stack après création de deux
;; environnements en plus du top-level et ajout de plusieurs laisons.
(defvar exemple-env-stack 
  '(;; Environnement le plus bas (dernières définitions par ordre
    ;; chronologique).
    ((x . plop))
    ;; Un autre environnement (définitions "plus vieilles").
    ((y . "#lambda")
     (x . "bijour")
     (z . 123))
    ;; Top-level. Environnement le plus haut (définitions "globales"
    ;; faites avec defun, defvar etc.).
    ((y . 56)
     (x . 42)
     (foo . "#lambda"))))

;; '((...) (...) (...)) => 3 environnement dans env-stack

(defun empty-env-stack ()
  "Constructeur de la pile d'environnements."
  '(()))

(defun push-new-env (env-stack)
  "Crée un nouvel environnement, l'ajoute à ENV-STACK et renvoie la
version modifiée (sans altérer l'original).
Le paramètre ENV-STACK est toute la pile d'environnements."
  (cons '() env-stack))

(defun add-binding (env-stack name value)
  "Ajoute une liaison au dernier environnement (le plus bas)."
  (setf (car env-stack)
	(cons (cons name value)
	      (car env-stack)))
  env-stack)

(defun set-binding (env-stack name new-value)
  "Modifie la valeur associée à une liaison."
  (setf (cdr (get-binging name))
	new-value)
  env-stack)

(defun get-binding-value (env-stack name)
  "Récupère la valeur associée a NAME ."
  (cdr (get-binding name)))

(defun get-binding (env-stack name)
  "Récupère la liaison correspondant à NAME ."
  (if (atom env-stack)
      nil ; TODO : Penser à peut-être mettre un warn ou error.
  (let ((ass (assoc name (car env-stack))))
    (if ass ass
      (get-binding (cdr env-stack) name)))))

(defun top-level (env-stack)
  "Recupere la pile d'environnement contenant uniquement
l'environnement top-level"
  (if (atom (cdr env-stack))
      env-stack
    (top-level (cdr env-stack))))

(defun add-top-level-binding (env-stack name value)
  "Ajoute une liaison \"globale\" à l'environnement top-level."
  (add-binding (top-level env-stack) name value))

(defun set-top-level-binding (env-stack name new-value) ;; modifie une definition
  "Modifie la valeur associée à une liaison \"globale\" de
l'environnement top-level."
  (set-binding (top-level env-stack) name new-value))

(defun test-env (num)
  (case num
	(0 (push-new-env (empty-env-stack)))
	(1 (push-new-env exemple-env-stack))
	(2 (add-binding (push-new-env (empty-env-stack)) 'x 42))
	(3 (add-binding (add-binding (push-new-env (empty-env-stack)) 'x 42) 'y 56))
	))

(test-env 0)
