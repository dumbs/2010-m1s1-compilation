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
;;  - L'environnement top-level est partage par tous le monde

;; Exemple de la structure env-stack après création de deux
;; environnements en plus du top-level et ajout de plusieurs laisons.
(load "test-unitaire")
(deftestvar environnement exemple-env-stack 
  '(;; Environnement le plus bas (dernières définitions par ordre
    ;; chronologique).
    ("DEFUN"
     (x . plop))
    ;; Un autre environnement (définitions "plus vieilles").
    ("LET"
     (y . "#lambda")
     (x . "bijour")
     (z . 123))
    ;; Top-level. Environnement le plus haut (définitions "globales"
    ;; faites avec defun, defvar etc.).
    ("TOP-LEVEL"
     (y . 56)
     (x . 42)
     (foo . "#lambda"))))

;; '((...) (...) (...)) => 3 environnement dans env-stack

(defun empty-env-stack ()
  "Constructeur de la pile d'environnements."
  (list (list "TOP-LEVEL")))

(defun push-new-env (env-stack name)
  "Crée un nouvel environnement, l'ajoute à ENV-STACK et renvoie la
version modifiée (sans altérer l'original).
Le paramètre ENV-STACK est toute la pile d'environnements."
  (cons (list name) env-stack))

(defun add-binding (env-stack name value)
  "Ajoute une liaison au dernier environnement (le plus bas)."
  (setf (cdar env-stack)
        (cons (cons name value)
              (cdar env-stack)))
  env-stack)

(defun get-binding (env-stack name)
  "Récupère la liaison correspondant à NAME ."
  (if (atom env-stack)
      nil ; TODO : Penser à peut-être mettre un warn ou error.
  (let ((ass (assoc name (cdar env-stack))))
    (if ass ass
      (get-binding (cdr env-stack) name)))))

(defun set-binding (env-stack name new-value)
  "Modifie la valeur associée à une liaison."
  (setf (cdr (get-binding env-stack name))
	new-value)
  env-stack)

(defun get-binding-value (env-stack name)
  "Récupère la valeur associée a NAME ."
  (cdr (get-binding env-stack name)))

(defun top-level-env-stack (env-stack)
  "Recupere la pile d'environnement contenant uniquement
l'environnement top-level"
  (if (atom (cdr env-stack))
      env-stack
    (top-level-env-stack (cdr env-stack))))

(defun add-top-level-binding (env-stack name value)
  "Ajoute une liaison \"globale\" à l'environnement top-level."
  (add-binding (top-level-env-stack env-stack) name value)
  env-stack)

(defun set-top-level-binding (env-stack name new-value) ;; modifie une definition
  "Modifie la valeur associée à une liaison \"globale\" de
l'environnement top-level."
  (set-binding (top-level-env-stack env-stack) name new-value)
  env-stack)

(defun print-env-stack (env-stack)
  (let ((*print-circle* t))
    (if (atom env-stack)
        nil
      (progn (format t "~&~a: " (caar env-stack))
             (mapcar (lambda (b) (format t "~&   ~w = ~w" (car b) (cdr b)))
                     (cdar env-stack))
             (print-env-stack (cdr env-stack))))))

;(defun print-env-stack (env-stack)
;  (if (atom env-stack)
;      nil
;    (progn (format t "~&~a: " (caar env-stack))
;           (mapcar (lambda (b) (format t "~&   ~w = ~w" (car b) (cdr b)))
;                   (cdar env-stack))
;           (print-env-stack (cdr env-stack)))))

;;Test Unitaire
(deftest environnement
  (push-new-env (empty-env-stack) "TEST")
  '(("TEST") ("TOP-LEVEL")))
(deftest environnement
  (push-new-env exemple-env-stack "TEST")
  (cons '("TEST") exemple-env-stack))
(deftest environnement
  (add-binding (empty-env-stack) 'x 42)
  '(("TOP-LEVEL" (x . 42))))
(deftest environnement
  (add-binding (push-new-env (empty-env-stack) "FOO-BAR") 'x 42)
  '(("FOO-BAR" (x . 42)) ("TOP-LEVEL")))
(deftest environnement
  (add-binding (add-binding (empty-env-stack) 'x 42) 'y 56)
  '(("TOP-LEVEL" (y . 56) (x . 42))))
;; TODO : Rajouter un test d'erreur => Georges!!!!!!
;(deftest environnement (set-binding (empty-env-stack) 'x 42) nil)
(deftest environnement
  (set-binding (add-binding (empty-env-stack) 'x 42) 'x .42)
  '(("TOP-LEVEL" (x . .42))))
(deftest environnement
  (get-binding '(("TOP-LEVEL" (X . 42)))
               'x)
  '(x . 42))
(deftest environnement
  (get-binding-value '(("FOO" (Z . 42)) ("TOP-LEVEL" (x . 42)))
                     'x)
  42)
(deftest environnement
  (top-level-env-stack '(("BAR" (X . 42))
                         ("TOP-LEVEL" (X . 24) (Z . 73))))
  '(("TOP-LEVEL" (X . 24) (Z . 73))))
(deftest environnement
  (add-top-level-binding (copy-seq '(("TEST" (X . 42)) ("TOP-LEVEL" (Y . 56))))
                         'Z 78)
  '(("TEST" (X . 42)) ("TOP-LEVEL" (Z . 78) (Y . 56))))
(deftest environnement
  (set-top-level-binding (copy-seq '(("LEVEL2" (X . 42)) ("TOP-LEVEL" (Y . 56))))
                         'Y "42")
  '(("LEVEL2" (X . 42)) ("TOP-LEVEL" (Y . "42"))))
