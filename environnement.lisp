;; gestion de l'environnement
(defvar exemple-env-stack 
  '(((x . plop)) ;; Environnement le plus bas. (derriere definition)
    ((y . "#lambda") 
     (x . "bijour")
     (z . 123))
    ((y . 56)
     (x . 42)
     (foo . "#lambda")))) ;; Top-level. Environnement le plus haut (premiere definition)

;; '((...) (...) (...)) => 3 environnement dans env-stack

(defun empty-env-stack () '(())) ;; Constructeur de la pile d'environnement

(defun push-new-env (env-stack) ;; env-stack en parametre = toute la pile.
  (cons '() env-stack))

(defun add-binding (env-stack name value) ;; ajoute une definition
  (setf (car env-stack)
	(cons (cons name value)
	      (car env-stack)))
  env-stack
  )

(defun set-binding (env-stack name new-value) ;; modifie une definition
  (setf (cdr (get-binging name))
	new-value)
  env-stack
  )

(defun get-binding-value (env-stack name) ;; recupere la valeur associer a name
  (cdr (get-binding name)))

(defun get-binding (env-stack name) ;; recupere l'association correspondant a name
  (if (atom env-stack)
      nil ;; TODO : Penser a peut etre mettre un warn ou error
  (let ((ass (assoc name (car env-stack))))
    (if ass ass
      (get-binding (cdr env-stack) name))))
  )

(defun top-level (env-stack) ;; Recupere la pile d'environnement contenant unique l'environnement top-level
  (if (atom (cdr env-stack))
      env-stack
    (top-level (cdr env-stack)))
)

(defun add-top-level-binding (env-stack name value) ;; ajoute une definition
  (add-binding (top-level env-stack) name value)
  )

(defun set-top-level-binding (env-stack name new-value) ;; modifie une definition
  (set-binding (top-level env-stack) name new-value)
  )

(defun test-env (num)
  (case num
	(0 (push-new-env (empty-env-stack)))
	(1 (push-new-env exemple-env-stack))
	(2 (add-binding (push-new-env (empty-env-stack)) 'x 42))
	(3 (add-binding (add-binding (push-new-env (empty-env-stack)) 'x 42) 'y 56))
	))

(test-env 0)