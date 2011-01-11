;; Main

;; Chargement de tous les fichiers, dans l'ordre du tri topologique
;; pour tous les re-charger, sans les charger deux fois.

;; TODO : mettre de ** autour des variables en defvar.

(load "util")
(load "test-unitaire")
(load "vm")
(load "match")
(load "mini-meval")
(load "squash-lisp")
(load "squash-lisp-1")
(load "equiv-tests")

(provide 'main)
