;; Main

;; Chargement de tous les fichiers, dans l'ordre du tri topologique
;; pour tous les re-charger, sans les charger deux fois.

(load "util")
(load "test-unitaire")
(load "instructions")
(load "match")
(load "lisp2li")
(load "meval")
(load "implementation/mini-meval")

(provide 'main)