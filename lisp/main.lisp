;; Main

;; Chargement de tous les fichiers, dans l'ordre du tri topologique
;; pour tous les re-charger, sans les charger deux fois.

(setq *load-verbose* t)

(load "util")
(load "test-unitaire")
(load "vm")
(load "match")
(load "mini-meval")
(load "squash-lisp-1")
(load "squash-lisp-3")
(load "squash-lisp")
(load "compilation")
(load "equiv-tests")
(load "../implementation/meval-op")

(provide 'main)
