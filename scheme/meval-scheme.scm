;; {{{ Grammaire du langage
;; Le langage interprete est defini par la grammaire suivante :
;; meval-scheme := expression
;;   expression := variable
;;              |  constante  | (QUOTE donnee)                    ; citation
;;              |  (COND clause *)                                ; conditionnelle
;;              |  (IF condition consequence [alternant])         ; alternative
;;              |  (BEGIN expression*)                            ; sequence
;;              |  (LET (liaison*) corps)                         ; bloc
;;              |  (fonction argument*)                           ; application
;;    condition := expression
;;  consequence := expression
;;    alternant := expression
;;       clause := (condition expressin*)
;;              |  (ELSE expression*)
;;     fonction := expression
;;     argument := expression
;;    constante := nombre | chaine | booleen | caractere
;;       donnee := constante
;;              |  symbole
;;              |  (donnee*)
;;      liaison := (variable expression)
;;        corps := definition* expression expression*
;;   definition := (DEFINE (nom-fonction variable*) corps)
;; nom-fonction := variable
;;     variable := tous les symboles de Scheme autres que les mots-cles
;;      symbole := tous les symboles de Scheme
;;   }}} Grammaire du langage