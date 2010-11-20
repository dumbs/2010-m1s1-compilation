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

;; {{{ Utilitaire generaux
;;
;; Necessaires pour l'auto-amorcage (on pourrait egalement les placer
;; dans l'environnement initial)

;; Signaler une erreur et abandonner l'evaluation
(define (scheme-erreur fn message donnee)
  (erreur 'meval-scheme fn message donnee))

;; cadr: LISTE[alpha]/au moins 2 termes/ -> alpha
;; (cadr L) rend le second terme de la liste "L"
(define (cadr L)
  (car (cdr L)))

;; cddr: LISTE[alpha]/au moins 2 termes/ ->LISTE[alpha]
;; (cddr L) rend la liste "L" privee de ses deux premiers termes
(define (cddr L)
  (cdr (cdr L)))

;; caddr: LISTE[alpha]/au moins 3 termes/ -> alpha
;; (caddr L) rend le troisieme terme de la liste "L"
(define (caddr L)
  (car (cdr (cdr L))))

;; cdddr: LISTE[alpha]/au moins 3 termes/ -> LISTE[alpha]
;; (cdddr L) rend la liste "L" privee de ses trois premiers termes
(define (cdddr L)
  (cdr (cdr (cdr L))))

;; cadddr: LISTE[alpha]/au moins 4 termes/ -> alpha
;; (cadddr L) rend le quatrieme terme de la liste "L"
(define (cadddr L)
  (car (cdr (cdr (cdr L)))))

;; length: LISTE[alpha] -> entier
;; (length L) rend la longueur de la liste "L"
(define (length L)
  (if (pair? L)
	  (+ 1 (length (cdr L)))
	  0))

;; meval-scheme-map: (alpha -> beta) * LISTE[alpha] -> LISTE[beta]
;; (meval-scheme-map f L) rend la liste des valeurs de "f" appliquee
;; aux termes de la liste "L"
(define (meval-scheme-map f L)
  (if (pair? L)
	  (cons (f (car L)) (meval-scheme-map f (cdr L)))
	  '()))

;; member: alpha * LISTE[alpha] -> LISTE[alpha] + #f
;; (member e L) rend le suffixe de "L" debutant par la premiere
;; occurence de "e" ou #f si "e" n'apparait pas dans "L"
(define (member e L)
  (if (pair? L)
	  (if (equal? e (car L))
		  L
		  (member e (cdr L)))
	  #f))

;; rang: alpha * LISTE[alpha] -> entier
;; (rang e L) rend le rang de l'element donne dans la liste "L"
;; (ou on sait que l'element apparait). Le premier element a pour rang 1.
(define (rang e L)
  (if (equal? e (car L))
	  1
	  (+ 1 (rang e (cdr L)))))

;; }}} Utilitaire generaux
