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

;; {{{ Barriere-syntaxique
;;
;; Ces fonctions permettent de manipuler les differentes expression syntaxiques
;; dont Scheme est forme. Pour chacune de ces differentes formes syntaxiques, on
;; trouve le reconnaisseur et les accesseurs.

;; variable?: Expression -> bool
(define (variable? expr)
  (if (symbol? expr)
	  (cond ((equal? expr 'cond)   #f)
			((equal? expr 'else)   #f)
			((equal? expr 'if)     #f)
			((equal? expr 'quote)  #f)
			((equal? expr 'begin)  #f)
			((equal? expr 'let)    #f)
			((equal? expr 'let*)   #f)
			((equal? expr 'define) #f)
			((equal? expr 'or)     #f)
			((equal? expr 'and)    #f)
			(else                  #t))
	  #f))

;; citation?: Expression -> bool
(define (citation expr)
  (cond ((number? expr)  #t)
		((char? expr)    #t)
		((string? expr)  #t)
		((boolean? expr) #t)
		((pair? expr)    (equal? (car expr) 'quote))
		(else            #f)))

;; conditionnelle?: Expression -> bool
(define (conditionnelle? expr)
  (if (pair? expr)
	  (equal? (car expr) 'cond)
	  #f))

;; conditionnelle-clauses: Conditionnelle -> LISTE[Clause]
(define (conditionnelle-clause cond)
  (cdr cond))

;; alternative?: Expression -> bool
(define (alternative? expr)
  (if (pair? expr)
	  (equal? (car expr) 'if)
	  #f))

;; alternative-condition: Alternative -> Expression
(define (alternative-condition alt)
  (cadr alt))

;; alternative-consequence: Alternative -> Expression
(define (alternative-consequence alt)
  (caddr alt))

;; alternative-alternant: Alternative -> Expression
(define (alternative-alternant alt)
  (if (pair? (cdddr alt))
	  (cadddr alt)
	  #f))

;; sequence?: Expression -> bool
(define (sequence? expr)
  (if (pair? expr)
	  (equal? (car expr) 'begin)
	  #f))

;; sequence-exprs: Expression -> LISTE[Expression]
(define (sequence-exprs expr)
  (cdr expr))

;; bloc?: Expression -> bool
(define (bloc? expr)
  (if (pair? expr)
	  (equal? (car expr) 'let)
	  #f))

;; bloc-liaisons: Bloc -> LISTE[Liaison]
(define (bloc-liaisons bloc)
  (cadr bloc))

;; bloc-corps: Bloc -> Corps
(define (bloc-corps bloc)
  (cddr bloc))

;; application?: Expression -> bool
(define (application? expr)
  (pair? expr))

;; application-fonction: Application -> Expression
(define (application-fonction app)
  (car app))

;; application-arguments: Application -> LISTE[Expression]
(define (application-arguments app)
  (cdr app))

;; clause-condition: Clause -> Expression
(define (clause-condition clause)
  (car clause))

;; clause-expressions: Clause -> LISTE[Expression]
(define (clause-expressions clause)
  (cdr clause))

;; liaison-variable: Liaison -> Variable
(define (liaison-variable liaison)
  (car liaison))

;; liaison-expr: Liaison -> Expression
(define (liaison-expr liaison)
  (cadr liaison))

;; definition?: Corps -> bool
;; (definition? corps) rend #t ssi le premier elements de "corps" est une definition
(define (definition? corps)
  (if (pair? corps)
	  (equal? (car corps) 'define)
	  #f))

;; definition-nom-fonction: Definition -> Variable
(define (definition-nom-fonction def)
  (car (cadr def)))

;; definition-variables: Definition -> LISTE[Variable]
(define (definition-variables def)
  (cdr (cadr def)))

;; definition-corps: Definition -> Corps
(define (definition-corps def)
  (cddr def))

;; }}} Barriere-syntaxique

