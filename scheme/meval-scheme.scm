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

;; {{{ Evaluateur

;; meval-scheme: Expression -> Valeur
;; (meval-scheme e) rend la valeur de l'expression "e"
(define (meval-scheme e)
  (evaluation e (env-initial)))

;; evaluation: Expression * Environnement -> Valeur
;; (evaluation expr env) rend la valeur de l'expression "expr" dans l'environnement "env"
(define (evaluation expr env)
  ;; (discrimine l'expression et invoque l'evaluateur specialise)
  (cond
   ((variable? expr) (variable-val expr env))
   ((citation? expr) (citation-val expr env))
   ((alternative? expr) (alternative-eval
						 (alternative-condition expr)
						 (alternative-consequence expr)
						 (alternative-alternant expr) env))
   ((conditionnelle? expr) (conditionnelle-eval
							(conditionnelle-clauses expr) env))
   ((sequence? expr) (sequence-eval (sequence-exprs expr) env))
   ((bloc? expr) (bloc-eval (bloc-liaisons expr)
							(bloc-corps expr) env))
   ((application? expr) (application-eval
						 (application-fonction expr)
						 (application-arguments expr) env))
   (else
	(scheme-erreur 'evaluation "pas un programme" expr))))

;; alternative-eval: Expression3 * Environnement -> Valeur
;; (alternative-eval condition consequence alternant env) rend la valeur de
;; l'expression "(if condition consequence alternant)" dans l'environnement "env"
(define (alternative-eval condition consequence alternant)
  (if (evaluation condition env)
	  (evaluation consequence env)
	  (evaluation alternant env)))

;; conditionnelle-eval: LISTE[Clause] -> Expression
;; (conditionnelle-eval clauses env) rend la valeur, dans l'environnement "env",
;; de l'expression "(cond c1 c2 ... cn)".
(define (conditionnelle-eval clauses env)
  (evaluation (cond-expansion clauses) env))

;; cond-expansion: LISTE[Clause] -> Expression
;; (cond-expansion clauses) rend l'expression, ecrite avec des alternatives,
;; equivalente a l'expression "(cond c1 c2 .. cn)".
(define (cond-expansion clauses)
  (if (pair? clauses)
	  (let ((first-clause (cadr clauses)))
		(if (equal? (clause-condition first-clause) 'else)
			(cons 'begin (clause-expressions first-clause))
			(cons 'if
				  (cons
				   (clause-condition first-clause)
				   (cons
					(cons 'begin (clause-expressions first-clause))
					(let ((seq (cond-expansion (cdr clauses))))
					  (if (pair? seq)
						  (list seq)
						  seq)))))))
	  '()))

;; sequence-eval: LISTE[Expression] * Environnement -> Valeur
;; (sequence-eval exprs env) rend la valeur, dans l'environnement "env",
;; de l'expression "(begin e1 ... en)". (Il faut evaluer tour a tour les
;; expressions et rendre la valeur de la derniere expression.)
(define (sequence-eval exprs env)
  ;; sequence-eval+: LISTE[Expression]/non vide/ -> Valeur
  ;; meme fonction, sachant que la liste "exprs" n'est pas vide et en globalisant
  ;; la variable "env"
  (define (sequence-eval+ exprs)
	(if (pair? (cdr exprs))
		(begin (evaluation (car exprs) env)
			   (sequence-eval+ (cdr exprs)))
		(evaluation (car exprs) env)))
  ;; expression de (sequence-eval exprs env):
  (if (pair? exprs)
	  (sequence-eval+ exprs)
	  #f))

;; application-eval: Expression * LISTE[Expression] * Environnement -> Valeur
;; (application-eval exp-fn args env) rend la valeur de l'invocation de
;; l'expression "exp-fn" aux arguments "args" dans l'environnement "env"
(define (application-eval exp-fn args env)
  ;; eval-env : Expression -> Valeur
  ;; (eval-env expr) rend la valeur de "expr" dans l'environnement "env"
  (define (eval-env expr)
	(evaluation expr env))
  ;;expression de (application-eval exp-fn args env) :
  (let ((f (evaluation exp-fn env)))
	(if (invocable? f)
		(invocation f (meval-scheme-map eval-env args))
		(scheme-erreur 'application-eval
					   "pas une fonction" f))))

;; bloc-eval: LISTE[Liaison] * Corps * Environnement -> Valeur
;; (bloc-eval liaisons corps env) rend la valeur, dans l'environnement "env"
;; de l'expression "(let liaisons corps)"
(define (bloc-eval liaisons corps env)
  (corps-eval corps (env-add-liaisons liaisons env)))

;; corps-eval: Corps * Environnement -> Valeur
;; (corps-eval corps env) rend la valeur de "corps" dans l'environnement "env"
(define (corps-eval corps env)
  (let ((def-exp (corps-separation-defs-exps corps)))
	(let ((defs (car def-exp))
		  (exps (cadr def-exp)))
	  (sequence-eval exps (env-enrichissement env defs)))))

;; corps-separation-defs-exps: Corps -> COUPLE[LISTE[Definition] LISTE[Expression]]
;; (corps-separation-defs-exps corps) rend un couple dont le premier elements est
;; la liste des definitions du corps "corps" et le second element est la liste des
;; expressions de ce corps
(define (corps-separation-defs-exps corps)
  (if (definition? (car corps))
	  (let ((def-exp-cdr
			  (corps-separation-defs-exps (cdr corps))))
		(cons (cons (car corps)
					(car def-exp-cdr))
			  (cdr def-exp-cdr)))
	  (list '() corps)))

;; }}} Evaluateur

