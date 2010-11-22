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

;; {{{ Barriere-interpretation

;; Un programme Scheme comme LISP decrit deux sortes d'objets: les valeurs non fonctionnelles
;; (entier, bool, liste...) et les valeurs fonctionnelles

;;   {{{ Valeurs-non-fonctionnelles

;; citation-val: Citation -> Valeur/non fonctionnelle/
;; (citation-val cit) rend la valeur de la citation "cit"
(define (citation-val cit)
  (if (pair? cit)
	  (cadr cit)
	  cit))

;;   }}} Valeur-non-fonctionnelles

;;   {{{ Valeur-fonctionnelles

;; Il y a deux type de fonctions, les fonctions predefinies (reconnues par primitive? en Scheme
;; et special-form-p en LISP) et les fonctions du programme en cours d'evaluation (creer par
;; fonction-creation)

;; invocable?: Valeur -> bool
;; (invocable? val) rend vrai ssi "val" est un fonction (primitive ou definie)
(define (invocable? val)
  (if (primitive? val)
	  #t
	  (fonction? val)))

;; invocation: Invocable * LISTE[Valeur] -> Valeur
;; (invocation f args) rend la valeur de l'application de "f" aux elements de "vals"
(define (invocation f args)
  (if (primitive? f)
	  (primitive-invocation f args)
	  (fonction-invocation f args)))

;;     {{{ Primitives

;; Une primitive est implantee par un 4-uplet :
;; - le premier element est le symbole *primitive* (pour les reconnaitre)
;; - le second element est la fonction du Scheme sous-jacent qui implante la primitive,
;; - le troisieme element est un comparateur (= ou >=)
;; - le quatrieme element est un entier naturel, ces deux derniers elements permettant
;;   de specifier l'arite de la primitive.

;; primitive?: Valeur -> bool
;; (primitive? val) rend vrai ssi "val" est une fonction primitive
(define (primitive? val)
  (if (pair? val)
	  (equal? (car val) '*primitive*)
	  #f))

;; primitive-creation: N-UPLET[(Valeur... -> Valeur)(num * num -> bool) num] -> Primitive
;; (primitive-creation f-c-n) rend la primitive implantee par la fonction (du Scheme sous-jacent)
;; "f", le premier element de "f-c-n", et dont l'arite est specifier par le
;; comparateur "c", deuxieme element de "f-c-n" et l'entier "n", le troisieme element
(define (primitive-creation f-c-n)
  (cons '*primitive* f-c-n))

;; primitive-invocation: Primitive * LISTE[Valeur] -> Valeur
;; (primitive-invocation p args) rend la valeur de l'application de la primitive "p" aux element
;; de args
(define (primitive-invocation p args)
  (let ((n       (length args))
		(f       (cadr p))
		(compare (caddr p))
		(arite   (cadddr p)))
	(if (compare n arite)
		(cond
		 ((= n 0) (f))
		 ((= n 1) (f (car args)))
		 ((= n 2) (f (car args) (cadr args)))
		 ((= n 3) (f (car args) (cadr args) (caddr args)))
		 ((= n 4) (f (car args) (cadr args) (caddr args) (cadddr args)))
		 (else
		  (scheme-erreur 'primitive-invocation
						 "limite implantation (arite quelconque < 5)" args)))
		(scheme-erreur 'primitive-invocation
					   "arite incorrecte" args))))

;;     }}} Primitives

;;     {{{ Fonction-meta-definies

;; Une fonction definie est implantee par un 4-uplet:
;; - le premier element est le symbole *fonction* (pour les reconnaitre)
;; - le second element est la liste des variables de la definition de la fonction
;; - le troisieme element est le corps de la definition de la fonction
;; - le dernier element est l'environnement ou est definie la fonction

;; fonction?: Valeur -> bool
;; (fonction? val) rend vrai ssi "val" est une fonction meta-definie
(define (fonction? val)
  (if (pair? val)
	  (equal? (car val) '*fonction*)
	  #f))

;; fonction-invocation: Fonction * LISTE[Valeur] -> Valeur
;; (fonction-invocation f args) rend la valeur de l'application de la fonction
;; meta-definie "f" aux element de "args"
(define (fonction-invocation f args)
  (let ((variables (cadr f))
		(corps     (caddr f))
		(env       (cadddr f)))
	(corps-eval corps (env-extension env variables args))))

;; fonction-creation: Definition * Environnement -> Fonction
;; (fonction-creation definition env) rend la fonction definie par "definition" dans
;; l'environnement "env"
(define (fonction-creation definition env)
  (list '*fonction*
		(definition-variables definition)
		(definition-corps definition)
		env))

;;     }}} Fonctions-meta-definies
;;  }}} Valeurs-fonctionnelles
;; }}} Barriere-interpretation

;; {{{ Environnement-H (barriere de haut niveau)

;; variable-val: Variable * Environnement -> Valeur
;; (variable-val var env) rend la valeur de la variable "var" dans l'environnement "env"
(define (varible-val var env)
  (if (env-non-vide? env)
	  (let ((bloc (env-1er-bloc env)))
		(let ((variables (blocActivation-variables bloc)))
		  (if (member var variables)
			  (blocActivation-val bloc var)
			  (variable-val var (env-reste env)))))
	  (scheme-erreur 'variable-val "variable inconnue" var)))

;; env-extension: Environnement * LISTE[Variable] * LISTE[Valeur] -> Environnement
;; (env-extension env vars vals) rend l'environnement "env" etendu avec un bloc
;; d'activation liant les variables "vars" aux valeurs "vals"
(define (env-extension env vars vals)
  (if (= (length vars) (length vals))
	  (let ((bloc (blocActivation-creation vars)))
		(begin
		  (blocActivation-mettre-valeurs! bloc vals)
		  (env-add bloc env)))
	  (scheme-erreur 'env-extension
					 "arite incorrecte" (list vars vals))))

;; env-add-liaisons: LISTE[Liaison] * Environnement -> Environnement
;; (env-add-liaisons liaisons env) rend l'environnement obtenu en ajoutant a l'environnement
;; "env", les liaisons "liaisons"
(define (env-add-liaisons liaisons env)
  ;; eval-env: Expression -> Valeur
  ;; (eval-env exp) rend la valeur de "exp" dans l'environnement "env"
  (define (eval-env exp)
	(evaluation exp env))
  ;; expression de (env-add-liaisons env) :
  (env-extension
   env
   (meval-scheme-map liaison-variable liaisons)
   (meval-scheme-map eval-env
					 (meval-scheme-map liaison-exp liaisons))))

;; env-enrichissement: Environnement * LISTE[Definition] -> Environnement
;; (env-enrichissement env defs) rend l'environnement "env" etendu avec un bloc
;; d'activation pour les definitions fonctionnelles "defs"
(define (env-enrichissement env defs)
  (let ((noms (meval-scheme-map definition-nom-fonction defs)))
	(let ((bloc (blocActivation-creation noms)))
	  (let ((env-plus (env-add bloc env)))
		(define (fonction-creation-env-plus definition)
		  (fonction-creation definition env-plus))
		(begin
		  (blocActivation-mettre-valeurs!
		   bloc
		   (meval-scheme-map fonction-creation-env-plus defs))
		  env-plus)))))

;;   {{ Environnement-B (barriere de bas niveau)

;; Les environnement sont representes par la structure de donnees
;; LISTE[BlocActivation]

;; env-vide: -> Environnement
;; (env-vide) rend l'environnement vide
(define (env-vide) '())

;; env-non-vide?: Environnement -> bool
;; (env-non-vide? env) rend #t ssi l'environnement "env" n'est pas vide
(define (env-non-vide? env)
  (pair? env))

;; env-add: Environnement * BlocActivation -> Environnement
;; (env-add bloc env) rend l'environnement obtenu en ajoutant devant
;; l'environnement "env" le bloc d'activation "bloc"
(define (env-add bloc env)
  (cons bloc env))

;; env-1er-bloc: Environnement -> BlocActivation
;; (env-1er-bloc env) rend le premier bloc (le dernier dans la liste) d'activation
;; de l'environnement "env"
;; ERREUR lorsque l'environnement donnee est vide
(define (env-1er-bloc env)
  (car env))

;; env-reste: Environnement -> Environnement
;; (env-reste env) rend l'environnement obtenu en supprimant le premier bloc d'activation
;; de l'environnement "env"
;; ERREUR lorsque l'environnement donnee est vide
(define (env-reste env)
  (cdr env))

;;     {{{ Bloc d'activation

;; Les blocs d'activation sont representes par la structure de donnees:
;; VECTEUR[LISTE[Variables] Valeur ...]

;; blocActivation-variables: BlocActivation -> LISTE[Variable]
;; (blocActivation-variables bloc) rend la liste des variables definies
;; dans le bloc d'activation "bloc"
(define (blocActivation-variables bloc)
  (vector-ref bloc 0))

;; blocActivation-val: BlocActivation * Variable -> Valeur
;; (blocActivation-val bloc val) rend la valeur de la variable "var" dans le bloc
;; d'activation "bloc"
;; HYPOTHESE: "var" est une variable definie dans le "bloc"
(define (blocActivation-val bloc val)
  (let ((i (rang var (blocActivation-variables bloc))))
	(vector-ref bloc i)))

;; blocActivation-creation: LISTE[Variable] -> BlocActivation
;; (blocActivation-creation vars) rend un bloc d'activation contenant la liste des variables
;; "vars" avec la place qu'il faut pour les valeurs de ces variables, cette place n'etant pas
;; remplie
(define (blocActivation-creation vars)
  (let ((bloc (make-vector (+ 1 (length vars)))))
	(begin
	  (vector-set! bloc 0 vars)
	  bloc)))

;; blocActivation-mettre-valeurs!: BlocActivation * LISTE[Valeur] -> Rien
;; (blocActivation-mettre-valeurs! bloc vals) affecte les valeurs "vals" (donnees sous forme de
;; liste) dans le bloc d'activation "bloc".
(define (blocActivation-mettre-valeurs! bloc vals)
  ;; remplir!: entier * LISTE[Valeur] -> Rien
  ;; (remplir! i vals) remplit les cases du vecteur "bloc" a partir de l'indice "i", avec les valeurs
  ;; de la liste "vals" (et dans le meme ordre)
  (define (remplir! i vals)
	(if (pair? vals)
		(begin
		  (vector-set! bloc i (car vals))
		  (remplir! (+ i 1) (cdr vals)))))
  (remplir! 1 vals))

;;     }}} Bloc d'activation
;;   }}} Environnement-B (barriere de bas niveau

;;   {{{ Environnement-initial

;; L'environnement initial est compose des primitives.
;; Pour faciliter la description des differentes primitives (et par manque de temps :D)
;; on les ecrira sous forme d'une liste de description de primitive.
;; Une element du type DescriptionPrimitive est une description d'une primitive. C'est
;; une liste dont :
;; - le premier element est la variable representant la primitive consideree
;; - les trois autres elements sont les trois elements qui decrivent
;; l'implantation de la primitive (la fonction Scheme, le comparateur, et l'arite)

;; env-initial -> Environnement
;; (env-initial) rend l'environnement initial (l'environnement qui contient toutes les primitives.
(define (env-initial)
  (env-extension
   (env-vide)
   (meval-scheme-map car (descriptions-primitves))
   (meval-scheme-map primitive-creation
					 (meval-scheme-map cdr
									   (descriptions-primitives)))))

;; description-primitive: Variable * (Valeur ... -> Valeur) * (num * num -> bool) * num -> DescriptionPrimitive
;; (description-primitive var f comparator arite) rend la description de la primitive designee par "var" implantee
;; dans Scheme par "f" et dont l'arite est definie par "comparator" et "arite"
(define (description-primitive var f comparator arite)
  (list var f comparator arite))

;; descriptions-primitives: -> LISTE[DescriptionPrimitive]
;; (descriptions-primitives) rend la liste des descriptions de toutes les primitives
(define (descriptions-primitives)
  (cons (description-primitive 'car             car              =   1)
  (cons (description-primitive 'cdr             cdr              =   1)
  (cons (description-primitive 'cons            cons             =   2)
  (cons (description-primitive 'list            list             >=  0)
  (cons (description-primitive 'vector-length   vector-length    =   1)
  (cons (description-primitive 'vector-ref      vector-ref       =   2)
  (cons (description-primitive 'vector-set!     vector-set!      =   3)
  (cons (description-primitive 'make-vector     make-vector      =   1)
  (cons (description-primitive 'pair?           pair?            =   1)
  (cons (description-primitive 'symbol?         symbol?          =   1)
  (cons (description-primitive 'number?         number?          =   1)
  (cons (description-primitive 'string?         string?          =   1)
  (cons (description-primitive 'boolean?        boolean?         =   1)
  (cons (description-primitive 'vector?         vector?          =   1)
  (cons (description-primitive 'char?           char?            =   1)
  (cons (description-primitive 'equal?          equal?           =   2)
  (cons (description-primitive '+               +                >=  0)
  (cons (description-primitive '-               -                =   2)
  (cons (description-primitive '*               *                >=  0)
  (cons (description-primitive '/               /                =   2)
  (cons (description-primitive '=               =                =   2)
  (cons (description-primitive '<               <                =   2)
  (cons (description-primitive '>               >                =   2)
  (cons (description-primitive '<=              <=               =   2)
  (cons (description-primitive '>=              >=               =   2)
  (cons (description-primitive 'remainder      remainder         =   2)
  (cons (description-primitive 'display        display           =   1)
  (cons (description-primitive 'newline        newline           =   0)
  (cons (description-primitive 'read           read              =   0)
  (cons (description-primitive 'erreur         erreur            >=  2)
		(list))))))))))))))))))))))))))))))))

;;   }}} Environnement-initial
;; }}} Environnement-H (barriere de haut niveau)
