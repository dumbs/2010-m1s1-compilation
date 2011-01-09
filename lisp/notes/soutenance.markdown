Choses à ne pas oublier à dire lors de la soutenance (keywords: compilation lafourcade um2 m1)

= lisp2li =
On a choisi de ne pas utiliser le li du prof, mais au contraire d'utiliser du LISP simplifié qui est donc exécutable donc on a pu faire des
tests unitaires pour voir si la transformation préservait le sens du programme.

= tests unitaires =
'test de régression': on a pu modifier des morceaux de code qui avait déjà les tests et voir que ça tournait toujours (après modifs)
framework extensible : possibilité d'ajouter de nouveaux types de tests (ex. deftest-error, deftest-equiv)
séparation des tests en modules / sous-modules, possibilité de lancer des tests de certains (sous-)modules seulement.
contabilisation du nombre de tests, d'erreurs, etc.

= mini-meval =
C'est un méta-évaluateur 'naïf' mais qui supporte pratiquement tout LISP sauf CLOS (Common Lisp Object System)

= syntaxe supportée =
* Le 'eval-when'
* Les 'tagbody' et 'go', les 'throw' et 'catch', les 'block' et 'return' et 'return-from' et le 'unwind-protect'
* Les 'let', 'let*', 'flet', 'labels', 'macrolet', 
* Les 'macro'
* Les 'lambda' avec capture de variables
* Dans la 'lambda-list': '&optional', '&rest', '&key', '&allow-other-keys', '&aux', mais pas les '&body'
* Plus progn, if, #', quote, etc.

= squash-lisp =
* En 3 passes :
  * Passe 1 :
    * macro-expansion (on utilise mini-meval) et eval-when
    * simplification globale de la syntaxe :
      * (let (a (b 2) c) (list a b c)) -> (let ((a nil) (b 2) (c nil)) (list a b c))
      * unification de la syntaxe des let, let*, flet, labels.
      * tous les appels de fonction sont transformés en funcall.
    * ré-écriture des tagbody/go, throw/catch, block/return(-from) en termes de unwind/unwind-protect, jump/jump-label + variations.
    * noms uniques pour les jump/jump-label.
    * toutes les constantes sont emballées dans (quote).
    * toutes les lectures de variables sont emballées dans (get-var)
  * Passe 2 :
    * noms uniques pour les variables locales (il n'y a donc plus besoin de connaître l'imbrication des environnements)
    * toutes les déclarations de variables locales (let,let*,flet,labels,lambda) sont transformées en simple-let
    * le simple-let ne fait que déclarer, il n'afecte pas de valeurs :
      (let ((a nil) (b 2)) (list a b)) -> (simple-let (a b) (setq a nil) (setq b 2) (list a b))
    * simplification de la lambda-list (élimination de &optional &rest &key &allow-other-keys &aux)
    * suppression des paramètres de la lambda :
      (lambda (x y) (+ x y)) -> (simple-lambda (simple-let (x y) (setq x (get-param 0)) (setq y (get-param 1)) (+ x y)))
  * Passe 3
	* On lorsqu'une variable à l'intérieur d'un lambda référence une déclaration à l'extérieur du lambda, on la marque comme étant capturée.
	* On fusionne tous les let d'une lambda en les remontant dans un let à la racine de la lamdba.
    * On sort toutes les lambdas (fonctions anonymes), on les nomme avec un symbole unique, et on les met au top-level.

= match =
* on a fait une fonction de pattern matching qui utilise une syntaxe proche de expressions régulières pour reconnaître des expressions LISP
  qui ont une certaine forme / motif . On peut 'capturer' des portions de ce motif dans des variables qui sont accessibles dans le corps des
  match.
* On a aussi 'cond-match' qui est un dérivé d'un 'case' mais où chaque test est un match sur une expression.
* On a aussi un 'match-automaton' qui est une sorte d'automate où les transitions ne sont pas des lettres mais des motifs. On avance dans
  une liste en sélectionnant à chaque fois la première transition qui match avec l'élément en cours.  On peut 'sauvegarder' des données à
  chaque transition qui seront mises côte à côte dans une liste.
* defmatch permet de définir une famille de fonctions, dont une seule est exécutée à l'appel (celle qui match avec le(s?) paramètres).
* Pas d'optimisation à la compilation de quelle fonction match : la syntaxe de match est assez lourde, et vu qu'on ne match pas sur la forme
  de l'appel de fonction, mais sur les valeurs passées en paramètre, l'optimisation n'est pas faisable 90% du temps.

= compilation =
* On compile vers de l'assembleur assez proche de l'x86. On avait déjà un système d'exploitation assez simple qui tournait de manière
  autonome sur un PC. On a donc stotché ensemble une REPL compilée et le SE de manière à avoir une REPL autonome qui ne dépend de rien.
  le système d'exploitation sous-jacent fournit juste de quoi faire des entrées-sorties.

= garbage collector =
* gestion du tas
* on a un gc très simpliste qui copie les données d'une zone de la mémoire vers une autre et vice versa à chaque fois qu'elle est pleine. La
  raison est que les autres gc nécessite une occupation variable de la mémoire pour l'exécution du gc qui peut être aussi grosse que la
  mémoire occupée (O(n)).

= fonctions LISP =
* On a notre propre fonction 'read' et notre propre fonction 'format' pour être autonomes.
