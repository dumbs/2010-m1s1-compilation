Tests unitaires
===============
* «tests de régression» : on a pu modifier des morceaux de code qui avait déjà les tests et voir que ça tournait toujours (après modifs)
* framework extensible : possibilité d'ajouter de nouveaux types de tests (ex. `deftest-error`, `deftest-equiv`)
* séparation des tests en modules / sous-modules, possibilité de lancer des tests de certains (sous-)modules seulement.
* Statistiques : contabilisation du nombre de tests, d'erreurs, etc.

`mini-meval`
============
C'est un méta-évaluateur «naïf» mais qui supporte pratiquement tout LISP sauf CLOS (Common Lisp Object System)

Syntaxe supportée par mini-meval et le simplificateur
=====================================================
* Les macros
* Les `eval-when`
* Les `tagbody` et `go`, les `throw` et `catch`, les `block` et `return` et `return-from` et le `unwind-protect`
* Les `let`, `let*`, `flet`, `labels`, `macrolet`, 
* Les `macro`
* Les `lambda` avec capture de variables
* Dans la `lambda-list`: `&optional`, `&rest`, `&key`, `&allow-other-keys`, `&aux`, mais pas les `&body`
* Plus `progn`, `if`, `#'`, `quote`, etc.

`lisp2li`
=========
On a choisi de ne pas utiliser le li proposé par les encadrants, mais au contraire d'utiliser du LISP (très) simplifié qui est donc exécutable ce qui nous a permis de faire des tests unitaires pour voir si la transformation préservait le sens du programme.

Cette transformation est assurée par la fonction squash-lisp décrite ci-dessous.

`squash-lisp`
=============
* En 3 passes :
  * Passe 1 :
    * macro-expansion (on utilise `mini-meval`) et `eval-when`
    * simplification globale de la syntaxe :
      * 
            (let (a (b 2) c) (list a b c))
            -> (let ((a nil) (b 2) (c nil)) (list a b c))
      * unification de la syntaxe des let, let*, flet, labels.
      * tous les appels de fonction sont transformés en funcall.
    * ré-écriture des `tagbody`/`go`, `throw`/`catch`, `block`/`return`/`return-from` en termes de `unwind`/`unwind-protect`,
      `jump`/`jump-label` plus quelques variations de ces derniers.
    * noms uniques pour les étiquettes des `jump`/`jump-label`.
    * toutes les constantes sont emballées dans `(quote)`.
    * toutes les lectures de variables sont emballées dans `(get-var)`.
  * Passe 2 :
    * noms uniques pour les variables locales. Il n'y a donc plus besoin de connaître l'imbrication des environnements pour savoir à quelle
      définition fait référence l'utilisation d'une variable.
    * toutes les déclarations de variables locales (`let`,`let*`,`flet`,`labels`,`lambda`) sont transformées en `simple-let`
    * le `simple-let` ne fait que déclarer, il n'afecte pas de valeurs :
          (let ((a nil) (b 2)) (list a b))
          -> (simple-let (a b) (setq a nil) (setq b 2) (list a b))
    * simplification de la lambda-list (élimination de `&optional`, `&rest`, `&key`, `&allow-other-keys`, `&aux`)
    * suppression des paramètres de la `lambda` :
          ```lisp
          (lambda (x y) (+ x y))
          -> (simple-lambda (simple-let (x y) (setq x (get-param 0)) (setq y (get-param 1)) (+ x y)))
```
  * Passe 3
    * On lorsqu'une variable à l'intérieur d'une `lambda` référence une déclaration à l'extérieur de la `lambda`, on la marque comme étant *capturée*.
    * On fusionne tous les `let` d'une `lambda` en les remontant dans un `let` unique à la racine de la `lamdba`.
    * On sort toutes les lambdas (fonctions anonymes), on les nomme avec un symbole unique, et on les met au top-level.

`pattern-match`
===============
* Nous avons cherché des bibliothèques de pattern-matching pour LISP, mais la seule qui était suffisemment puissante pour nos besoins
  utilisait une syntaxe très lourde qui prenait pratiquement autant de place que des tests du type `(if (eq (car expr) progn) ...)`.
* Nous avons fait une fonction de pattern matching, `pattern-match` qui utilise une syntaxe proche de celle des expressions régulières pour
  reconnaître des expressions LISP qui ont une certaine forme (suivant un motif). On peut «capturer» des portions de ce motif dans des
  variables qui sont accessibles dans le corps du match. Exemple :
      (match (lambda :params @ :body _*) expr
        (format t "La liste des paramètres est : ~a" params)
		(format t "Le premier élément du corps est : ~a" (car body)))
* On a aussi `cond-match` qui est un dérivé d'un `case` mais où chaque test est un motif qui doit correspondre avec l'expression donnée au
  début du `cond-match`.
* On a aussi `match-automaton` qui est une sorte d'automate où les transitions ne sont pas des lettres mais des motifs. On avance dans une
  liste en sélectionnant à chaque fois la première transition qui match avec l'élément en cours.  On peut «collecter» des données à chaque
  transition qui seront mises côte à côte dans une liste, renvoyée par le `match-automaton`.
* `defmatch` permet de définir une famille de fonctions, dont une seule est exécutée à l'appel (celle qui match avec le paramètre).
* Pas d'optimisation à la compilation permettant de détecter de manière statuque quelle fonction de la famille exécuter : la syntaxe de
  match est assez lourde, et vu qu'on ne match pas sur la forme de l'appel de fonction, mais sur les valeurs passées en paramètre,
  l'optimisation n'est pas faisable 90% du temps (on ne sait pas à la «compilation» quelle sera la valeur du paramètre).

compilation
===========
* On compile vers de l'assembleur assez proche de l'x86.
* Le code généré reste exécutable, tout comme le lisp simplifié. On utilise la même technique : emballer le code généré dans un `macrolet`.
* On avait déjà un système d'exploitation assez simple qui tournait de manière autonome sur un PC.
* On a donc stotché ensemble une REPL compilée et le SE de manière à avoir une REPL autonome qui ne dépend de rien.
* Le système d'exploitation sous-jacent fournit juste de quoi faire des entrées-sorties.

Ramasse-miettes
===============
* gestion du tas
* On a un gc très simpliste qui copie les données d'une zone de la mémoire vers une autre et vice versa à chaque fois qu'elle est pleine.
* Ce type de gc s'appelle un [two-finger garbage collector](http://en.wikipedia.org/wiki/Cheney's_algorithm "Article wikipédia").
* La raison de ce choix de modèle de gc est que les autres types de gc nécessitent une occupation variable de la mémoire pour l'exécution du
  gc (nécessaire pour le parcours en largeur/profondeur) qui peut être aussi grosse que la mémoire occupée (O(n)) dans le pire des cas.

Implémentation de fonctions LISP
================================
* On a notre propre fonction `read` et notre propre fonction `format` pour être autonomes.
