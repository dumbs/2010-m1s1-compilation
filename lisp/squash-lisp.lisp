(require 'mini-meval "mini-meval")
(require 'match "match")

(require 'squash-lisp-1 "squash-lisp-1")
(require 'squash-lisp-1 "squash-lisp-3")

;; Notes sur le fonctionnement (théorique) de squash-lisp


#|

;; Formes pouvant créer des variables capturables :
lambda
let
let* // let imbriqués
// progv // compliqué, pas très utile
flet // let pour les fonctions
labels // letrec pour les fonctions
macrolet // letrec pour les macros
// symbol-macrolet // compliqué, pas très utile

;; Formes pouvant capturer des variables :
lambda
defun => lambda

;; Comportement des variables globales et spéciales
- une variable qui n'est pas attachée lexicalement est globale
- une variable qui est déclarée speciale dans le cadre d'un let, defun, etc., est
  modifiée globallement par sa nouvelle valeur (celle du let / paramètre), puis
  sa valeur est restaurée à la fin du let / defun / ...
- une variable qui est globalement spéciale (comme c'est le cas pour les variables defvar)
  a une seule valeur partagée entre toutes ses utilisations. Autrement dit, partout
  où cette variable est lue ou modifiée, c'est la même valeur qui est utilisée.

(defvar x val)
=> (progn (proclaim '(special x))
          (unless (boundp 'x)
            (setq x val)))
(boundp var)
=> t si var _globale_ est bound (on s'en fiche de son état lexical).

;; Comportement des closures
Lorsqu'on fait une closure (à l'exécution donc), elle capture *toutes* les variables capturables de l'environnement.
Les variables capturées ont ensuite leur valeur partagée entre les différentes closures qui les utilisent et "l'extérieur" (le lieu de déclaration initial des variables).
Exemple :
(defun introspect () nil)
(defun make-closure (initial)
  (let ((a 1) (b 2) (c 3))
    (let ((closure-incf (lambda () (incf initial)))
          (closure-return (lambda () (introspect) initial)))
      (print initial)
      (funcall closure-incf)
      (print initial) ;; l'extérieur partage la même valeur
      (cons closure-incf closure-return))))
(setq cl1 (make-closure 1))
=> 1
=> 2
=> (#<closure...> . #<closure...>)
(setq cl42 (make-closure 42))
=> 42
=> 43
=> (#<closure...> . #<closure...>)
;; les valeurs sont partagées entre les closures créées sur les mêmes instances de variables
(funcall (car cl1))
=> 3
(funcall (cdr cl1))
=> 3
;; mais pas entre des closures créées à différents moments
(funcall (cdr cl42))
=> 43

Le comportement des fonctions et des macros expliqué ci-dessous permet de prouver que la capture s'effectue sur toutes les variables et non pas seulement celles qui paraissent être accessibles :
(defmacro introspect-closure (get-variable closure)
  `(progn (defmacro introspect () '(print ,get-variable))
          (funcall ,closure)
          (defun introspect () nil)))
(introspect-closure a (cdr cl1))
=> 1 ;; (print a)
=> 3
(introspect-closure b (cdr cl1))
=> 2 ;; (print b)
=> 3
(introspect-closure c (cdr cl1))
=> 3 ;; (print c)
=> 3
(introspect-closure initial (cdr cl1))
=> 3 ;; (print intitial)
=> 3

Un autre moyen de le vérifier est de mettre dans le let ((a 1) (b 2) (c 3)) un autre variable, non utilisée, qui est associée à une très grosse liste (un million d'éléments).
Après avoir créé une vingtaine de closures, on voit dans "top" que clisp occupe environ 90 Mo de RAM, alors qu'auparavent il n'en occupait que très peu.
Pourtant ces listes d'un million d'éléments semblent inaccessibles, sauf par notre trucage introspect-closure.

;; Comportement des fonctions et des macros
Si une macro est rencontrée, elle est expansée
Si un appel de fonction est rencontré, la fonction est appellée telle qu'elle
Si une fonction est redéfinie en tant que macro, tous les appels de fonction qui lui correspondent sont transformés en appels de macro (expansion à la volée).
On peut alors redéfinir la macro en macro ou en fonction, au choix, plusieurs fois, les appels suivent "intuitivement". (Ça existe encore ça l'intuition ?)
Si une macro "rencontrée initialement" est redéfinie en tant que fonction, les appels qui ont déjà été "expansés initialement" ne sont pas redéfinis.
Dans la structure suivante, la règle du "rencontrée initialement" est bien appliquée, la macro n'est pas ré-expansée :
(defmacro mcr (x) `(list ',x 'y))
(defun bar-maker () (defun bar () (mcr a)))
(bar-maker)
(bar)
=> (a y)
(defmacro mcr (x) `(list ',x 'z))
(bar)
=> (a y)
(bar-maker)
(bar)
=> (a y)

;; Décision

Pour des raisons de santé mentale, d'efficacité et d'alignement des planètes, nous ne supporterons pas la redéfinition de fonctions en tant que macros.
De même, si une macro est utilisée avant sa définition, elle ne sera pas expansée, et déclenchera probablement une erreur "undefined function".
Et pour simplifier la compilation, toutes les définitions de macros seront prises en compte,
qu'elles soient à l'intérieur d'un if, d'un defun, d'un let... sans prendre en compte leur environnement.

;; Fonctionnement des block et tagbody
Les noms de blocs sont un peu comme des variables, ils sont capturés par les closures.
Ainsi, dans l'exemple suivant, lorsque le (return-from a) est apellé, on sort directement
du premier bloc a dans la pile d'appels.

(defun foo (fn n)
  (format t "~&foo ~a ~a" n fn)
  (block a
    (bar (if fn fn (lambda (x)
                     (format t "~&lambda ~a" x)
                     (if (<= x 0) (return-from a 42))))
         n))
  (format t "~&foo2"))

(defun bar (fn n)
  (format t "~&bar ~a ~a" n fn)
  (funcall fn n)
  (foo fn (- n 1))
  (format t "~&bar2"))

;; Choix d'implémentation des block / return-from, tagbody / go, catch / throw

Lorsqu'on rentre dans un block, on met sur la pile un marqueur spécial avec un pointeur vers un objet créé à l'exécution.
Les return-from <nom> qui sont accessibles lexicalement sont remplacés par un (unwind <l'objet>)
Unwind remonte la pile jusqu'à trouver le marqueur spécial, tout en exécutant les unwind-protect éventuels.
Si unwind ne trouve pas le marqueur et arrive en haut de la pile, il signale une erreur et termine le programme.
Sinon, l'exécution reprend après le block.
Le traitement de tagbody/go est similaire pour sortir d'un tag, puis on jmp directement sur le tag de destination (vu qu'il est au même niveau).
Le traitement de catch/throw est similaire, sauf que le pointeur est simplement un pointeur vers l'objet utilisé pour le catch / throw.
À noter que la liaison lexicale pour le block et le tagbody est à effectuer avant de sortir éventuellement des lambdas anonymes de leur fonction englobante.
(comme la liaison lexicale ne s'effectue pas sur des variables, cette transformation ne présèrverait pas la liaison).
Cette étape doit s'effectuer après l'expansion de macros, sinon on risque de louper des block / return-from / ... .

;; Choix d'implémentation des closures :
Lorsqu'une variable est capturée, on ne peut pas copier directement l'adresse dans le tas de sa valeur pour pouvoir la manipuler,
car il se peut que l'on doive déplacer la valeur si on souhaite la remplacer par quelque chose de plus gros (par ex. remplacer un nombre par une string).
On est donc obligé d'avoir un pointeur d'indirection supplémentaire, de taille fixe, qui ne sera pas déplacé.
Ce pointeur est ce qu'on appellera la closure-cell.

Lorsqu'on rencontre un binding d'une variable (let, labels, lambda, ...), on regarde si elle est capturée à l'intérieur
du corps du special-form qui effectue la liaison.
Si c'est le cas, on crée une closure-cell, qui contient un pointeur vers l'endroit où est stockée la vraie valeur de la variable,
et à chaque lecture / écriture de la variable, on utilise (get-closure-cell-value <cl-cell>) à la place.
Ceci doit s'effectuer après l'expansion de macros, sinon on risque de louper des noms de variable capturées écrits par les macros.
Chaque lambda capturant des variables est ensuite modifié de manière à prendre les closure-cell des variables capturées en paramètre.
Il est "emballé" par une sorte de forme spéciale "closure", qui contient la liste des closure-cell à passer en paramètres à la lambda.
Il n'y a alors plus de closures dans le sens où toutes les variables capturées le sont explicitement, et sont passées en paramètre.
On peut donc "sortir" toutes les closures de leur environnement englobant, en les transformant en defuns nommés avec un symbole unique
généré avec make-symbol. La lambda elle-même est alors remplacée par (closure symbole-unique-du-defun closure-cell*).

TODO : revoir les choix d'implémentation des closures après une nuit de someil...
Le but est de ne plus avoir aucun lambda imbriqué dans quoi que ce soit
(tous les lambdas doivent être au top-level, juste emballés par un truc qui les nomme).

;; Implémentation des let, let*, flet, labels, macrolet, ...
Vu que tous les lambda ont été ramenés au top-level, il n'y a plus de capture de variables.
Les let sont donc :
 - À l'intérieur d'un lambda, quelque part
 - À l'intérieur d'un autre let qui est au top-level
 - À l'intérieur d'un progn qui est au top-level
 - Directement au top-level
Les trois derniers cas peuvent être ramenés au premier en les emballant avec un lambda sans paramètres
On peut alors "applatir" tous les let imbriqués dans un lambda dans la liste de paramètres du lambda.

Au top-level, on aura donc uniquement des lambda nommés, avec ou sans paramètres, qui ne contiendront ni lambda ni aucune forme de let.
Il n'y aura plus de macros.
Plus de block ni tagbody, donc pas de liaison lexicale à ce niveau.

Voici la liste des special-form.
block                   OK
catch                   OK
declare                 -- ?
eval-when              *OK Avant/pendant macro-expansion
flet                   *OK
function                -- ?
generic-flet            ~~ Non implémenté
generic-labels          ~~ Non implémenté
go                      OK
if                      -- À compiler
labels                 *OK
let                    *OK
let*                   *OK
macrolet               *OK
multiple-value-call     ~~ Non implémenté
multiple-value-prog1    ~~ Non implémenté (mais le serait avec une macro)
progn                  *OK (un seul géant qui représente le top-level)
progv                   ~~ Non implémenté
quote                   -- À compiler
return-from             OK
setq                    -- À compiler
symbol-macrolet         ~~ Non implémenté (peut-être ?)
tagbody                 OK
the                     ~~ Non implémenté, transformé ainsi : (the type form) => form
throw                   OK
unwind-protect          -- À compiler (ou bien macro-expansé en termes de "asm")
with-added-methors      ~~ Non implémenté

Les "formes spéciales du compilo" suivantes ont été rajoutées :
asm                     -- À compiler
unwind                  -- À compiler (ou bien macro-expansé en termes de "asm")
closure                 -- À compiler
+ les appels de lambdas nommés. -- À compiler

;; Implémentation des macros et de eval-when
Lors de la compilation d'un fichier, son top-level est traversé de la manière suivante :
On crée une instance de compiler-meval, un mini-meval qui renvoie toujours
un cons de la valeur de retour et de son état, pour qu'on puisse le rappeler.
compiler-meval transforme le eval-when en progn si sa situation contient :execute, en nil sinon.

NOTE : lorsqu'on rencontre la macro declaim au top-level, la proclamation est prise en compte.

| - Si on rencontre un defmacro (au toplevel ou ailleurs).
|   - On demande à compiler-meval de l'exécuter.
| - Si on rencontre EVAL-WHEN,
|   - Au top-level,
|     - Pour chaque form du body,
|       - Si la situation contient :compile-toplevel, le form est évalué dans compiler-meval.
|       - Si la situation contient :load-toplevel, le form est compilé (après évaluation dans compiler-meval s'il y a lieu).
|       - Lorsqu'un eval-when au top-level contient des eval-when directement sous lui, ils sont traités comme s'ils étaient directement au top-level.
|   - Ailleurs
|     - Si la situation contient :load-toplevel, le eval-when est remplacé par son body (TODO : À VÉRIVFIER !).
| - Si on rencontre un macrolet
|   - On fait une copie de l'état de compiler-meval
|   - On lui demande d'exécuter les définitions
|   - On évalue le body avec ce nouvel état
|   - On continue avec l'ancien état
| - Si on gère le symbol-macrolet
|   - Le fonctionnement est le même que pour le macrolet
|   - Lorsqu'on rencontre un symbole, on regarde s'il a une définition de type symbol-macrolet
| - Si on rencontre une macro définie dans l'environnement de compiler-meval,
|   1) On demande à compiler-meval d'expanser la macro sur un niveau.
|   2) On re-lance la transformation (eval-when / defmacro / appel de macro / ...) sur le résultat s'il y a a eu expansion.
| - S'occuper du cas du lambda et des autres mot-clés bizarres (ne pas faire de macro-expansion dessus).
| - Dans les autres cas, on transforme récursivement l'expression.

;; Comportement des variables globales et spéciales
Lorsqu'une variable est utilisée mais ne correspond à aucune liaison (établie par let, …), cette utilisation fait référence
à une liaison "globale" de cette variable (autrement dit, la valeur est partagée entre toutes les utilisations sans liaison).
Par défaut, une variable globale est "unbound", et n'a donc pas de valeur. La lecture d'une variable unbound est une erreur.
x
=> erreur
(defun bar () x)
(bar)
=> erreur
(setq x 3)
(bar)
=> 3

Lorsqu'une variable est déclarée localement spéciale, avec (declare (special nom-de-variable)) au début d'un defun, d'un let etc.,
la valeur de la variable est alors la même que la valeur globale.
Ce comportement spécial s'applique là où la variable est dans la portée lexicale de la forme spéciale englobant le declare (donc uniquement dans le defun, let, …).

(defun baz () y)
(let ((y 3)) (let ((z 1)) (declare (special y)) (setq y 42) (baz)))
=> 42 ;; Bien que y soit une liaison lexicale à cause du (let ((y 3)), le (special y) le transforme en variable globale.
y
=> 42

Si la forme spéciale englobante devait créer une liaison lecicale pour cette variable, elle ne le fait pas, mais à la place,
 - la valeur globale d'origine de la variable est sauvegardée,
 - sa valeur globale est modifiée avec la nouvelle valeur (paramètre effectif pour un defun, valeur de droite pour un let, ...)
 - La variable devient boundp si elle ne l'était pas déjà
 - le corps est exécuté, avec la variable partageant sa valeur avec la varaible globale
 - la valeur d'origine de la variable globale est restaurée.
 - Si la valeur était unbound, elle redevient unbound.

(defun quux () (print (boundp 'w)) w)
(boundp 'w)
=> NIL
(quux)
=> erreur
(let ((w 3)) (declare (special w)) (print (boundp 'w)) (quux))
=> T ;; boundp
=> T ;; boundp
=> 3
(boundp 'w)
(quux)
=> erreur ;; La valeur est bien restaurée.

Lorsqu'on effectue un (defvar var val), var devient globalement spéciale : toutes les utilisations de var pointent vers la même valeur,
y compris les utilisations effectuées avant le defvar.
(defun foo1 () var)
(defun foo2 () (let ((var 4)) (print var) (foo1)))
var
=> erreur
(foo1)
=> erreur
(foo2)
=> 4
=> erreur
(defvar var 123)
var
=> 123
(foo1)
=> 123
(foo2)
=> 4
=> 4

Lors du defvar, si la variable est boundp, sa valeur était conservée, sinon sa valeur globale devient la valeur spécifiée par le defvar.
Notemment, si le defvar apparaît à l'intérieur d'un let-special qui rend la variable boundp locallement, sa valeur globale sera restaurée à unbound à la sortie du let.
(defun get-1 () not-boundp)
(defun get-2 () is-boundp)
(defun get-3 () locally-boundp)
(defun getlet-1 () (let ((not-boundp 123))     (get-1)))
(defun getlet-2 () (let ((is-boundp 123))      (get-2)))
(defun getlet-3 () (let ((locally-boundp 123)) (get-3)))
(setq is-boundp 42)
(get-1)    => error ;; not-boundp
(get-2)    => 42    ;; is-boundp
(get-3)    => error ;; locally-boundp
(getlet-1) => error ;; not-boundp
(getlet-2) => 42    ;; is-boundp
(getlet-3) => error ;; locally-boundp

(defvar not-boundp 3)
(defvar is-boundp  3)
(let ((locally-boundp 42))
  (declare (special locally-boundp))
  (defvar locally-boundp 3))
(get-1)    => 3     ;; not-boundp
(get-2)    => 42    ;; is-boundp
(get-3)    => error ;; locally-boundp
;; La variable est maintenant spéciale partout :
(getlet-1) => 123   ;; not-boundp
(getlet-2) => 123   ;; is-boundp
(getlet-3) => 123   ;; locally-boundp

;; Implémentation des variables globales et spéciales

Pour les mêmes raisons de santé d'esprit et d'efficacité et d'alignement des planètes que pour la redéclaration de fonctions en macros, nous ne supporterons pas la redéclaration
de variables non spéciales en spéciales. Ainsi, si un defvar apparaît *après* des utilisations non spéciales de la variable, ces utilisations resteront non spéciales.

Lorsqu'une variable est détectée comme étant spéciale (soit globalement, avec defvar, soit localement, avec declare), sa valeur est stockée dans une global-cell,
qui ressemble comme deux goutes d'eau à une closure-cell, et toutes ces utilisations passent par la globla-cell, comme pour les variables capturées.
On est obligé d'avoir ce niveau d'indirection pour les mêmes raisons que pour le closure-cell.
La différence avec une closure-cell est qu'il n'y a qu'une seule global-cell par variable, qui est créée à la compilation.
De même, l'utilisation globale d'une variable de manière globale est remplacée par une référence à sa global-cell.
De plus, les formes spéciales qui devaient créer une liaison locale sont transformées comme suit :
(let ((x 3)) (declare (special x)) (setq x 42) x)
Est transformé en :
== En-tête
[global-cell x]
#<unbound>
== Code
[push [get-global-cell-value x]]
[set-global-cell-value x 3]
[set-global-cell-value x 42]
[get-global-cell-value x]
[set-global-cell-value x [pop]]

|#

(provide 'squash-lisp)