;; lisp2li simpliste pour le compilateur. On fusionnera les deux plus tard.

(defmatch lisp2cli)

(defmatch lisp2cli (:num . (? numberp)) `(:const . ,num))
(defmatch lisp2cli (:str . (? stringp)) `(:const . ,str))
(defmatch lisp2cli (quote :val _)       `(:const . ,val))
(defmatch lisp2cli ()                   `(:const . nil))
(defmatch lisp2cli (let ((:name $ :value _)*) :body _*)
  `(:let ,name ,value ,body))
(defmatch lisp2cli (:name _ :params _*) `(:call ,name ,@(mapcar #'lisp2cli params)))
(defmatch lisp2cli (:x . _)             (error "Lisp2cli ne sait pas gérer : ~w" x))


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
          (funcall ,closure)))
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
Si une fonction est redéfinie en tant que macro, tous les appels de fonction qui lui correspondent sont transformés en appels de macro (expansion à la volée). On peut alors redéfinir la  macro en macro ou en fonction, au choix, plusieurs fois, les appels suivent "intuitivement". (Ça existe encore ça l'intuition ?)
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
eval-when               OK Avant/pendant macro-expansion
flet                    OK
function                -- ?
generic-flet            ~~ Non implémenté
generic-labels          ~~ Non implémenté
go                      OK
if                      -- À compiler
labels                  OK
let                     OK
let*                    OK
macrolet                OK
multiple-value-call     ~~ Non implémenté
multiple-value-prog1    ~~ Non implémenté (mais le serait avec une macro)
progn                   OK (un seul géant qui représente le top-level)
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
asm
unwind
closure
+ les appels de lambdas nommés.

;; Implémentation des macros et de eval-when
Lors de la compilation d'un fichier, son top-level est traversé de la manière suivante :
On crée une instance de compiler-meval, un mini-meval qui renvoie toujours
un cons de la valeur de retour et de son état, pour qu'on puisse le rappeler.
compiler-meval transforme le eval-when en progn si sa situation contient :execute, en nil sinon.

- Si on rencontre EVAL-WHEN,
  - Au top-level,
    - Si la situation contient :compile-toplevel, le body est évalué dans compiler-meval.
    - Si la situation contient :load-toplevel, le eval-when est remplacé par son body.
  - Ailleurs
    - Si la situation contient :load-toplevel, le eval-when est remplacé par son body (TODO : À VÉRIVFIER !).
- Si on rencontre un defmacro
  - On demande à compiler-meval de l'exécuter. TODO : doit-on le faire uniquement au top-level ?.
- Si on rencontre un macrolet
  - On fait une copie de l'état de compiler-meval
  - On lui demande d'exécuter les définitions
  - On évalue le body avec ce nouvel état
  - On continue avec l'ancien état
- Si on gère le symbol-macrolet
  - Le fonctionnement est le même que pour le macrolet
  - Lorsqu'on rencontre un symbole, on regarde s'il a une définition de type symbol-macrolet
- Si on rencontre une macro définie dans l'environnement de compiler-meval,
  1) On demande à compiler-meval d'expanser la macro sur un niveau.
  2) On re-lance la transformation (eval-when / defmacro / appel de macro / ...) sur le résultat s'il y a a eu expansion.
- S'occuper du cas du lambda et des autres mot-clés bizarres (ne pas faire de macro-expansion dessus).
- Dans les autres cas, on transforme récursivement l'expression.

;; TODO : Comportement des variables globales et spéciales

;; TODO : Implémentation des variables globales et spéciales

|#