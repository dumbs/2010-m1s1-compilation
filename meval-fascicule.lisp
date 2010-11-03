#|
Exo 4.1 Vérifier ce que fait la fonction "error" dans le poly ou le manuel en ligne.

24.1. General Error-Signaling Functions (beginning only)
The functions in this section provide various mechanisms for signaling warnings, breaks, 
continuable errors, and fatal errors.

In each case, the caller specifies an error message (a string) that may be processed (and 
perhaps displayed to the user) by the error-handling mechanism. All messages are constructed 
by applying the function format to the quantities nil, format-string, and all the args to produce 
a string.

An error message string should not contain a newline character at either the beginning or end, 
and should not contain any sort of herald indicating that it is an error. The system will take care
of these according to whatever its preferred style may be.

Conventionally, error messages are complete English sentences ending with a period. 
Newlines in the middle of long messages are acceptable. There should be no indentation after
a newline in the middle of an error message. The error message need not mention the name of the 
function that signals the error; it is assumed that the debugger will make this information available.

Implementation note: If the debugger in a particular implementation displays error messages 
indented from the prevailing left margin (for example, indented by seven spaces because they 
are prefixed by the seven-character herald ``Error: ''), then the debugger should take care of 
inserting the appropriate indentation into a multi-line error message. Similarly, a debugger that 
prefixes error messages with semicolons so that they appear to be comments should take care of 
inserting a semicolon at the beginning of each line in a multi-line error message. These rules 
are suggested because, even within a single implementation, there may be more than one program 
that presents error messages to the user, and they may use different styles of presentation. 
The caller of error cannot anticipate all such possible styles, and so it is incumbent upon the 
presenter of the message to make any necessary adjustments.

Common Lisp does not specify the manner in which error messages and other messages are displayed. 
For the purposes of exposition, a fairly simple style of textual presentation will be used in the 
examples in this chapter. The character > is used to represent the command prompt symbol for a debugger.


[Function]
error format-string &rest args

This function signals a fatal error. It is impossible to continue from this kind of error; 
thus error will never return to its caller.

The debugger printout in the following example is typical of what an implementation might print when 
error is called. Suppose that the (misspelled) symbol emergnecy-shutdown has no property named command 
(all too likely, as it is probably a typographical error for emergency-shutdown).

(defun command-dispatch (cmd) 
  (let ((fn (get cmd 'command))) 
    (if (not (null fn)) 
        (funcall fn)) 
        (error "The command ~S is unrecognized." cmd)))) 

(command-dispatch 'emergnecy-shutdown) 
Error: The command EMERGNECY-SHUTDOWN is unrecognized. 
Error signaled by function COMMAND-DISPATCH. 
>


Compatibility note: Lisp Machine Lisp calls this function ferror. 
MacLisp has a function named error that takes different arguments and can signal either a fatal 
or a continuable error. 
|#

#|
Exo 4.2 Vérifier ce que fait la fontion "assoc" dans le poly ou le manuel en ligne.

15.6. Association Lists

An association list, or a-list, is a data structure used very frequently in Lisp. 
An a-list is a list of pairs (conses); each pair is an association. The car of a pair is called 
the key, and the cdr is called the datum.

An advantage of the a-list representation is that an a-list can be incrementally augmented 
simply by adding new entries to the front. Moreover, because the searching function assoc 
searches the a-list in order, new entries can ``shadow'' old entries. If an a-list is viewed 
as a mapping from keys to data, then the mapping can be not only augmented but also altered in a 
non-destructive manner by adding new entries to the front of the a-list.

Sometimes an a-list represents a bijective mapping, and it is desirable to retrieve a key given a datum. 
For this purpose, the ``reverse'' searching function rassoc is provided. Other variants of a-list 
searches can be constructed using the function find or member.

It is permissible to let nil be an element of an a-list in place of a pair. Such an element is not 
considered to be a pair but is simply passed over when the a-list is searched by assoc.

[Function]
acons key datum a-list

acons constructs a new association list by adding the pair (key . datum) to the old a-list.

(acons x y a) == (cons (cons x y) a)

This is a trivial convenience function, but I find I use it a lot.



[Function]
pairlis keys data &optional a-list

pairlis takes two lists and makes an association list that associates elements of the first list 
to corresponding elements of the second list. It is an error if the two lists keys and data are not 
of the same length. If the optional argument a-list is provided, then the new pairs are added to the 
front of it.

The new pairs may appear in the resulting a-list in any order; in particular, either forward or 
backward order is permitted. Therefore the result of the call

(pairlis '(one two) '(1 2) '((three . 3) (four . 19)))

might be

((one . 1) (two . 2) (three . 3) (four . 19))

but could equally well be

((two . 2) (one . 1) (three . 3) (four . 19))


[Function]
assoc item a-list &key :test :test-not :key
assoc-if predicate a-list
assoc-if-not predicate a-list

[Function]
assoc-if predicate a-list &key :key
assoc-if-not predicate a-list &key :key

The omission of :key arguments for these functions in the first edition was probably an oversight.

Each of these searches the association list a-list. The value is the first pair in the a-list such 
that the car of the pair satisfies the test, or nil if there is no such pair in the a-list. For example:

(assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z))) 
        =>  (r . x) 
(assoc 'goo '((foo . bar) (zoo . goo))) => nil 
(assoc '2 '((1 a b c) (2 b c d) (-7 x y z))) => (2 b c d)

It is possible to rplacd the result of assoc provided that it is not nil, in order to ``update'' the 
``table'' that was assoc's second argument. (However, it is often better to update an a-list by 
adding new pairs to the front, rather than altering old pairs.) For example:

(setq values '((x . 100) (y . 200) (z . 50))) 
(assoc 'y values) => (y . 200) 
(rplacd (assoc 'y values) 201) 
(assoc 'y values) => (y . 201) now

A typical trick is to say (cdr (assoc x y)). Because the cdr of nil is guaranteed to be nil, 
this yields nil if no pair is found or if a pair is found whose cdr is nil. This is useful 
if nil serves its usual role as a ``default value.''

The two expressions

(assoc item list :test fn)

and

(find item list :test fn :key #'car)

are equivalent in meaning with one important exception: if nil appears in the a-list in place of 
a pair, and the item being searched for is nil, find will blithely compute the car of the nil 
in the a-list, find that it is equal to the item, and return nil, whereas assoc will ignore the 
nil in the a-list and continue to search for an actual pair (cons) whose car is nil. 
See find and position.

Compatibility note: In MacLisp, the assoc function uses an equal comparison rather than eql, 
which is the default test for assoc in Common Lisp. Where in MacLisp one would write (assoc x y), 
in Common Lisp one must write (assoc x y :test #'equal) to get the completely identical effect. 
Similarly, one can get the precise effect, and no more, of the MacLisp (assq x y) by writing in 
Common Lisp (assoc x y :test #'eq).

In Interlisp, assoc uses an eq test, and sassoc uses an Interlisp equal test.


[Function]
rassoc item a-list &key :test :test-not :key
rassoc-if predicate a-list
rassoc-if-not predicate a-list

[Function]
rassoc-if predicate a-list &key :key
rassoc-if-not predicate a-list &key :key

The omission of :key arguments for these functions in the first edition was probably an oversight.
change_end

rassoc is the reverse form of assoc; it searches for a pair whose cdr satisfies the test, rather than 
the car. If the a-list is considered to be a mapping, then rassoc treats the a-list as representing 
the inverse mapping. For example:

(rassoc 'a '((a . b) (b . c) (c . a) (z . a))) => (c . a)

The expressions

(rassoc item list :test fn)

and

(find item list :test fn :key #'cdr)

are equivalent in meaning, except when the item is nil and nil appears in place of a pair in the a-list. 
See the discussion of the function assoc.

|#

#|
Exo 4.3 Définir la fonction "meval-body" qui prend en paramètre une liste d'expressions évaluables
et un environnement, qui les évalue en séquence et retourne la valeur retourée par la dernière.
|#
#|
(defun meval-body (liste-expr env)
;; on boul
 )
 |#

#|
Exo 4.4 Définir la fonction "meval-args" qui prend en paramètre une liste d'expressions évaluables
et un environnement, qui les évalue en séquence et retourne la liste de leurs valeurs.
|#
#|
(defun meval-args (liste-expr)
  )
|#


#|
Exo 4.5 Définir la fonction "make-env" qui prend en paramètre une liste de symboles, une liste de
valeurs et un environnement : construit l'environnement (une liste d'association) en appariant
les paramètres aux valeurs correspondantes et signale une exception si paramètres et arguments 
ne concordent pas. On ne traitera d'abord que le cas des paramètres obligatoires. Si l'environnement
passé en paramètre n'est pas vide, le nouvel environnement doit l'inclure.
|#
#|
(defun make-env (liste-symb liste-val env)
 )
 |#
 
#|
Exo 4.6 Définir la fonction "meval-lambda" qui applique une lambda-fonction quelconque à des valeurs
d'arguments dans un certain environnement. Cette fonction servira aussi pour les autres cas 
d'application de fonction, par exemple pour les macros.
Exemple d'utilisation : 
((and (consp (car expr)) (eq 'lambda (caar expr))) ; lambda-fonction
(meval-lambda (car expr) (meval-args (cdr expr) env) env))
  ;; une fonction est un symbole non constant
((get-defun (car expr))
(meval-lambda (get-defun (car expr)) (meval-args (cdr expr) env) ()))
...)
|#
#|
(defun meval-lambda (expr)
  )
|#

#|
Exo 4.7 Vérifier ce que fait la fonction "get" dans le poly ou le manuel en ligne.

10.1. The Property List

Since its inception, Lisp has associated with each symbol a kind of tabular data structure called 
a property list (plist for short). A property list contains zero or more entries; each entry 
associates with a key (called the indicator), which is typically a symbol, an arbitrary Lisp object 
(called the value or, sometimes, the property). There are no duplications among the indicators; a 
property list may only have one property at a time with a given name. In this way, given a symbol 
and an indicator (another symbol), an associated value can be retrieved.

A property list is very similar in purpose to an association list. The difference is that a property 
list is an object with a unique identity; the operations for adding and removing property-list entries 
are destructive operations that alter the property list rather than making a new one. Association lists, 
on the other hand, are normally augmented non-destructively (without side effects) by adding new entries 
to the front (see acons and pairlis).

A property list is implemented as a memory cell containing a list with an even number (possibly zero) 
of elements. (Usually this memory cell is the property-list cell of a symbol, but any memory cell 
acceptable to setf can be used if getf and remf are used.) Each pair of elements in the list 
constitutes an entry; the first item is the indicator, and the second is the value. Because property-list 
functions are given the symbol and not the list itself, modifications to the property list can be 
recorded by storing back into the property-list cell of the symbol.

When a symbol is created, its property list is initially empty. Properties are created by using get 
within a setf form.

Common Lisp does not use a symbol's property list as extensively as earlier Lisp implementations did. 
Less-used data, such as compiler, debugging, and documentation information, is kept on property lists 
in Common Lisp.

In Common Lisp, the notion of ``disembodied property list'' introduced in MacLisp is eliminated. 
It tended to be used for rather kludgy things, and in Lisp Machine Lisp is often associated with 
the use of locatives (to make it ``off by one'' for searching alternating keyword lists). 
In Common Lisp special setf-like property-list functions are introduced: getf and remf.


[Function]
get symbol indicator &optional default

get searches the property list of symbol for an indicator eq to indicator. The first argument 
must be a symbol. If one is found, then the corresponding value is returned; otherwise default 
is returned.

If default is not specified, then nil is used for default.

Note that there is no way to distinguish an absent property from one whose value is default.

(get x y) == (getf (symbol-plist x) y)

Suppose that the property list of foo is (bar t baz 3 hunoz "Huh?"). Then, for example:

(get 'foo 'baz) => 3 
(get 'foo 'hunoz) => "Huh?" 
(get 'foo 'zoo) => nil

Compatibility note: In MacLisp, the first argument to get could be a list, in which case the cdr 
of the list was treated as a so-called ``disembodied property list.'' The first argument to get 
could also be any other object, in which case get would always return nil. In Common Lisp, it is 
an error to give anything but a symbol as the first argument to get.

setf may be used with get to create a new property-value pair, possibly replacing an old pair 
with the same property name. For example:

(get 'clyde 'species) => nil 
(setf (get 'clyde 'species) 'elephant) => elephant 
and now (get 'clyde 'species) => elephant

The default argument may be specified to get in this context; it is ignored by setf but may be 
useful in such macros as push that are related to setf:

(push item (get sym 'token-stack '(initial-item)))

means approximately the same as

(setf (get sym 'token-stack '(initial-item)) 
      (cons item (get sym 'token-stack '(initial-item))))

which in turn would be treated as simply

(setf (get sym 'token-stack) 
      (cons item (get sym 'token-stack '(initial-item))))

[Function]
remprop symbol indicator

This removes from symbol the property with an indicator eq to indicator. The property indicator 
and the corresponding value are removed by destructively splicing the property list. 
It returns nil if no such property was found, or non-nil if a property was found.

(remprop x y) == (remf (symbol-plist x) y)

For example, if the property list of foo is initially

(color blue height 6.3 near-to bar)

then the call

(remprop 'foo 'height)

returns a non-nil value after altering foo's property list to be

(color blue near-to bar)

[Function]
symbol-plist symbol

This returns the list that contains the property pairs of symbol; the contents of the property-list 
cell are extracted and returned.

Note that using get on the result of symbol-plist does not work. One must give the symbol itself 
to get or else use the function getf.

setf may be used with symbol-plist to destructively replace the entire property list of a symbol. 
This is a relatively dangerous operation, as it may destroy important information that the 
implementation may happen to store in property lists. Also, care must be taken that the new property 
list is in fact a list of even length.

Compatibility note: In MacLisp, this function is called plist; in Interlisp, it is called getproplist.

[Function]
getf place indicator &optional default

getf searches the property list stored in place for an indicator eq to indicator. 
If one is found, then the corresponding value is returned; otherwise default is returned. 
If default is not specified, then nil is used for default. Note that there is no way to 
distinguish an absent property from one whose value is default. Often place is computed 
from a generalized variable acceptable to setf.

setf may be used with getf, in which case the place must indeed be acceptable as a place to setf. 
The effect is to add a new property-value pair, or update an existing pair, in the property 
list kept in the place. The default argument may be specified to getf in this context; 
it is ignored by setf but may be useful in such macros as push that are related to setf. 
See the description of get for an example of this.

Compatibility note: The Interlisp function listget is similar to getf. The Interlisp function 
listput is similar to using getf with setf.

[Macro]
remf place indicator

This removes from the property list stored in place the property with an indicator eq to indicator. 
The property indicator and the corresponding value are removed by destructively splicing the property list. 
remf returns nil if no such property was found, or some non-nil value if a property was found. 
The form place may be any generalized variable acceptable to setf. See remprop.

[Function]
get-properties place indicator-list

get-properties is like getf, except that the second argument is a list of indicators. 
get-properties searches the property list stored in place for any of the indicators in 
indicator-list until it finds the first property in the property list whose indicator 
is one of the elements of indicator-list. Normally place is computed from a generalized 
variable acceptable to setf.

get-properties returns three values. If any property was found, then the first two values are 
the indicator and value for the first property whose indicator was in indicator-list, and 
the third is that tail of the property list whose car was the indicator (and whose cadr 
is therefore the value). If no property was found, all three values are nil. Thus the 
third value serves as a flag indicating success or failure and also allows the search to be 
restarted, if desired, after the property was found.
|#

#|
"Exo 4.8a" Il reste enfin à définir "get-defun". On pourrait construire un environnement spécial - 
il s'agit bien d'ailleurs d'un environnement spécial, réservé aux fonctions et global - en réutilisant 
des listes d'association, mais cela poserait divers problèmes techniques et le plus simple est 
d'utiliser les propriétés des symboles et la fonction "get". On définira alors "get-defun" ainsi : ..
|#
(defun get-defun (symb)
  (get symb :defun))
#|
... où "symb" est le symbole, c-à-d le nom de fonction concerné et ":defun" est un 'keyword',
c-à-d un symbole constant arbitraire. Cependant, si "get" est bien "setf-able", ce n'est plus 
le cas de "get-defun".
|#
#|
Exo 4.8 Ecrire "get-defun" sous forme de maro et vérifier que cette nouvelle version est bien 
setf-able.
|#
#|
(defun get-defun (symb)
 )
|#

#|
Exo 4.9 Vérifier ce que fait la fonction "symbol-function" dans le poly ou le manuel en ligne.

[Function]
symbol-function symbol

symbol-function returns the current global function definition named by symbol. 
An error is signalled if the symbol has no function definition; see fboundp. 
Note that the definition may be a function or may be an object representing a special 
form or macro. In the latter case, however, it is an error to attempt to invoke the object 
as a function. If it is desired to process macros, special forms, and functions equally 
well, as when writing an interpreter, it is best first to test the symbol with macro-function 
and special-form-p and then to invoke the functional value only if these two tests both yield false.

This function is particularly useful for implementing interpreters for languages embedded in Lisp.

symbol-function cannot access the value of a lexical function name produced by flet or labels; 
it can access only the global function value.

The global function definition of a symbol may be altered by using setf with symbol-function. 
Performing this operation causes the symbol to have only the specified definition as its 
global function definition; any previous definition, whether as a macro or as a function, 
is lost. It is an error to attempt to redefine the name of a special form (see table 5-1).

change_begin
X3J13 voted in June 1988 (FUNCTION-TYPE)   to clarify the behavior of symbol-function in the 
light of the redefinition of the type function.

    * It is permissible to call symbol-function on any symbol for which fboundp returns true. 
    Note that fboundp must return true for a symbol naming a macro or a special form.

    * If fboundp returns true for a symbol but the symbol denotes a macro or special form, 
    then the value returned by symbol-function is not well-defined but symbol-function will 
    not signal an error.

    * When symbol-function is used with setf the new value must be of type function. 
    It is an error to set the symbol-function of a symbol to a symbol, a list, or the 
    value returned by symbol-function on the name of a macro or a special form. 
|#

#|
Exo 4.10 Vérifier ce que fait la fonction "special-form-p" dans le poly ou le manuel en ligne.

[Function]
special-form-p symbol

The function special-form-p takes a symbol. If the symbol globally names a special form, then 
a non-nil value is returned; otherwise nil is returned. A returned non-nil value is typically 
a function of implementation-dependent nature that can be used to interpret (evaluate) the special form.

It is possible for both special-form-p and macro-function to be true of a symbol. This is 
possible because an implementation is permitted to implement any macro also as a special form 
for speed. On the other hand, the macro definition must be available for use by programs 
that understand only the standard special forms listed in table 5-1.
|#

#|
Exo 4.11 Vérifier ce que fait la fonction "fboundp" dans le poly ou le manuel en ligne.

[Function]
fboundp symbol

fboundp is true if the symbol has a global function definition. Note that fboundp is true 
when the symbol names a special form or macro. macro-function and special-form-p may be used 
to test for these cases.

change_begin
X3J13 voted in June 1988 (FUNCTION-TYPE)   to emphasize that, despite the tightening of the 
definition of the type function, fboundp must return true when the argument names a special 
form or macro.

See also symbol-function and fmakunbound.

X3J13 voted in March 1989 (FUNCTION-NAME)   to extend fboundp to accept any function-name (a 
symbol or a list whose car is setf-see section 7.1). Thus one may write (fboundp '(setf cadr)) 
to determine whether a setf expansion function has been globally defined for cadr.
change_end 
|#

#|
Exo 4.12 Tester "symbol-function", "special-form-p" et "fboundp" sur des arguments de différents types : 
autre que symbol, sur des symboles avec ou sans définition fonctionnelle, et enfin avec des définitions
fonctionnelles de différents types (formes syntaxiques, macros, fonctions globales ou locales).
|#

#|
Exo 4.13 Méta-définir les fonctions "fact" et "fibo". Les tester.
|#
#|
(defun fact-meta ()
  )
|#
#|
(defun fibo-meta ()
  )
|#

#|
Exo 4.14 Considérer l'expression (meval '(meval '(fibo 10))) et en déduire quelle va
être la première erreur produite par son évaluation. Vérifier par un test. Si ça marche du 
premier coup, c'est mauvais signe : vérifier que meval a bien été méta-définie (par 
(meval '(defun meval ...))) ! 
|#

#|
Exo 4.15 Etendre la définition de "make-env" aux mots-clés &optional et &rest.
On se basera sur le fait que la spécification des ces mots-clés repose sur un automate
implicite. Expliciter l'automate et l'implémentation par des fonctions adéquates
(voir aussi Chapitre 3, en particulier la section 3.3)
|#

(defun meval (expr &optional env)
  (cond
    ((and (atom expr) (constantp expr)) ; constante atomique
    (print "constante atomique")
      expr)
    ((atom expr) ; atom non constant, donc variable
      (print "atom non constant, donc variable")
      (let ((cell (assoc expr env)))
        (if cell
          (cdr cell)
          (error "~s n'est pas une variable" expr))))
    ;; plus d'atome à partir d'ici
    ((and (consp (car expr)) (eq 'lambda (caar expr))) ;; lambda-fonction
      (print "lambda-fonction")
      ;; ((meval-lambda (car expr) (meval-args (cdr expr) env) env))	
      (meval-body (cddar expr)
        (make-env (cadar expr)
          (meval-args (cdr expr) env)
          env)))
    ((or (not (symbolp (car expr))) (constantp (car expr))) ;; ?? en cas de "nil"
      ;; une fonction est un symbole non constant
      (error "~s ne peut être une fonction" (car expr)))
;    ((get-defun (car expr))
;      (let ((fun (get-defun (car expr))))
;        ;; (meval-lambda (get-defun (car expr)) (meval-args (cdr expr) env) ()))
;        (meval-body (cddr fun)
;          (make-env (cadr fun)
;            (meval-args (cdr expr) env)
;            ()))))
;      ((eq 'defun (car expr))
;        (setf (get-defun (cadr expr))
;          '(lambd ,@(cddr expr))))
    ((eq 'quote (car expr)) ;; quote
      (print "quote")
      (cadr expr))
    ((not (fboundp (car expr))) ;; faux gd // à étudier
      (error "~s symbole sans définition fonctionnelle" (car expr)))
    ((special-form-p (car expr))
      (print "forme spéciale non implémentée")
      (if (null env)
        (eval expr)
        (error "~s forme spéciale NYI" (car expr))))
;    (t (apply (symbol-function (car expr)) (meval-args (cdr expr) env)))
    ; TODO : la fin est fausse
    ((null env)
      (eval expr))
    (t (error "impossible d'évaluer ~s dans l'environnement ~s" expr env))
    ;(t (eval expr)) ; triche
    
  ))

#|
Exo 4.16 Définir cette fonction mload : (voir fascicule, page 23) on regardera dans le manuel le 
chapitre sur les entrées-sorties, en particulier les fonctions open, read et close, ainsi que le traitement
de la fin de fichier
|#

#|
23.2. Opening and Closing Files
When a file is opened, a stream object is constructed to serve as the file system's ambassador 
to the Lisp environment; operations on the stream are reflected by operations on the file in the
file system. The act of closing the file (actually, the stream) ends the association; the 
transaction with the file system is terminated, and input/output may no longer be performed 
on the stream. The stream function close may be used to close a file; the functions described 
below may be used to open them. The basic operation is open, but with-open-file is usually 
more convenient for most applications.

[Function]
open filename &key :direction :element-type :if-exists :if-does-not-exist :external-format

This returns a stream that is connected to the file specified by filename. The filename is the 
name of the file to be opened; it may be a string, a pathname, or a stream. (If the filename 
is a stream, then it is not closed first or otherwise affected; it is used merely to provide 
a file name for the opening of a new stream.)

The keyword arguments specify what kind of stream to produce and how to handle errors:

:direction
    This argument specifies whether the stream should handle input, output, or both.
        :input
            The result will be an input stream. This is the default.
        :output
            The result will be an output stream.
        :io
            The result will be a bidirectional stream.
        :probe
            The result will be a no-directional stream (in effect, the stream is created and then closed). This is useful for determining whether a file exists without actually setting up a complete stream.
:element-type
    This argument specifies the type of the unit of transaction for the stream. Anything that can 
    be recognized as being a finite subtype of character or integer is acceptable. In particular, 
    the following types are recognized:
        string-char
            The unit of transaction is a string-character. The functions read-char and/or write-char 
            may be used on the stream. This is the default.
        character
            The unit of transaction is any character, not just a string-character. The functions read-char and/or write-char may be used on the stream.

  to eliminate the type string-char, add the type base-character, and redefine open to use the type character as the default :element-type.

    The preceding two possibilities should therefore be replaced by the following.
        character
            The unit of transaction is any character, not just a string-character. The functions 
            read-char and write-char (depending on the value of the :direction argument) may be 
            used on the stream. This is the default.
        base-character
            The unit of transaction is a base character. The functions read-char and write-char 
            (depending on the value of the :direction argument) may be used on the stream.
        (unsigned-byte n)
            The unit of transaction is an unsigned byte (a non-negative integer) of size n. 
            The functions read-byte and/or write-byte may be used on the stream.
        unsigned-byte
            The unit of transaction is an unsigned byte (a non-negative integer); the size of the byte 
            is determined by the file system. The functions read-byte and/or write-byte may be used 
            on the stream.
        (signed-byte n)
            The unit of transaction is a signed byte of size n. 
            The functions read-byte and/or write-byte may be used on the stream.
        signed-byte
            The unit of transaction is a signed byte; the size of the byte is determined by the 
            file system. The functions read-byte and/or write-byte may be used on the stream.
        bit
            The unit of transaction is a bit (values 0 and 1). The functions read-byte and/or 
            write-byte may be used on the stream.
        (mod n)
            The unit of transaction is a non-negative integer less than n. The functions read-byte 
            and/or write-byte may be used on the stream.
        :default
            The unit of transaction is to be determined by the file system, based on the file it finds. The type can be determined by using the function stream-element-type.
:if-exists
    This argument specifies the action to be taken if the :direction is :output or :io and a file of 
    the specified name already exists. If the direction is :input or :probe, this argument is ignored.
        :error
            Signals an error. This is the default when the version component of the 
            filename is not :newest.
        :new-version
            Creates a new file with the same file name but with a larger version number. This is the 
            default when the version component of the filename is :newest.
        :rename
            Renames the existing file to some other name and then creates a new file with the 
            specified name.
        :rename-and-delete
            Renames the existing file to some other name and then deletes it (but does not expunge it, 
            on those systems that distinguish deletion from expunging). Then create a new file with the specified name.
        :overwrite
            Uses the existing file. Output operations on the stream will destructively modify the 
            file. If the :direction is :io, the file is opened in a bidirectional mode that allows 
            both reading and writing. The file pointer is initially positioned at the beginning of 
            the file; however, the file is not truncated back to length zero when it is opened. 
            This mode is most useful when the file-position function can be used on the stream.
        :append
            Uses the existing file. Output operations on the stream will destructively modify the 
            file. The file pointer is initially positioned at the end of the file. If the :direction 
            is :io, the file is opened in a bidirectional mode that allows both reading and writing.
        :supersede
            Supersedes the existing file. If possible, the implementation should arrange not to 
            destroy the old file until the new stream is closed, against the possibility that the 
            stream will be closed in ``abort'' mode (see close). This differs from :new-version in 
            that :supersede creates a new file with the same name as the old one, rather than a 
            file name with a higher version number.
        nil
            Does not create a file or even a stream, but instead simply returns nil to indicate failure.
    If the :direction is :output or :io and the value of :if-exists is :new-version, then the version 
    of the (newly created) file that is opened will be a version greater than that of any other file 
    in the file system whose other pathname components are the same as those of filename.
    If the :direction is :input or :probe or the value of :if-exists is not :new-version, and 
    the version component of the filename is :newest, then the file opened is that file already 
    existing in the file system that has a version greater than that of any other file in the file 
    system whose other pathname components are the same as those of filename.
:if-does-not-exist
    This argument specifies the action to be taken if a file of the specified name does not already exist.
        :error
            Signals an error. This is the default if the :direction is :input, or if the :if-exists 
            argument is :overwrite or :append.
        :create
            Creates an empty file with the specified name and then proceeds as if it had 
            already existed (but do not perform any processing directed by the :if-exists argument). 
            This is the default if the :direction is :output or :io, and the :if-exists argument is 
            anything but :overwrite or :append.
        nil
            Does not create a file or even a stream, but instead simply returns nil to indicate failure. 
            This is the default if the :direction is :probe.
:external-format
    This argument specifies an implementation-recognized scheme for representing characters in files. 
    The default value is :default and is implementation-defined but must support the base characters. 
    An error is signaled if the implementation does recognize the specified format.

    This argument may be specified if the :direction argument is :input, :output, or :io. 
    It is an error to write a character to the resulting stream that cannot be represented 
    by the specified file format. (However, the #\Newline character cannot produce such an 
    error; implementations must provide appropriate line division behavior for all character streams.)

When the caller is finished with the stream, it should close the file by using the close function. The with-open-file form does this automatically, and so is preferred for most purposes. open should be used only when the control structure of the program necessitates opening and closing of a file in some way more complex than provided by with-open-file. It is suggested that any program that uses open directly should use the special form unwind-protect to close the file if an abnormal exit occurs.

[Macro]
with-open-file (stream filename {options}*)
       {declaration}* {form}*
with-open-file evaluates the forms of the body (an implicit progn) with the variable stream bound 
to a stream that reads or writes the file named by the value of filename. The options are evaluated 
and are used as keyword arguments to the function open.

When control leaves the body, either normally or abnormally (such as by use of throw), the file 
is automatically closed. If a new output file is being written, and control leaves abnormally, 
the file is aborted and the file system is left, so far as possible, as if the file had never 
been opened. Because with-open-file always closes the file, even when an error exit is taken, 
it is preferred over open for most applications.

filename is the name of the file to be opened; it may be a string, a pathname, or a stream.

For example:
(with-open-file (ifile name 
                 :direction :input) 
  (with-open-file (ofile (merge-pathname-defaults ifile 
                                                  nil 
                                                  "out") 
                         :direction :output 
                         :if-exists :supersede) 
    (transduce-file ifile ofile)))
...
(with-open-file (ifile name 
                 :direction :input 
                 :if-does-not-exist nil) 
  ;; Process the file only if it actually exists. 
  (when (streamp name)
    (compile-cobol-program ifile)))

Implementation note: While with-open-file tries to automatically close the stream on exit from 
the construct, for robustness it is helpful if the garbage collector can detect discarded 
streams and automatically close them. 

...

READ

[Function]
read &optional input-stream eof-error-p eof-value recursive-p

read reads in the printed representation of a Lisp object from input-stream, builds a corresponding 
Lisp object, and returns the object.

Note that when the variable *read-suppress* is not nil, then read reads in a printed representation 
as best it can, but most of the work of interpreting the representation is avoided (the intent 
being that the result is to be discarded anyway). For example, all extended tokens produce 
the result nil regardless of their syntax. 

|#
(defun mload ()
  )

#|
Exo 4.17 - Définir 'get-defmacro' comme une macro.
|#

#|
Exo 4.18 - Définir la fonction 'displace' qui prend en argument 2 cellules, met dans la première
le contenu de la seconde et retourne la première. Rajouter le cas où le résultat de la macro-expansion
est un atome.
|#

#|
Exo 4.19 - Définir la fonction 'm-macroexmand-1' qui expanse une fois une macro méta-définie
par analogie avec 'macroexpand-1'
|# 

#|
Exo 4.20 - Définir la fonction m-macroexpand qui expanse complètement une macro métadéfinie,
par analogie avec 'macroexpand'. Le principe de 'macroexpand' est d'appliquer 'macroexpand-1'
tant que le résultat de l'expansion est toujours une macro. On traitera dans cette fonction
aussi bien les macros méta-définies que les prédéfinies.
|#

#|
Exo 4.21 - Définir la fonction 'meval-let' qui méta-évalue une expression 'let'.
|#

#|
Exo 4.22 - Définir la fonction 'meval-cond' qui méta-évalue une expression 'cond', 
comme si c'était une forme syntaxique.
|#

#|
Exo 4.23 - Etendre 'msetf' à l'affectation d'arité quelconque.
|#

#|
Exo 4.24 - Intégrer la macro-expansion de 'place' dans 'msetf' : traiter les deux cas de macros 
méta-définies et prédéfinies.
|#

#|
Exo 4.25 - Au lieu d'énumérer dans 'msetf' toutes les fonctions setf-able, le mieux est 
d'évaluer tous les arguments de 'place', de reconstruire l'expression 'place' en remplaçant les arguments
par leur quotée, d'évaluer 'val', puis de reconstruire l'expression 'expr' avec 'place' transformée et
la valeur de 'val' quotée. On peut alors évaluer 'expr' pour effectuer l'affectation : tous les
arguments étant quotés, cette évaluation peut se faire dans un environnement vide.
|#

#|
Exo 4.26 - Définir 3 fonctions de l'exemple du compteur dans le polycopié LISP
|#

#|
Exo 4.27 - Définir cette fonction 'meval-args*' qui est à 'meval-args' ce que 
'list*' est à 'list'.
|#

#|
Exo 4.28 - Tester les fermetures sur le schéma de terminalisation des récursions enveloppées
par passage de continuation. Voir polycopié de LISP.
|#

#|
Exo 4.29 - Redéfinir les fonctions 'make-closure' et 'meval-closure' pour tenir compte de 
l'environnement fonctionnel.
|#

#|
Exo 4.30 - Etendre le traitement de 'function' aux fonctions locales.
|#

#|
Exo 4.31 - Définir la fonction 'make-flet-fenv' qui construit cet environnement fonctionnel.
|#

#|
Exo 4.32 - Définir la fonction 'make-labels-fenv' qui construit cet environnement fonctionnel
circulaire. On appellera 'make-flet-fenv' en lui passant un environnement fonctionnel "vide"
qu'il s'agira ensuite de remplacer par son résultat même, par exemple par appel de 'displace'.
|#

#|
Exo 4.33 - Définir la fonction 'destruct' qui construit un environnement de façon similaire à 
'make-env' mais avec la destructuration.
|#

#|
Exo 4.34 - Traiter la forme syntaxique destructuring-bind' dans 'meval'.
|#

#|
Exo 4.35 - Etendre la fonction 'make-env' pour qu'elle inclue la destructuration sur les
paramètres obligatoires, tout en conservant la possibilité des mots-clés &optional, &key,
&rest avec leur syntaxe habituelle.
|#

#|
Exo 4.36 - Etendre la fonction 'destruct' pour qu'elle interdise la double occurrence d'un
paramètre dans l'arbre.
|#

#|
Exo 4.37 - Définir la fonction 'match' qui apparie un motif et une valeur dans un 
environnement qu'elle étend et retourne. En cas d'échec, retourne le mot-clé :fail.
La fonction est similaire à 'destruct' mais elle intègre ces nouvelles contraintes.
|#

#|
Exo 4.38 - Définir la fonction 'meval-case-match' qui implémente la forme syntaxique
'case-match' dans le méta-évaluateur.
|#

#|
Exo 4.39 - Définir la macro 'defil' qui construit progressivement la 'case-match' qui 
fait office de corps de la fonction.
Le filtrage s'applique particulièrement bien aux macros, les différents motifs correspondant
à l'analyse par cas à faire sur la syntaxe de l'expression.
|#

#|
Exo 4.40 - Définir la macro 'defil-macro' qui construit progressivement le 'case-match'
qui fait office de corps d'une macro.
|#

#|
Exo 4.41 - Définir la macro 'or' par filtrage. Faire de même pour les différents exemples de
macros du polycopié de LISP.
|#

#|
Exo 4.42 - Définir la fonction 'rewrite-1' qui prend en entrée une donnée et une liste 
de règles de réécriture et réécrit la donnée suivant la règle de réécriture donnée par le 
motif et la production. Retourne :fail si l'appariement ne réussit pas.
|#

#|
Exo 4.43 - Définir la fonction 'rewrite' qui prend en entrée une donnée et une liste de règles
de réécriture et réécrit la donnée tant qu'une règle s'applique.
|#

#|
Exo 4.44 - Définir la macro 'defrewrite-macro' qui définit une macro par des règles de
réécriture, comme les 'let-syntax' et 'syntax-rules' de SCHEME. Cela revient à remplacer
la construction explicite de l'expansion, avec 'backquote' par une construction implicite où
l'action associée à chaque motif est implicitement 'backquotée' et où chaque variable
figurant dans le motif y est implicitement virgulée.
|#

#|
Exo 4.45 - Définir la macro 'or' par règles de réécriture et faire de même pour les différents
exemples de macros du polycopié de LISP. Comment pourrait-ont éviter avec 'or' les problèmes
de capture de variable ? (cf. Section sur les macros dans le chapitre 3 du polycopié LISP).
|#

#|
Exo 4.46 - Etendre la fonction 'match' aux variables segments. On définira deux fonctions 
auxiliaires pour tester si une variable est segment et pour en extraire la variable simple
correspondante. Pour simplifier ce traitement, on peut utiliser une forme parenthésée pour
les segments, par exemple (*x), avec le risque de limiter les squelettes possibles : il faut
en tout cas bien placer la clause sur les segments.
|#

#|
Exo 4.47 - Définir dans 'meval' les deux formes syntaxiques 'delay' et 'force'.
|#

#|
Exo 4.48 - Il n'est en fait pas nécessaire de passer par des formes syntaxiques pour définir
les retardements. Définir 'delay' comme une macro et 'force' comme une fonction.
|#

#|
Exo 4.49 - Définir le flot 'enum-fibo' qui énumère la suite de Fibonacci. Voir aussi la 
fonction 'next-fibo' dans le polycopié de LISP.
|#

#|
Exo 4.50 - Définir le flot enum-prime qui énumère les nombres premiers. Voir aussi la 
fonction 'next-prime' dans le polycopié de LISP.
|#

#|
Exo 4.51 - Définir la fonction 'scheme-symbol-function'.
|# 

#|
(deftest jc-meval 
  (meval "bonjour") 
  "bonjour")
|#
