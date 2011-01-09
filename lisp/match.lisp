(require 'util "util") ;; n-consp

;; Syntaxe : (match <motif> expression)
;;       ex: (match (:a ? :c) '(a b c)) => t
;; Motif               Condition
;; (pattern * . rest)  ;; matche 0 à n occurences de pattern.
;;                     ;; Attention, la complexité est 2^n dans le pire des cas.
;; (pattern + . rest)  ;; idem, mais au moins 1 occurence
;; (pattern ? . rest)  ;; idem, 0 ou une occurences
;; (? and p1 ... pn)   (every (lambda (x) (funcall x expr)) '(p1 ... pn))
;; (? or p1 ... pn)    (some (lambda (x) (funcall x expr)) '(p1 ... pn))
;; (? <code>)          (funcall (lambda (x) <code>) expr)
;;                     ;; Note : fonctionne aussi pour and et or.
;; (? (lambda ...))    (funcall (lambda ...) expr)
;; (? symbole)         (funcall 'symbole expr) ;; en général, #'symbole est un prédicat.
;; (a . rest)          (and (match a (car expr)) (match rest (cdr expr)))
;; ()                  (null expr)
;; $                   (and (atom expr) (not (null expr)))
;; $$                  (symbolp expr)
;; $k                  (keywordp expr)
;; $&                  (and (symbolp expr) (member expr '(&optional &rest &key &allow-other-keys &aux)))
;; @                   liste propre : (and (listp expr) (match @ (cdr expr)))
;; @.                  cons : (consp expr)
;; _                   t
;; :symbole            Nom pour la capture (voir le paragraphe ci-dessous)
;; symbole             (eq 'symbole expr)
;; Autres valeurs      (equal <valeur> epxr)
;;
;; TODO : faire en sorte que si pattern est une chaîne, alors c'est une
;; regexp qu'on matche avec l'expression correspondante.
;;
;; De plus, si :symbole précède un pattern "simple" (pas de * + ?),
;; si le motif correspond à l'expression, plutôt que de renvoyer T,
;; match renverra une liste associative où :sybole est associé à la
;; portion de l'expression qui correspond au motif suivant :symbole.
;; Dans le cas où le pattern n'est pas "simple", la valeur correspondante
;; sera une liste de toutes les occurences de pattern

(defun pattern-match-do-lambdas-transform (pattern)
  (mapcar (lambda (pred)
            (cond ((atom pred)               (list 'function pred))
                  ((eq (car pred) 'function) pred)
                  ((eq (car pred) 'lambda)   pred)
                  (t
                   `(lambda (x) x ,pred))))
          pattern))

(defun pattern-match-do-lambdas-1 (pattern)
  (if (atom pattern)
      `',pattern
      `(list ',(first pattern)
             ',(second pattern)
             ,(if (second pattern)
                  (let ((?-clause (cdr (third pattern)))
                        (type '_))
                    (when (and (consp ?-clause) (member (car ?-clause) '(nil _ $ $$ $k $& @ @.)))
                      (setq type (car ?-clause))
                      (setq ?-clause (cdr ?-clause)))
                    ;; TODO : (? or foo (? _ and bar baz) (? $ and quux))
                    (cond ((atom ?-clause)            `(list ',type 'and #'identity))
                          ((eq 'and (first ?-clause)) `(list ',type 'and ,@(pattern-match-do-lambdas-transform (cdr ?-clause))))
                          ((eq 'or  (first ?-clause)) `(list ',type 'or  ,@(pattern-match-do-lambdas-transform (cdr ?-clause))))
                          (t                          `(list ',type 'and ,@(pattern-match-do-lambdas-transform ?-clause)))))
                  (pattern-match-do-lambdas-1 (third pattern)))
             ',(fourth pattern)
             ,(pattern-match-do-lambdas-1 (fifth pattern)))))

(defmacro pattern-match-do-lambdas (pattern)
  "Transforme les (? (<code>)) et (? (lambda ...)) en vrais lambdas."
  (pattern-match-do-lambdas-1 pattern))

(defun transform-symbol-to-multi (pat)
  (let ((str-sym (string pat)))
    (if (< (length str-sym) 2)
        pat
        (let* ((sym (map 'list #'identity str-sym))
               (lsym (car (last sym))))
          (if (or (char= lsym #\*)
                  (char= lsym #\+)
                  (char= lsym #\?))
              (list (intern (format nil "~{~a~}" (butlast sym)))
                    (intern (string lsym)))
              pat)))))

(defun pattern-match-preprocess-multi (pattern)
  "Transforme les symbol*, symbol+ et symbol?
   en symbol *, symbol + et symbol ?"
  (cond ((and (consp pattern) (eq '? (first pattern)))
         pattern) ;; On ne touche pas les (? ...)
        ((consp pattern)
         (labels ((transform-symbol-to-multi (pat)
                    (let ((str-sym (string pat)))
                      (if (< (length str-sym) 2)
                          pat
                          (let* ((sym (map 'list #'identity str-sym))
                                 (lsym (car (last sym))))
                            (if (or (char= lsym #\*)
                                    (char= lsym #\+)
                                    (char= lsym #\?))
                                (list (intern (format nil "~{~a~}" (butlast sym)))
                                      (intern (string lsym)))
                                pat)))))
                  (recurse (pat)
                    (cond
                      ((null pat) nil)
                      ((symbolp pat) (transform-symbol-to-multi pat))
                      ((atom pat) pat)
                      ((keywordp (car pat)) ;; TODO : non testé !!!
                       `(,(car pat)
                          ,@(recurse (cdr pat))))
                      ((symbolp (car pat))
                       (let ((transf (transform-symbol-to-multi (car pat))))
                         (if (consp transf)
                             `(,@transf ,@(recurse (cdr pat)))
                             `(,transf ,@(recurse (cdr pat))))))
                      (t (cons (pattern-match-preprocess-multi (car pat))
                               (recurse (cdr pat)))))))
           (recurse pattern)))
        ((symbolp pattern)
         (transform-symbol-to-multi pattern))
        (t
         pattern)))

(defun keyword-to-symbol (keyword)
  (intern (format nil "~a" keyword)))

(defun pattern-match-preprocess-capture (pattern &optional capture-name)
  "Transforme pattern en un arbre (capture-name is-predicate pattern multi rest)."
  (if (and (consp pattern) (keywordp (car pattern)))
      ;; capture-name
      (if (and (n-consp 2 (cdr pattern)) (member (caddr pattern) '(* + ?)))
          ;; avec capture-name, avec multi
          (list (keyword-to-symbol capture-name)
                nil
                (pattern-match-preprocess-capture (second pattern) (first pattern))
                (third pattern)
                (pattern-match-preprocess-capture (cdddr pattern)))
          ;; avec capture-name, sans multi
          (cond
            ;; (:x . a)
            ((atom (cdr pattern))
             (list (keyword-to-symbol (car pattern))
                   nil
                   (cdr pattern)
                   nil
                   nil))
            ;; (:x . (? ...))
            ((and (consp pattern) (eq '? (cadr pattern)))
             (list (keyword-to-symbol (car pattern))
                   t
                   (cdr pattern)
                   nil
                   nil)) ;; TODO
            ;; (:x cadr-pattern . cddr-pattern)
            (t
             (list (keyword-to-symbol capture-name)
                   nil
                   (pattern-match-preprocess-capture (cadr pattern) (car pattern))
                   nil
                   (pattern-match-preprocess-capture (cddr pattern))))))
      ;; pas de capture-name
      (if (and (n-consp 2 pattern) (member (cadr pattern) '(* + ?)))
          ;; sans capture-name, avec multi
          (list (keyword-to-symbol capture-name)
                nil
                (pattern-match-preprocess-capture (first pattern))
                (second pattern)
                (pattern-match-preprocess-capture (cddr pattern)))
          ;; sans capture-name, sans multi
          (cond
            ;; a
            ((atom pattern)
             (list (keyword-to-symbol capture-name)
                   nil
                   pattern
                   nil
                   nil))
            ;; (? ...)
            ((and (consp pattern) (eq '? (car pattern)))
             (list (keyword-to-symbol capture-name)
                   t
                   pattern
                   nil
                   nil))
            ;; (car-pattern . cdr-pattern)
            (t
             (list (keyword-to-symbol capture-name)
                   nil
                   (pattern-match-preprocess-capture (car pattern))
                   nil
                   (pattern-match-preprocess-capture (cdr pattern))))))))

;; Fonctionnement du match avec *
;; 
;; (match (:x (:y _ :z _)* :e _) '((a b) (1 2) (foo bar) x))
;; ;; greedy match ok
;; => ((:x (a b)) (:y a) (:z b))   ---> (match (:x (:y _ :z _)* :e _) '((1 2) (foo bar) x))
;;    [________________________]        ;; greedy match ok
;;                |                     => ((:x (1 2)) (:y 1) (:z 2))   ---> (match (:x (:y _ :z _)* :e _) '((foo bar) x))
;;                |                        [________________________]        ;; greedy match ok
;;                |                                    |                     => ((:x (foo bar)) (:y foo) (:z bar))   ---> (match (:x (:y _ :z _)* :e _) '(x))
;;                |                                    |                        [________________________________]        ;; not greedy match !!!!
;;                |                                    |                                                  |                   [ car = make-empty-matches ] [cdr = matches non-greedy ]
;;                |                                    +-------------+                                    v               => (((:x nil) (:y nil) (:z nil)) (:e x))
;;                |                                                  |                           [ --- param 1 = matches here --- ] ([ :x lst of rest of greedy ] [ matches non-greedy ])
;;                |                                                  |       => (append-captures ((:x (foo bar)) (:y foo) (:z bar)) (((:x nil) (:y nil) (:z nil)) (:e x)))
;;                |                                                  |           [ ---- greedy matches appended --------] [ matches non-greedy ]
;;                +-------------+                                    |       => (((:x ((foo bar))) (:y (foo)) (:z (bar))) (:e x))
;;                              |                                    |          [_______________________________________________]
;;                              |                                    v                                          v
;;                              |                           [________________________] [_______________________________________________]
;;                              |       => (append-captures ((:x (1 2)) (:y 1) (:z 2)) (((:x ((foo bar))) (:y (foo)) (:z (bar))) (:e x)))
;;                              |       => (((:x ((1 2) (foo bar))) (:y (1 foo)) (:z (2 bar))) (:e x))
;;                              |          [_________________________________________________________]
;;                              v                                                 v
;;                     [________________________] [_________________________________________________________]
;; => (append-captures ((:x (a b)) (:y a) (:z b)) (((:x ((1 2) (foo bar))) (:y (1 foo)) (:z (2 bar))) (:e x)))
;; => (((:x ((a b) (1 2) (foo bar))) (:y (a 1 foo)) (:z (b 2 bar))) (:e x))
;;     |                                                          |
;;     |                      first-multi = t                     |
;;     |              => (append (car ___) (cdr ___))             |
;;     v                                                          v
;; => ( (:x ((a b) (1 2) (foo bar))) (:y (a 1 foo)) (:z (b 2 bar))  (:e x))

(defun append-captures-1 (captures-1 captures-2)
  (if (endp captures-1)
      nil
      (if (caar captures-1) ;; ignorer les captures nommées nil
          (cons (cons (caar captures-1) ;; nom de capture
                      (cons (cdar captures-1) ;; nouvelle capture
                            (cdr (assoc (caar captures-1) captures-2))))
                (append-captures-1 (cdr captures-1) captures-2))
          (append-captures-1 (cdr captures-1) captures-2))))

(defun append-captures (captures-1 captures-2)
  "captures-1 et 2 sont des alist nom-capture . arbre-capture
   Renvoie une alist nom-capture . (append arbre-c1 arbre-c2)"
  (cons (append-captures-1 captures-1 (car captures-2))
        (cdr captures-2)))

(defun make-empty-matches-1 (pattern result)
  (if (atom pattern)
      result
      (let ((here (if (first pattern)       ;; pas les captures nommées nil
                      (acons (first pattern) nil result)
                      result)))
        (if (second pattern) ;; ne pas descendre dans les les (? ...)
            here
            (make-empty-matches-1 (fifth pattern)
                                  (make-empty-matches-1 (third pattern) here))))))

(defun make-empty-matches (pattern)
  (reverse (make-empty-matches-1 pattern '())))
   
(defun acons-capture (capture-name value captures)
  (if (or capture-name (not captures))
      (acons capture-name value captures)
      captures))

(defun append-car-cdr-not-nil (c)
  (if (or (car c) (cdr c))
      (append (car c) (cdr c))
      (acons nil nil nil)))

(defun append-not-nil-1 (a b)
  (if (endp a)
      b
      (if (caar a)
          (cons (car a) (append-not-nil-1 (cdr a) b))
          (append-not-nil-1 (cdr a) b))))

(defun append-not-nil (a b)
  (or (append-not-nil-1 a b)
      (acons nil nil nil)))

(declaim (ftype function pattern-match)) ;; récursion mutuelle recursive-backtrack / pattern-match
(defun recursive-backtrack (pattern rest expr capture-name)
  (or
   ;; match greedy (on avance dans le *)
   (and (consp expr)
        (let ((greedy-left (pattern-match pattern (car expr))))
          (when greedy-left
            (let ((greedy-right (recursive-backtrack pattern rest (cdr expr) capture-name)))
              (when greedy-right
                (append-captures (acons-capture capture-name (car expr) greedy-left)
                                 greedy-right))))))
   ;; match non-greedy (on match avec le rest)
   (let ((non-greedy (pattern-match rest expr)))
     (when non-greedy
       (cons (acons-capture capture-name expr (make-empty-matches pattern))
             non-greedy)))))

(defun pattern-match (pat expr)
  (let ((capture-name (first pat))
        (is-predicate (second pat))
        (pattern (third pat))
        (multi (fourth pat))
        (rest (fifth pat)))
    (if multi
        (if (not (listp expr))
            nil
            (cond
              ;; (pattern * ...)
              ((eq multi '*)
               (let ((match (recursive-backtrack pattern rest expr capture-name)))
                 (when match
                   (append-car-cdr-not-nil match))))
              ;; (pattern + ...)
              ((eq multi '+)
               (let ((first-match (and (consp expr) (pattern-match pattern (car expr)))))
                 (when first-match
                   (let ((match (recursive-backtrack pattern rest (cdr expr) capture-name)))
                     (when match
                       (let ((result (append-captures first-match match)))
                         (append-car-cdr-not-nil result)))))))
              ;; (pattern ? ...)
              ((eq multi '?)
               (let ((match (and (consp expr) (pattern-match pattern (car expr)))))
                 (or (when match
                       (let ((match-rest (pattern-match rest (cdr expr))))
                         (when match-rest
                           ;; Attention, les trois lignes suivantes ont été modifiées sans que je comprenne vraiement les manipulations...
                           (append-car-cdr-not-nil
                            (append-captures match (cons (acons-capture capture-name expr (make-empty-matches pattern))
                                                         match-rest))))))
                     (let ((match-only-rest (pattern-match rest expr)))
                       (when match-only-rest
                         (append (acons-capture capture-name expr (make-empty-matches pattern))
                                 match-only-rest))))))))
        (if rest
            ;; (pattern . rest)
            (and (consp expr)
                 (let ((left (pattern-match pattern (car expr))))
                   (when left
                     (let ((right (pattern-match rest (cdr expr))))
                       (when right
                         (acons-capture capture-name expr (append-not-nil left right)))))))
            ;; pattern est un atom
            (cond
              ;; (? <prédicat(s)>)
              (is-predicate
               (when (and (pattern-match `(nil nil ,(car pattern) nil nil) expr)
                          (cond
                            ;; (? _ and symbole-1 ... symbole-n)
                            ((eq 'and (second pattern))
                             (every (lambda (predicat) (funcall predicat expr)) (cddr pattern)))
                            ;; (? _ or symbole-1 ... symbole-n)
                            ((eq 'or (second pattern))
                             (some (lambda (predicat) (funcall predicat expr)) (cddr pattern)))))
                 (acons-capture capture-name expr nil)))
              ;; ()
              ((null pattern)
               (when (null expr)
                 (acons-capture capture-name expr nil)))
              ;; $
              ((eq '$ pattern)
               (when (and (atom expr)
                          (not (null expr)))
                 (acons-capture capture-name expr nil)))
              ;; $$
              ((eq '$$ pattern)
               (when (symbolp expr)
                 (acons-capture capture-name expr nil)))
              ;; $k
              ((eq '$k pattern)
               (when (keywordp expr)
                 (acons-capture capture-name expr nil)))
              ;; $&
              ((eq '$& pattern)
               (when (and (symbolp expr) (member expr '(&optional &rest &key &allow-other-keys &aux)))
                 (acons-capture capture-name expr nil)))
              ;; @
              ((eq '@ pattern)
               (when (propper-list-p expr)
                 (acons-capture capture-name expr nil)))
              ;; @.
              ((eq '@. pattern)
               (when (consp expr)
                 (acons-capture capture-name expr nil)))
              ;; _
              ((eq '_ pattern)
               (acons-capture capture-name expr nil))
              ;; Autres valeurs (symbole, nombre, etc.)
              (t
               (when (equal pattern expr)
                 (acons-capture capture-name expr nil))))))))

(defmacro pattern-match-preprocess (pattern)
  "Tous les preprocess de pattern-match en un seul appel."
  `(pattern-match-do-lambdas
    ,(pattern-match-preprocess-capture
      (pattern-match-preprocess-multi
       pattern))))

(defmacro real-match (pattern expr body &optional else-clause)
  (let* ((result-sym (make-symbol "RESULT"))
         (result-of-if-sym (make-symbol "RESULT-OF-IF"))
         (pattern-sym (make-symbol "PATTERN"))
         (else-sym (make-symbol "ELSE"))
         (pattern-preproc (pattern-match-preprocess-capture
                           (pattern-match-preprocess-multi
                            pattern)))
         (capture-names (mapcar #'car (make-empty-matches pattern-preproc))))
    `(let* ((,pattern-sym (pattern-match-do-lambdas ,pattern-preproc))
            (,result-sym (pattern-match ,pattern-sym ,expr))
            (,result-of-if-sym
             (if ,result-sym
                 ;; Si le match a été effectué avec succès
                 ,@(if body
                       ;; Si on a un body
                       ;; On bind les variables correspondant aux noms de capture
                       `((let ,(mapcar (lambda (x) `(,x (cdr (assoc ',x ,result-sym))))
                                       capture-names)
                           ;; "utilisation" des variables pour éviter les warning unused variable.
                           ,@capture-names
                           ;; On définit la fonction "else" qui produit le "code secret" permettant d'exécuter le else.
                           (labels ((else () ',else-sym))
                             ;; On exécute le body
                             ,@body)))
                       ;; S'il n'y a pas de body, on renvoie l'alist des captures s'il y en a ou T sinon.
                       (if capture-names
                           `((remove nil ,result-sym :key #'car))
                           `(t)))
                 ;; Si on ne match pas, on renvoie le "code secret" permettant d'exécuter le else.
                 ',else-sym)))
       ;; Si le résultat est le "code secret" du else, on fait le else, sinon on renvoie le résultat
       (if (eq ,result-of-if-sym ',else-sym)
           ,else-clause
           ,result-of-if-sym))))

(defmacro match (pattern expr &rest body)
  (if (keywordp pattern)
      `(real-match (,pattern . ,expr) ,(car body) ,(cdr body))
      `(real-match ,pattern ,expr ,body)))

(defmacro if-match (pattern expr body-if body-else)
  `(real-match ,pattern ,expr (,body-if) ,body-else))

(defmacro cond-match-1 (expr cond-clauses)
  (if (endp cond-clauses)
      'nil
      (if (keywordp (caar cond-clauses))
          `(real-match (,(caar cond-clauses) . ,(cadar cond-clauses))
                       ,expr
                       ,(cddar cond-clauses)
                       (cond-match-1 ,expr ,(cdr cond-clauses)))
          `(real-match ,(caar cond-clauses)
                       ,expr
                       ,(cdar cond-clauses)
                       (cond-match-1 ,expr ,(cdr cond-clauses))))))

(defmacro cond-match (expr &rest cond-clauses)
  (let ((expr-sym (make-symbol "expr")))
    `(let ((,expr-sym ,expr))
       (cond-match-1 ,expr-sym ,cond-clauses))))

;; Explication du defmatch-closures
;; 
;; ========== État initial ==========================================================
;; 
;; next-rule -----------> (  (lambda else)  )
;;                           ^
;; first-car-next-rule ------|
;; 
;; ========== Après un add-pattern-xxx ==============================================
;;
;; next-rule -----------> (  (lambda else)  )
;;                           ^
;;                           `----------------------------------------.
;;                                                                    |
;; old-next-rule -------> (  (lambda (x) (if pattern-1 t (funcall car-next-rule)))  )
;;                           ^
;; first-car-next-rule ------|
;;
;; ========== Après un autre add-pattern-xxx ========================================
;;
;; next-rule -----------> (  (lambda else)  )
;;                           ^
;;                           `----------------------------------------.
;;                                                                    |
;; old-next-rule -------> (  (lambda (x) (if pattern-2 t (funcall car-next-rule)))  )
;;                           ^
;;                           `----------------------------------------.
;;                                                                    |
;;                        (  (lambda (x) (if pattern-1 t (funcall car-next-rule)))  )
;;                           ^
;; first-car-next-rule ------|

(defmacro defmatch-closures (name &rest patbody)
  "Une première version de defmatch, techniquement intéressante, mais avec beaucoup trop de closures..."
  (let ((add-pattern-function (intern (format nil "ADD-PATTERN-~a" name)))
        (set-else-function (intern (format nil "SET-ELSE-~a" name))))
    (if patbody
        (let ((pattern (car patbody))
              (body (cdr patbody))
              (param-expr-sym (make-symbol "param-expr"))
              (tail-sym (make-symbol "tail")))
          (if (eq pattern 'else)
              `(,set-else-function ,(if (n-consp 2 body)
                                        `(lambda ,(car body) ,@(cdr body))
                                        `(lambda (x) x ,@body)))
              (if (keywordp pattern)
                  `(defmatch-closures ,name (,pattern . ,(car body)) ,@(cdr body))
                  `(,add-pattern-function
                    (lambda (,param-expr-sym ,tail-sym)
                      (real-match ,pattern ,param-expr-sym ,body (funcall ,tail-sym ,param-expr-sym)))))))
        `(let* ((else-rule (list (lambda (x) x nil)))
                (next-rule (list (list else-rule)))
                (first-call-next-rule
                 (let ((car-next-rule (car next-rule)))
                   (lambda (x) (funcall (caar car-next-rule) x)))))
           (defun ,name (x)
             (funcall first-call-next-rule x))
           (defun ,set-else-function (else)
             (setf (car else-rule) else))
           (defun ,add-pattern-function (func)
             (let ((old-next-rule next-rule))
               (setq next-rule (list (list else-rule)))
               (let ((car-next-rule (car next-rule)))
                 (setf (car (car old-next-rule))
                       (list (lambda (x) (funcall func x (lambda (x) (funcall (caar car-next-rule) x)))))))))))))

(defmacro defmatch (name &rest patbody)
  "Version de defmatch avec une seule closure par pattern plus deux par name."
  (let ((add-pattern-function (intern (format nil "ADD-PATTERN-~a" name)))
        (set-else-function (intern (format nil "SET-ELSE-~a" name))))
    (if patbody
        (let ((pattern (car patbody))
              (body (cdr patbody))
              (param-expr-sym (make-symbol "param-expr")))
          (if (eq pattern 'else)
              `(,set-else-function ,(if (n-consp 2 body)
                                        `(lambda ,(car body) ,@(cdr body))
                                        `(lambda (x) x ,@body)))
              (if (keywordp pattern)
                  `(defmatch ,name (,pattern . ,(car body)) ,@(cdr body))
                  `(,add-pattern-function
                    (lambda (,param-expr-sym)
                      (real-match ,pattern
                                  ,param-expr-sym
                                  ((cons t ,(cons 'progn body)))
                                  nil))))))
        `(let* ((rules nil)
                (else-rule (lambda (x) x nil)))
           (defun ,name (x)
             (cdr (or (some (lambda (r) (funcall r x)) rules)
                      (cons t (funcall else-rule x)))))
           (defun ,set-else-function (else)
             (setf else-rule else))
           (defun ,add-pattern-function (func)
             (setf rules (append rules (list func))))))))

;; Version de match-automaton avec loop :
;; `(loop
;;     with ,state = ',initial-state
;;     with accumulator = nil
;;     for step = (cond-match ...)
;;     when (eq state 'accept)
;;       return (reverse accumulator)
;;     when (eq state 'reject)
;;       return nil
;;     do (setq state (car step)))

#|
Syntaxe du match-automaton :
(match-automaton expression initial-state
                 (stateX stateY [pattern] [code [code ...]])
                 (stateX stateY [pattern] [code [code ...]])
                 ...)
- certains noms d'état pour stateX et stateY sont réservés : initial accept reject do collect

(stateX stateY)
- Si pattern n'est pas fourni, alors stateY est en général accept ou reject, pour indiquer qu'on accepte ou rejette l'expression si on arrive sur cet état à la fin.
- stateY peut aussi être un autre état, auquel cas, si tous les pattern de stateX échouent, ou si on se retrouve à la fin (de la liste qu'on analyse),
  on jump vers stateY au lieu d'aller à reject (note : il est possible de faire une boucle infinie ainsi).

(stateX stateY pattern)
- Si pattern est fourni, cela signifie «Si l'élément courant de l'expression matche avec ce pattern, passer à l'élément suivant et aller dans l'état stateY.».
- Lorsque l'expression est acceptée, pattern-match renvoie une liste associative ((state1 élément-1 ... élément-n) (state2 e1..eN) ...)
- Les éléments à droite de chaque nom d'état sont obtenus de la manière suivante :
  - Si code est fourni: (stateX stateY pattern code*)
    il est exécuté, et sa valeur de retour est ajoutée à la fin de la liste correspondant à stateX (l'état de départ).
  - Si code n'est pas fourni : (stateX stateY pattern)
    rien n'est ajouté à cette liste.
- Lorsqu'on est sur un certain état, chaque transition partant de cet état est testée dans l'ordre d'écriture, et la première qui matche est choisie.

De plus, il est possible de spécifier des clauses spéciales, au lieu des (stateX stateY [pattern] [code]) :
 - (initial code ...)         qui exécute le code avant de démarrer (pas très utile…)

 - (accept code ...)          qui exécute le code dans le cas où l'expression est acceptée. La valeur renvoyée par ce code sera la valeur de retour de match-automaton.
                              Dans cette clause, on peut accéder à la valeur qui aurait dû être renvoyée via la variable return-value.

 - (reject code ...)          qui exécute le code dans le cas où l'expression est rejetée. La valeur renvoyée par ce code sera la valeur de retour de match-automaton.
                              Dans cette clause, le dernier élément considéré est stocké dans last-element.
                              Cet élément est celui qui a causé le reject, ou nil si on est au début de la liste, ou nil si on est à la fin de la liste sans accept.
                              De même, le dernier état courant est dans last-state.

 - (stateX do code ...)       qui exécute code à chaque fois qu'on entre sur stateX.
                              On peut accéder à last-element et last-state.

 - (stateX collect code ...)  qui exécute code à chaque fois qu'on entre sur stateX et ajoute sa valeur de retour à la fin de la liste correspondant à stateX.
                              On peut accéder à last-element et last-state.

Il peut y avoir plusieurs initial, accept et reject, auquel cas tous sont exécutés dans l'ordre d'écriture, et sont concaténés dans un progn,
donc seule la valeur de la dernière expression de la dernière clause est renvoyée.
|#

;; ATTENTION, par excès de flemme, match-automaton ne supporte pas la syntaxe
;;   de découplage du nom de capture et de l'élément : (macro :var atom expr code)
;; ATTENTION, j'ai renoncé à rendre ce code lisible.
(defun match--transitions-to-progns (transitions)
  ;; On remet to, pattern et code bout à bout (c'est tout du code)
  (mapcar (lambda (x) `(progn ,(car x) ,@(cadr x) ,@(caddr x)))
          transitions))

(defmacro match-automaton (expr initial-state &rest rules)
  (match ((:from $$ :to _ :pattern _? :code _*) *) rules
         (let ((storage (mapcar (lambda (s) (cons s (make-symbol (format nil "STORAGE-~a" (string s))))) (remove-duplicates from)))
               (expr-sym (make-symbol "EXPR"))
               (block-sym (make-symbol "BLOCK"))
               (grouped-transitions (group (mapcar #'list from to pattern code)))
               (last-state-sym (make-symbol "LAST-STATE"))
               (last-element-sym (make-symbol "LAST-ELEMENT")))
           `(let (,@(mapcar (lambda (s) `(,(cdr s) nil)) storage)
                  (,expr-sym ,expr)
                  (,last-state-sym 'initial)
                  (,last-element-sym nil))
              (block ,block-sym
                (tagbody
                 initial
                   (progn ,(match--transitions-to-progns (cdr (assoc 'initial grouped-transitions))))
                   (go ,initial-state)
                 accept
                   (let ((return-value (list ,@(mapcar (lambda (s) `(cons ',(car s) (reverse ,(cdr s)))) storage))))
                     return-value
                     (return-from ,block-sym
                       (progn return-value
                              ,@(match--transitions-to-progns (cdr (assoc 'accept grouped-transitions))))))
                 reject
                   (return-from ,block-sym
                     (let ((last-element ,last-element-sym)
                           (last-state ,last-state-sym))
                       last-element
                       last-state
                       (progn nil
                              ,@(match--transitions-to-progns (cdr (assoc 'reject grouped-transitions))))))
                   ;; On va générer ceci :
                   ;; state1
                   ;;   (cond-match ... (go state2) ... (go state1) ...)
                   ;; state2
                   ;;   (cond-match ... (go staten) ... (go reject) ...)
                   ;; staten
                   ;;   (cond-match ... (go accept) ... (go state1) ...)))
                   ,@(loop
                        for (from . transitions) in grouped-transitions
                        and temp-do = nil
                        and temp-collect = nil
                        ;; syntaxe (stateX code) => exécute le code à chaque fois qu'on rentre dans stateX.
                        for jump = (member nil (reverse transitions) :key #'second)
                        unless (member from '(initial accept reject))
                          collect from
                          and collect `(setq ,last-state-sym ',from)
                          and collect `(setq ,last-element-sym (car ,expr-sym))
                          and if jump
                            ;; va à l'état désigné par la dernière transition "finale".
                            collect `(when (endp ,expr-sym) (go ,(caar jump)))
                          else
                            collect `(when (endp ,expr-sym) (go reject))
                          end
                          and do (setq temp-do      (remove nil (mapcar (lambda (x) (when (eq 'do      (car x)) `(progn ,@(cadr x) ,@(caddr x)))) transitions)))
                          and do (setq temp-collect (remove nil (mapcar (lambda (x) (when (eq 'collect (car x)) `(progn ,@(cadr x) ,@(caddr x)))) transitions)))
                          and when (or temp-do temp-collect)
                            collect `(let ((last-element ,last-element-sym)
                                           (last-state ,last-state-sym))
                                       last-element
                                       last-state
                                       ,@(if temp-do `((progn ,@temp-do)) nil)
                                       ,@(if temp-collect `((push (progn ,@temp-collect) ,(cdr (assoc from storage)))) nil))
                          end
                          and collect `(cond-match (car ,expr-sym)
                                                   ,@(loop
                                                        for (to pattern code) in transitions
                                                        unless (or (not pattern) (eq to 'do) (eq to 'collect))
                                                          if code
                                                            collect `(,@pattern
                                                                    (push (progn ,@code) ,(cdr (assoc from storage)))
                                                                    (setq ,expr-sym (cdr ,expr-sym))
                                                                    (go ,to))
                                                          else
                                                            collect `(,@pattern
                                                                      (setq ,expr-sym (cdr ,expr-sym))
                                                                      (go ,to)))
                                                   (_ ,(if jump `(go ,(caar jump)) '(go reject)))))))))))

(require 'test-unitaire "test-unitaire")
(erase-tests match)

;;;; Tests de matching (vrai / faux)

;;; Symboles, chiffres, etc

(deftest (match atom divers) (match a              'a)             t   #'booleq)
(deftest (match atom divers) (match 1              '1)             t   #'booleq)
(deftest (match atom divers) (match 1              '1.0)           nil #'booleq) ;; TODO : devrait être nil ou t ?
(deftest (match atom divers) (match a              'b)             nil #'booleq) ;; $ oui, a non
(deftest (match atom divers) (match 1              '2)             nil #'booleq) ;; $ oui, 1 non
(deftest (match atom divers) (match a              '())            nil #'booleq)
(deftest (match atom divers) (match a              '(a))           nil #'booleq)
(deftest (match atom divers) (match a              '(a b))         nil #'booleq)
(deftest (match atom divers) (match a              '(a . b))       nil #'booleq)

;; Même chose que pour symboles, mais avec $

(deftest (match atom $) (match $              'a)             t   #'booleq)
(deftest (match atom $) (match $              '1)             t   #'booleq)
(deftest (match atom $) (match $              '1.0)           t   #'booleq) ;; $ oui, 1 je sais pas
(deftest (match atom $) (match $              'b)             t   #'booleq) ;; $ oui, a non
(deftest (match atom $) (match $              '2)             t   #'booleq) ;; $ oui, 1 non
(deftest (match atom $) (match $              '())            nil #'booleq)
(deftest (match atom $) (match $              '(a))           nil #'booleq)
(deftest (match atom $) (match $              '(a b))         nil #'booleq)
(deftest (match atom $) (match $              '(a . b))       nil #'booleq)

;;; Listes et sous-lites
(deftest (match listes) (match ()             '())            t   #'booleq)
(deftest (match listes) (match ()             '(a))           nil #'booleq)
(deftest (match listes) (match ()             'a)             nil #'booleq)
(deftest (match listes) (match ()             '(a . b))       nil #'booleq)

(deftest (match listes) (match (a)            '(a))           t   #'booleq)
(deftest (match listes) (match (a)            '(b))           nil #'booleq)
(deftest (match listes) (match (a)            'a)             nil #'booleq)
(deftest (match listes) (match (a)            '(a b))         nil #'booleq)
(deftest (match listes) (match (a)            '())            nil #'booleq)

(deftest (match listes) (match (a b)          '(a b))         t   #'booleq)
(deftest (match listes) (match (a b)          '(a c))         nil #'booleq)
(deftest (match listes) (match (a b)          '(c b))         nil #'booleq)
(deftest (match listes) (match (a b)          '(a))           nil #'booleq)
(deftest (match listes) (match (a b)          '())            nil #'booleq)
(deftest (match listes) (match (a b)          'a)             nil #'booleq)
(deftest (match listes) (match (a b)          '(a b c))       nil #'booleq)

(deftest (match listes) (match (a (1 2 3) c)  '(a (1 2 3) c)) t   #'booleq)
(deftest (match listes) (match (a (1 2 3) c)  '(a (1 2) c))   nil #'booleq)
(deftest (match listes) (match (a (1 2 3) c)  '(a () c))      nil #'booleq)
(deftest (match listes) (match (a (1 2 3) c)  '(x (1 2 3) c)) nil #'booleq)

;;; _

(deftest (match _) (match _              'a)             t   #'booleq)
(deftest (match _) (match _              '(a b))         t   #'booleq)
(deftest (match _) (match _              '())            t   #'booleq)

(deftest (match _ fin-de-liste) (match (a _)          '(a b))         t   #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a (1 2 3)))   t   #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a (b c)))     t   #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a (b)))       t   #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a ()))        t   #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a (b . c)))   t   #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(x b))         nil #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(x (b)))       nil #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a))           nil #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '(a b c))       nil #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          '())            nil #'booleq)
(deftest (match _ fin-de-liste) (match (a _)          'a)             nil #'booleq)

(deftest (match _ milieu-de-liste) (match (a _ c)        '(a b c))       t   #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a (1 2 3) c)) t   #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a (b c) c))   t   #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a (b) c))     t   #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a () c))      t   #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a (b . c) c)) t   #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(x (b) c))     nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(x b c))       nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(x (b) c))     nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a c))         nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a))           nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a b c d))     nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '())            nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        'a)             nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a b b c))     nil #'booleq)
(deftest (match _ milieu-de-liste) (match (a _ c)        '(a b x c))     nil #'booleq)

(deftest (match _ fin-de-cons) (match (a . _)        '(a b))         t   #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(a b c))       t   #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(a))           t   #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(a b . c))     t   #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(a . b))       t   #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(a . (b c)))   t   #'booleq) ;; equivalent à '(a b c)
(deftest (match _ fin-de-cons) (match (a . _)        '(a . ()))      t   #'booleq) ;; equivalent à '(a)
(deftest (match _ fin-de-cons) (match (a . _)        '(x . b))       nil #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(x b c))       nil #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '(x))           nil #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        '())            nil #'booleq)
(deftest (match _ fin-de-cons) (match (a . _)        'a)             nil #'booleq)

(deftest (match _ cons) (match (a _ . _)      '(a b . c))     t   #'booleq)
(deftest (match _ cons) (match (a _ . _)      '(a () . ()))   t   #'booleq)
(deftest (match _ cons) (match (a _ . _)      '(a (1) . (2))) t   #'booleq)
(deftest (match _ cons) (match (a _ . _)      '(a 1 . (2)))   t   #'booleq) ;; equivalent à '(a 1 2)
(deftest (match _ cons) (match (a _ . _)      '(a (1) . 2))   t   #'booleq)
(deftest (match _ cons) (match (a _ . _)      '(a))           nil #'booleq)
(deftest (match _ cons) (match (a _ . _)      'a)             nil #'booleq)

(deftest (match _ liste) (match (_)            '(a))           t   #'booleq)
(deftest (match _ liste) (match (_)            '(()))          t   #'booleq)
(deftest (match _ liste) (match (_)            '((a b)))       t   #'booleq)
(deftest (match _ liste) (match (_)            '(a b))         nil #'booleq)
(deftest (match _ liste) (match (_)            '())            nil #'booleq)
(deftest (match _ liste) (match (_)            'a)             nil #'booleq)

;;; @  Mêmes tests que _ , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest (match @) (match @              'a)             nil #'booleq) ;; diff
(deftest (match @) (match @              '(a b))         t   #'booleq)
(deftest (match @) (match @              '())            t   #'booleq)

(deftest (match @ fin-de-liste) (match (a @)          '(a b))         nil #'booleq) ;; diff
(deftest (match @ fin-de-liste) (match (a @)          '(a (1 2 3)))   t   #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(a (b c)))     t   #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(a (b)))       t   #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(a ()))        t   #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(a (b . c)))   nil #'booleq) ;; diff
(deftest (match @ fin-de-liste) (match (a @)          '(x b))         nil #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(x (b)))       nil #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(a))           nil #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '(a b c))       nil #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          '())            nil #'booleq)
(deftest (match @ fin-de-liste) (match (a @)          'a)             nil #'booleq)

(deftest (match @ milieu-de-liste) (match (a @ c)        '(a b c))       nil #'booleq) ;; diff
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a (1 2 3) c)) t   #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a (b c) c))   t   #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a (b) c))     t   #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a () c))      t   #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a (b . c) c)) nil #'booleq) ;; diff
(deftest (match @ milieu-de-liste) (match (a @ c)        '(x (b) c))     nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(x b c))       nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(x (b) c))     nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a c))         nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a))           nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a b c d))     nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '())            nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        'a)             nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a b b c))     nil #'booleq)
(deftest (match @ milieu-de-liste) (match (a @ c)        '(a b x c))     nil #'booleq)

(deftest (match @ fin-de-cons) (match (a . @)        '(a b))         t   #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        '(a b c))       t   #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        '(a))           t   #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        '(a b . c))     nil #'booleq) ;; diff
(deftest (match @ fin-de-cons) (match (a . @)        '(a . b))       nil #'booleq) ;; diff
(deftest (match @ fin-de-cons) (match (a . @)        '(a . (b c)))   t   #'booleq) ;; equivalent à '(a b c)
(deftest (match @ fin-de-cons) (match (a . @)        '(a . ()))      t   #'booleq) ;; equivalent à '(a)
(deftest (match @ fin-de-cons) (match (a . @)        '(x . b))       nil #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        '(x b c))       nil #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        '(x))           nil #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        '())            nil #'booleq)
(deftest (match @ fin-de-cons) (match (a . @)        'a)             nil #'booleq)

(deftest (match @ cons) (match (a @ . @)      '(a b . c))     nil #'booleq) ;; diff
(deftest (match @ cons) (match (a @ . @)      '(a () . ()))   t   #'booleq)
(deftest (match @ cons) (match (a @ . @)      '(a (1) . (2))) t   #'booleq)
(deftest (match @ cons) (match (a @ . @)      '(a 1 . (2)))   nil #'booleq) ;; diff  ;; equivalent à '(a 1 2)
(deftest (match @ cons) (match (a @ . @)      '(a (1) . 2))   nil #'booleq) ;; diff
(deftest (match @ cons) (match (a @ . @)      '(a))           nil #'booleq)
(deftest (match @ cons) (match (a @ . @)      'a)             nil #'booleq)

(deftest (match @ liste) (match (@)            '(a))           nil #'booleq) ;; diff
(deftest (match @ liste) (match (@)            '(()))          t   #'booleq)
(deftest (match @ liste) (match (@)            '((a b)))       t   #'booleq)
(deftest (match @ liste) (match (@)            '(a b))         nil #'booleq)
(deftest (match @ liste) (match (@)            '())            nil #'booleq)
(deftest (match @ liste) (match (@)            'a)             nil #'booleq)

;;; @.  Mêmes tests que @ , on indique les différences avec ";; diff avec @" à la fin de la ligne.

(deftest (match @.) (match @.              'a)             nil #'booleq)
(deftest (match @.) (match @.              '(a b))         t   #'booleq)
(deftest (match @.) (match @.              '())            nil #'booleq) ;; diff avec @

(deftest (match @. fin-de-liste) (match (a @.)          '(a b))         nil #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(a (1 2 3)))   t   #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(a (b c)))     t   #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(a (b)))       t   #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(a ()))        nil #'booleq) ;; diff avec @
(deftest (match @. fin-de-liste) (match (a @.)          '(a (b . c)))   t   #'booleq) ;; diff avec @
(deftest (match @. fin-de-liste) (match (a @.)          '(x b))         nil #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(x (b)))       nil #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(a))           nil #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '(a b c))       nil #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          '())            nil #'booleq)
(deftest (match @. fin-de-liste) (match (a @.)          'a)             nil #'booleq)

(deftest (match @. milieu-de-liste) (match (a @. c)        '(a b c))       nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a (1 2 3) c)) t   #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a (b c) c))   t   #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a (b) c))     t   #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a () c))      nil #'booleq) ;; diff avec @
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a (b . c) c)) t   #'booleq) ;; diff avec @
(deftest (match @. milieu-de-liste) (match (a @. c)        '(x (b) c))     nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(x b c))       nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(x (b) c))     nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a c))         nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a))           nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a b c d))     nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '())            nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        'a)             nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a b b c))     nil #'booleq)
(deftest (match @. milieu-de-liste) (match (a @. c)        '(a b x c))     nil #'booleq)

(deftest (match @. fin-de-cons) (match (a . @.)        '(a b))         t   #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        '(a b c))       t   #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        '(a))           nil #'booleq) ;; diff avec @
(deftest (match @. fin-de-cons) (match (a . @.)        '(a b . c))     t   #'booleq) ;; diff avec @
(deftest (match @. fin-de-cons) (match (a . @.)        '(a . b))       nil #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        '(a . (b c)))   t   #'booleq) ;; equivalent à '(a b c)
(deftest (match @. fin-de-cons) (match (a . @.)        '(a . ()))      nil #'booleq) ;; diff avec @  ;; equivalent à '(a)
(deftest (match @. fin-de-cons) (match (a . @.)        '(x . b))       nil #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        '(x b c))       nil #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        '(x))           nil #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        '())            nil #'booleq)
(deftest (match @. fin-de-cons) (match (a . @.)        'a)             nil #'booleq)

(deftest (match @. cons) (match (a @. . @.)      '(a b . c))     nil #'booleq)
(deftest (match @. cons) (match (a @. . @.)      '(a () . ()))   nil #'booleq) ;; diff avec @
(deftest (match @. cons) (match (a @. . @.)      '(a (1) . (2))) t   #'booleq)
(deftest (match @. cons) (match (a @. . @.)      '(a 1 . (2)))   nil #'booleq)
(deftest (match @. cons) (match (a @. . @.)      '(a (1) . 2))   nil #'booleq)
(deftest (match @. cons) (match (a @. . @.)      '(a))           nil #'booleq)
(deftest (match @. cons) (match (a @. . @.)      'a)             nil #'booleq)

(deftest (match @. liste) (match (@.)            '(a))           nil #'booleq)
(deftest (match @. liste) (match (@.)            '(()))          nil #'booleq) ;; diff avec @
(deftest (match @. liste) (match (@.)            '((a b)))       t   #'booleq)
(deftest (match @. liste) (match (@.)            '(a b))         nil #'booleq)
(deftest (match @. liste) (match (@.)            '())            nil #'booleq)
(deftest (match @. liste) (match (@.)            'a)             nil #'booleq)

;;; $  Mêmes tests que _ , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest (match $) (match $              'a)             t   #'booleq)
(deftest (match $) (match $              '(a b))         nil #'booleq) ;; diff
(deftest (match $) (match $              '())            nil #'booleq) ;; diff

(deftest (match $ fin-de-liste) (match (a $)          '(a b))         t   #'booleq)
(deftest (match $ fin-de-liste) (match (a $)          '(a (1 2 3)))   nil #'booleq) ;; diff
(deftest (match $ fin-de-liste) (match (a $)          '(a (b c)))     nil #'booleq) ;; diff
(deftest (match $ fin-de-liste) (match (a $)          '(a (b)))       nil #'booleq) ;; diff
(deftest (match $ fin-de-liste) (match (a $)          '(a ()))        nil #'booleq) ;; diff
(deftest (match $ fin-de-liste) (match (a $)          '(a (b . c)))   nil #'booleq) ;; diff
(deftest (match $ fin-de-liste) (match (a $)          '(x b))         nil #'booleq)
(deftest (match $ fin-de-liste) (match (a $)          '(x (b)))       nil #'booleq)
(deftest (match $ fin-de-liste) (match (a $)          '(a))           nil #'booleq)
(deftest (match $ fin-de-liste) (match (a $)          '(a b c))       nil #'booleq)
(deftest (match $ fin-de-liste) (match (a $)          '())            nil #'booleq)
(deftest (match $ fin-de-liste) (match (a $)          'a)             nil #'booleq)

(deftest (match $ milieu-de-liste) (match (a $ c)        '(a b c))       t   #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a (1 2 3) c)) nil #'booleq) ;; diff
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a (b c) c))   nil #'booleq) ;; diff
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a (b) c))     nil #'booleq) ;; diff
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a () c))      nil #'booleq) ;; diff
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a (b . c) c)) nil #'booleq) ;; diff
(deftest (match $ milieu-de-liste) (match (a $ c)        '(x (b) c))     nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(x b c))       nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(x (b) c))     nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a c))         nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a))           nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a b c d))     nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '())            nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        'a)             nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a b b c))     nil #'booleq)
(deftest (match $ milieu-de-liste) (match (a $ c)        '(a b x c))     nil #'booleq)

(deftest (match $ fin-de-cons) (match (a . $)        '(a b))         nil #'booleq) ;; diff
(deftest (match $ fin-de-cons) (match (a . $)        '(a b c))       nil #'booleq) ;; diff
(deftest (match $ fin-de-cons) (match (a . $)        '(a))           nil #'booleq) ;; diff
(deftest (match $ fin-de-cons) (match (a . $)        '(a b . c))     nil #'booleq) ;; diff
(deftest (match $ fin-de-cons) (match (a . $)        '(a . b))       t   #'booleq)
(deftest (match $ fin-de-cons) (match (a . $)        '(a . (b c)))   nil #'booleq) ;; diff  ;; equivalent à '(a b c)
(deftest (match $ fin-de-cons) (match (a . $)        '(a . ()))      nil #'booleq) ;; diff  ;; equivalent à '(a)
(deftest (match $ fin-de-cons) (match (a . $)        '(x . b))       nil #'booleq)
(deftest (match $ fin-de-cons) (match (a . $)        '(x b c))       nil #'booleq)
(deftest (match $ fin-de-cons) (match (a . $)        '(x))           nil #'booleq)
(deftest (match $ fin-de-cons) (match (a . $)        '())            nil #'booleq)
(deftest (match $ fin-de-cons) (match (a . $)        'a)             nil #'booleq)

(deftest (match $ cons) (match (a $ . $)      '(a b . c))     t   #'booleq)
(deftest (match $ cons) (match (a $ . $)      '(a () . ()))   nil #'booleq) ;; diff
(deftest (match $ cons) (match (a $ . $)      '(a (1) . (2))) nil #'booleq) ;; diff
(deftest (match $ cons) (match (a $ . $)      '(a 1 . (2)))   nil #'booleq) ;; diff  ;; equivalent à '(a 1 2)
(deftest (match $ cons) (match (a $ . $)      '(a (1) . 2))   nil #'booleq) ;; diff
(deftest (match $ cons) (match (a $ . $)      '(a))           nil #'booleq)
(deftest (match $ cons) (match (a $ . $)      'a)             nil #'booleq)

(deftest (match $ liste) (match ($)            '(a))           t   #'booleq)
(deftest (match $ liste) (match ($)            '(()))          nil #'booleq) ;; diff
(deftest (match $ liste) (match ($)            '((a b)))       nil #'booleq) ;; diff
(deftest (match $ liste) (match ($)            '(a b))         nil #'booleq)
(deftest (match $ liste) (match ($)            '())            nil #'booleq)
(deftest (match $ liste) (match ($)            'a)             nil #'booleq)

;;; *

(deftest (match * symbole) (match (a *)             '(a a a a))                       t   #'booleq)
(deftest (match * symbole) (match (a *)             '(a))                             t   #'booleq)
(deftest (match * symbole) (match (a *)             '())                              t   #'booleq)
(deftest (match * symbole) (match (a *)             'a)                               nil #'booleq)
(deftest (match * symbole) (match (a *)             '(b b b b))                       nil #'booleq)
(deftest (match * symbole) (match (a *)             '(b))                             nil #'booleq)

(deftest (match * _) (match (a (_ *) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a ((1 2) 3 () (4 5)) c))        t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a ((1 2) (3) (4 5)) c))         t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a ((1 2) (3 . x) (4 5)) c))     t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a (x) c))                       t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a () c))                        t   #'booleq)
(deftest (match * _) (match (a (_ *) c)       '(a x c))                         nil #'booleq)

(deftest (match * @) (match (a (@ *) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a ((1 2) (3) (4 5)) c))         t   #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a (x) c))                       nil #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a () c))                        t   #'booleq)
(deftest (match * @) (match (a (@ *) c)       '(a x c))                         nil #'booleq)

(deftest (match * @.) (match (a (@. *) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a ((1 2) (3) (4 5)) c))         t   #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a ((1 2) (3 . x) (4 5)) c))     t   #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a (x) c))                       nil #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a () c))                        t   #'booleq)
(deftest (match * @.) (match (a (@. *) c)       '(a x c))                         nil #'booleq)

(deftest (match * $) (match (a ($ *) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a ((1 2) (3) (4 5)) c))         nil #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a ((1 2)) c))                   nil #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a (x) c))                       t   #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a () c))                        t   #'booleq)
(deftest (match * $) (match (a ($ *) c)       '(a x c))                         nil #'booleq)

(deftest (match * dans-sous-motif) (match (a (1 $ * 2)* c)  '(a (1 2) (1 b 2) (1 1 2) c))     t   #'booleq)
(deftest (match * dans-sous-motif) (match (a (1 $ * 2)* c)  '(a (1 x 2) (1 b 2) (1 1 1 2) c)) t   #'booleq)
(deftest (match * dans-sous-motif) (match (a (1 $ * 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2) c))   t   #'booleq)
(deftest (match * dans-sous-motif) (match (a (1 $ * 2)* c)  '(a (1 2) (1 b)   (1 1 1 2) c))   nil #'booleq)
(deftest (match * dans-sous-motif) (match (a (1 $ * 2)* c)  '(a (1 2)  1 b 2  (1 1 1 2) c))   nil #'booleq)
(deftest (match * dans-sous-motif) (match (a (1 $ * 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2)))     nil #'booleq)

(deftest (match * sous-motif) (match (a (1 $ * 2)* c)  '(a (1 b 2) (1 b 2) c))           t   #'booleq)
(deftest (match * sous-motif) (match (a (1 $ * 2)* c)  '(a (1 b 2) c))                   t   #'booleq)
(deftest (match * sous-motif) (match (a (1 $ * 2)* c)  '(a c))                           t   #'booleq)

;;; + Mêmes tests que * , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest (match + symbole) (match (a +)             '(a a a a))                       t   #'booleq)
(deftest (match + symbole) (match (a +)             '(a))                             t   #'booleq)
(deftest (match + symbole) (match (a +)             '())                              nil #'booleq) ;; diff
(deftest (match + symbole) (match (a +)             'a)                               nil #'booleq)
(deftest (match + symbole) (match (a +)             '(b b b b))                       nil #'booleq)
(deftest (match + symbole) (match (a +)             '(b))                             nil #'booleq)

(deftest (match + _) (match (a (_ +) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a ((1 2) 3 () (4 5)) c))        t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a ((1 2) (3) (4 5)) c))         t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a ((1 2) (3 . x) (4 5)) c))     t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a (x) c))                       t   #'booleq)
(deftest (match + _) (match (a (_ +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest (match + _) (match (a (_ +) c)       '(a x c))                         nil #'booleq)

(deftest (match + @) (match (a (@ +) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a ((1 2) (3) (4 5)) c))         t   #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a (x) c))                       nil #'booleq)
(deftest (match + @) (match (a (@ +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest (match + @) (match (a (@ +) c)       '(a x c))                         nil #'booleq)

(deftest (match + @.) (match (a (@. +) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a ((1 2) (3) (4 5)) c))         t   #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a ((1 2) (3 . x) (4 5)) c))     t   #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a (x) c))                       nil #'booleq)
(deftest (match + @.) (match (a (@. +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest (match + @.) (match (a (@. +) c)       '(a x c))                         nil #'booleq)

(deftest (match + $) (match (a ($ +) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a ((1 2) (3) (4 5)) c))         nil #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a ((1 2)) c))                   nil #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a (x) c))                       t   #'booleq)
(deftest (match + $) (match (a ($ +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest (match + $) (match (a ($ +) c)       '(a x c))                         nil #'booleq)

(deftest (match + dans-sous-motif) (match (a (1 $ + 2)* c)  '(a (1 2) (1 b 2) (1 1 2) c))     nil #'booleq) ;; diff
(deftest (match + dans-sous-motif) (match (a (1 $ + 2)* c)  '(a (1 x 2) (1 b 2) (1 1 1 2) c)) t   #'booleq)
(deftest (match + dans-sous-motif) (match (a (1 $ + 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2) c))   nil #'booleq) ;; diff
(deftest (match + dans-sous-motif) (match (a (1 $ + 2)* c)  '(a (1 2) (1 b)   (1 1 1 2) c))   nil #'booleq)
(deftest (match + dans-sous-motif) (match (a (1 $ + 2)* c)  '(a (1 2)  1 b 2  (1 1 1 2) c))   nil #'booleq)
(deftest (match + dans-sous-motif) (match (a (1 $ + 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2)))     nil #'booleq)

(deftest (match + sous-motif) (match (a (1 $ + 2)+ c)  '(a (1 b 2) (1 b 2) c))           t   #'booleq)
(deftest (match + sous-motif) (match (a (1 $ + 2)+ c)  '(a (1 b 2) c))                   t   #'booleq)
(deftest (match + sous-motif) (match (a (1 $ + 2)+ c)  '(a c))                           nil #'booleq) ;; diff

;;; ? Mêmes tests que * , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest (match ? symbole) (match (a ?)             '(a a a a))                       nil #'booleq) ;; diff
(deftest (match ? symbole) (match (a ?)             '(a))                             t   #'booleq)
(deftest (match ? symbole) (match (a ?)             '())                              t   #'booleq)
(deftest (match ? symbole) (match (a ?)             'a)                               nil #'booleq)
(deftest (match ? symbole) (match (a ?)             '(b b b b))                       nil #'booleq)
(deftest (match ? symbole) (match (a ?)             '(b))                             nil #'booleq)

(deftest (match ? _) (match (a (_ ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq) ;; diff
(deftest (match ? _) (match (a (_ ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq) ;; diff
(deftest (match ? _) (match (a (_ ?) c)       '(a ((1 2) (3) (4 5)) c))         nil #'booleq) ;; diff
(deftest (match ? _) (match (a (_ ?) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq) ;; diff
(deftest (match ? _) (match (a (_ ?) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match ? _) (match (a (_ ?) c)       '(a (1 2 3) c))                   nil #'booleq) ;; diff
(deftest (match ? _) (match (a (_ ?) c)       '(a (x) c))                       t   #'booleq)
(deftest (match ? _) (match (a (_ ?) c)       '(a () c))                        t   #'booleq)
(deftest (match ? _) (match (a (_ ?) c)       '(a x c))                         nil #'booleq)

(deftest (match ? @) (match (a (@ ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq) ;; diff
(deftest (match ? @) (match (a (@ ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match ? @) (match (a (@ ?) c)       '(a ((1 2) (3) (4 5)) c))         nil #'booleq) ;; diff
(deftest (match ? @) (match (a (@ ?) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq)
(deftest (match ? @) (match (a (@ ?) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match ? @) (match (a (@ ?) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest (match ? @) (match (a (@ ?) c)       '(a (x) c))                       nil #'booleq)
(deftest (match ? @) (match (a (@ ?) c)       '(a () c))                        t   #'booleq)
(deftest (match ? @) (match (a (@ ?) c)       '(a x c))                         nil #'booleq)

(deftest (match ? @.) (match (a (@. ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest (match ? @.) (match (a (@. ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq) 
(deftest (match ? @.) (match (a (@. ?) c)       '(a ((1 2) (3) (4 5)) c))         nil #'booleq) ;; diff
(deftest (match ? @.) (match (a (@. ?) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq) ;; diff
(deftest (match ? @.) (match (a (@. ?) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest (match ? @.) (match (a (@. ?) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest (match ? @.) (match (a (@. ?) c)       '(a (x) c))                       nil #'booleq)
(deftest (match ? @.) (match (a (@. ?) c)       '(a () c))                        t   #'booleq)
(deftest (match ? @.) (match (a (@. ?) c)       '(a x c))                         nil #'booleq)

(deftest (match ? $) (match (a ($ ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a ((1 2) (3) (4 5)) c))         nil #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a ((1 2) (3 . x) (4 5)) c))     nil #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a ((1 2)) c))                   nil #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a (1 2 3) c))                   nil #'booleq) ;; diff
(deftest (match ? $) (match (a ($ ?) c)       '(a (x) c))                       t   #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a () c))                        t   #'booleq)
(deftest (match ? $) (match (a ($ ?) c)       '(a x c))                         nil #'booleq)

(deftest (match ? dans-sous-motif) (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b 2) (1 1 2) c))     t   #'booleq)
(deftest (match ? dans-sous-motif) (match (a (1 $ ? 2)* c)  '(a (1 x 2) (1 b 2) (1 1 1 2) c)) nil #'booleq) ;; diff
(deftest (match ? dans-sous-motif) (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2) c))   nil #'booleq) ;; diff
(deftest (match ? dans-sous-motif) (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b)   (1 1 1 2) c))   nil #'booleq)
(deftest (match ? dans-sous-motif) (match (a (1 $ ? 2)* c)  '(a (1 2)  1 b 2  (1 1 1 2) c))   nil #'booleq)
(deftest (match ? dans-sous-motif) (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2)))     nil #'booleq)

(deftest (match ? sous-motif) (match (a (1 $ ? 2)? c)  '(a (1 b 2) (1 b 2) c))           nil #'booleq) ;; diff
(deftest (match ? sous-motif) (match (a (1 $ ? 2)? c)  '(a (1 b 2) c))                   t   #'booleq)
(deftest (match ? sous-motif) (match (a (1 $ ? 2)? c)  '(a c))                           t   #'booleq)

;;; (? tests...)

;; TODO : not, nand et nor + notation infixe (ou peut-être pas).

;; Identity par défaut.
(deftest (match predicats zéro) (match (?)                      t)  t   #'booleq)
(deftest (match predicats zéro) (match (?)                    nil)  nil #'booleq)

(deftest (match predicats un) (match (? numberp)                1)  t   #'booleq)
(deftest (match predicats un) (match (? numberp)               'a)  nil #'booleq)

(deftest (match predicats un) (match (? plusp)                  1)  t   #'booleq)
(deftest (match predicats un) (match (? plusp)                 -1)  nil #'booleq)

;; and par défaut
(deftest (match predicats deux and-implicite) (match (? integerp plusp)         1)  t   #'booleq)
(deftest (match predicats deux and-implicite) (match (? integerp plusp)       0.1)  nil #'booleq)
(deftest (match predicats deux and-implicite) (match (? integerp plusp)        -1)  nil #'booleq)
(deftest (match predicats deux and-implicite) (match (? integerp plusp)      -0.1)  nil #'booleq)

(deftest (match predicats deux and) (match (? and integerp plusp)     1)  t   #'booleq)
(deftest (match predicats deux and) (match (? and integerp plusp)   0.1)  nil #'booleq)
(deftest (match predicats deux and) (match (? and integerp plusp)    -1)  nil #'booleq)
(deftest (match predicats deux and) (match (? and integerp plusp)  -0.1)  nil #'booleq)

(deftest (match predicats deux or) (match (? or integerp plusp)      1)  t   #'booleq)
(deftest (match predicats deux or) (match (? or integerp plusp)    0.1)  t   #'booleq)
(deftest (match predicats deux or) (match (? or integerp plusp)     -1)  t   #'booleq)
(deftest (match predicats deux or) (match (? or integerp plusp)   -0.1)  nil #'booleq)

(deftest (match predicats code) (match (? numberp (> x 3))        5)  t   #'booleq)
(deftest (match predicats code) (match (? numberp (> x 3))        2)  nil #'booleq)

(deftest (match predicats lambda) (match (? numberp (lambda (y) (> y 3))) 5)  t   #'booleq)
(deftest (match predicats lambda) (match (? numberp (lambda (y) (> y 3))) 3)  nil #'booleq)

;; Tests de preprocess-capture

(deftest (match preprocess-capture)
    (pattern-match-preprocess (:x . nil))
  '(x nil nil nil nil))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (:x . a))
  '(x nil a nil nil))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (:x . (a)))
  '(nil nil (x nil a nil nil) nil (nil nil nil nil nil)))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (:x a))
  '(nil nil (x nil a nil nil) nil (nil nil nil nil nil)))

    
(deftest (match preprocess-capture)
    (pattern-match-preprocess (:x a *))
  '(nil nil (x nil a nil nil) * (nil nil nil nil nil)))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (a *))
  '(nil nil (nil nil a nil nil) * (nil nil nil nil nil)))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (:x (a) *))
  '(nil nil (x nil (nil nil a nil nil) nil (nil nil nil nil nil)) * (nil nil nil nil nil)))

(deftest (match preprocess-capture)
    (pattern-match-preprocess nil)
  '(nil nil nil nil nil))

(deftest (match preprocess-capture)
    (pattern-match-preprocess a)
  '(nil nil a nil nil))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (a))
  '(nil nil (nil nil a nil nil) nil (nil nil nil nil nil)))

(deftest (match preprocess-capture)
    (pattern-match-preprocess (a * b))
  '(nil nil (nil nil a nil nil) * (nil nil (nil nil b nil nil) nil (nil nil nil nil nil))))

(deftest (match preprocess-capture)
    (pattern-match-preprocess ((a)* b))
  '(nil nil (nil nil (nil nil a nil nil) nil (nil nil nil nil nil)) * (nil nil (nil nil b nil nil) nil (nil nil nil nil nil))))

;;;; Tests de capture (variables)

(deftest (match append-captures)
    (append-captures '((x . (foo bar)) (y . foo) (z . bar))
                     '(((x . nil) (y . nil) (z . nil)) (e . x)))
  '(((x . ((foo bar))) (y . (foo)) (z . (bar))) (e . x)))

(deftest (match append-captures)
    (append-captures '((x . (1 2)) (y . 1) (z . 2))
                     '(((x . ((foo bar))) (y . (foo)) (z . (bar))) (e . x)))
  '(((x . ((1 2) (foo bar))) (y . (1 foo)) (z . (2 bar))) (e . x)))

(deftest (match make-empty-matches)
    (make-empty-matches (pattern-match-preprocess (:y _ :z _)))
  '((y . nil) (z . nil)))

(deftest (match make-empty-matches)
    (make-empty-matches (pattern-match-preprocess ((:y _)* :z _)))
  '((y . nil) (z . nil)))

(deftest (match capture misc)
    (match (:x ((:y _)* :z _)*) '(((foo) (bar) baz) ((1) 2) ((a) (b) (c) d)))
  '((x . (((foo) (bar) baz) ((1) 2) ((a) (b) (c) d)))
    (y . ((foo bar) (1) (a b c)))
    (z . (baz 2 d))))

(deftest (match capture misc)
    (match (:x ((:y _)* :z _)*) '(((foo) (bar) baz) (2) ((a) (b) (c) d)))
  '((x . (((foo) (bar) baz) (2) ((a) (b) (c) d)))
    (y . ((foo bar) () (a b c)))
    (z . (baz 2 d))))

(deftest (match capture misc)
    (match (:x ((:y _)* :z _)*) '())
  '((x . ())
    (y . ())
    (z . ())))

(deftest (match capture keyword-for-single-pattern) (match (:x . _) '(foo bar baz) x) '(foo bar baz))
(deftest (match capture keyword-for-single-pattern) (match :x _ '(foo bar baz) x) '(foo bar baz))

(deftest (match capture litteral-keyword) (match :x :x 'foo x) nil)
(deftest (match capture litteral-keyword) (match :x :x :x x) :x)
(deftest (match capture litteral-keyword) (match (:x :x) '(foo) x) nil)
(deftest (match capture litteral-keyword) (match (:x :x) '(:x) x) :x)

(deftest (match capture last-cons) (match (foo :x . _) '(foo bar baz) x) '(bar baz))
(deftest (match capture last-cons) (match (:x . _) '(foo bar baz) x) '(foo bar baz))

(deftest (match capture simple) (match :x $ 'foo x) 'foo)
(deftest (match capture simple) (match :x @ 'foo x) nil)

(deftest (match capture simple)
    (match (:x _)
           '(foo)
           x)
  'foo)

(deftest (match capture multi ext 1)
    (match (:x _ *)
           '(foo bar baz)
           x)
  '(foo bar baz))

(deftest (match capture multi int 1)
    (match ((:x _) *)
           '((foo) (bar) (baz))
           x)
  '(foo bar baz))

(deftest (match capture multi ext 2)
    (match ((:x _ *) *)
           '((foo bar baz) ;; expr de ext 1
             ()
             (quux))
           x)
  '((foo bar baz) ;; résultat de ext 1
    ()
    (quux)))

(deftest (match capture multi int 2)
    (match (((:x _) *) *)
           '(((foo) (bar) (baz)) ;; expr de int 1
             ()
             ((quux)))
           x)
  '((foo bar baz) ;; résultat de int 1
    ()
    (quux)))

(deftest (match capture multi ext 3)
    (match (((:x _ *) *) *)
           '(((foo bar baz) () (quux)) ;; expr de ext 2
             ()
             ((1 2) (3)))
           x)
  '(((foo bar baz) () (quux)) ;; résultat de ext 2
    ()
    ((1 2) (3))))

(deftest (match capture multi int 3)
    (match ((((:x _) *) *) *)
           '((((foo) (bar) (baz)) () ((quux))) ;; expr de int 2
             ()
             (((1) (2)) ((3))))
           x)
  '(((foo bar baz) () (quux)) ;; résultat de int 2
    ()
    ((1 2) (3))))

(deftest (match capture labels)
    (match (labels :declarations ((:name $ :params (:param $*) :fbody _*) *) :body _*)
           '(labels ((foo (x y) (list x y))
                     (bar (z w) (print z) (print w))
                     (quux ()))
             (foo 1 2)
             (bar 3 4))
           param)
  '((x y) (z w) ()))

;; Extrait une liste associative nom . lambda correspondant aux déclarations,
;; en rajoutant labels- devant chaque nom de paramètre (juste pour le fun).
(deftest (match capture labels)
    (match (labels :declarations ((:name $ :params (:param $*) :fbody _*) *) :body _*)
           '(labels ((foo (x y) (list x y))
                     (bar (z w) (print z) (print w))
                     (quux ()))
             (foo 1 2)
             (bar 3 4))
           (mapcar (lambda (name params fbody)
                     `(,name . (lambda ,(mapcar (lambda (param)
                                                  (intern (format nil "LABELS-~w" param)))
                                                params)
                                 ,@fbody)))
                   name params fbody))
  '((foo . (lambda (labels-x labels-y) (list x y)))
    (bar . (lambda (labels-z labels-w) (print z) (print w)))
    (quux . (lambda ()))))

(deftest (match cond-match)
    (cond-match
     '(a b)
     ((:x $ $) (list 1 x))
     ((:y $ @) (list 2 y))
     ((:z _) (list 3 z)))
  '(1 a))

(deftest (match cond-match)
    (cond-match
     '(a (b))
     ((:x $ $) (list 1 x))
     ((:y $ @) (list 2 y))
     ((:z _) (list 3 z)))
  '(2 a))

(deftest (match cond-match)
    (cond-match
     '(x)
     ((:x $ $) (list 1 x))
     ((:y $ @) (list 2 y))
     ((:z _) (list 3 z)))
  '(3 x))


(defmatch test-match-foo)

(defmatch test-match-foo (:x $ $) (list 'x x))
(defmatch test-match-foo (:y $ @) (list 'y y))
(defmatch test-match-foo (:z _) (list 'z z))
(defmatch test-match-foo else (x) x 'i-m-else)

(deftest (match defmatch)
    (test-match-foo '(a b))
  '(x a))
  
(deftest (match defmatch)
    (test-match-foo '(a (b)))
  '(y a))

(deftest (match defmatch)
    (test-match-foo '((3)))
  '(z (3)))

(deftest (match defmatch)
    (test-match-foo 42)
  'i-m-else)

(defmatch test-match-bar)
(defmatch test-match-bar else 'i-m-else)
(deftest (match defmatch)
    (test-match-bar 42)
  'i-m-else)

(provide 'match)