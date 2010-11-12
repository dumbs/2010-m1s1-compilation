(load "util") ;; n-consp

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
                    (when (and (consp ?-clause) (member (car ?-clause) '(nil _ $ @ @.)))
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
                           (append match match-rest)))) ;; TODO : vérifier qu'on n'a pas besoin d'un make-empty-matches en cas de non-match de sous-trucs. (normalement non)
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
  (let* ((result-sym (make-symbol "result"))
         (pattern-sym (make-symbol "pattern"))
         (pattern-preproc (pattern-match-preprocess-capture
                           (pattern-match-preprocess-multi
                            pattern)))
         (capture-names (mapcar #'car (make-empty-matches pattern-preproc))))
    `(let* ((,pattern-sym (pattern-match-do-lambdas ,pattern-preproc))
            (,result-sym (pattern-match ,pattern-sym ,expr)))
       ;; Filtrage des captures nommées nil.
       (if ,result-sym
           ,@(if body
                 `((let ,(mapcar (lambda (x)
                                   `(,x (cdr (assoc ',x ,result-sym))))
                                 capture-names)
                     ;; "utilisation" des variables pour éviter les warning unused variable.
                     ,@capture-names
                     ,@body))
                 (if capture-names
                     `((remove nil ,result-sym :key #'car))
                     `(t)))
           ,else-clause))))

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

(load "test-unitaire")
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
