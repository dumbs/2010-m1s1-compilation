(progn
  (defun n-consp (n l)
    "Détermine s'il y a au moins n cellules dans la liste l."
    (if (<= n 0)
        t
        (and (consp l)
             (n-consp (- n 1) (cdr l)))))

  (defun reverse-alist (alist)
    (mapcar (lambda (x) (cons (car x) (reverse (cdr x))))
            alist))

  (defun group-1 (lst &optional result)
    "Groupe les éléments d'une lste en fonction de leur premier élément, et renvoie une lste associative"
    (if (endp lst)
        result
        (let ((association (assoc (caar lst) result)))
          (if association
              (push (cdar lst) (cdr association))
              (push (cons (caar lst) (list (cdar lst))) result))
          (group-1 (cdr lst) result))))

  (defun group (lst)
    (reverse-alist (group-1 lst)))

  (defmacro dolist* (spec &rest body)
    (let* ((vars (mapcar #'car spec))
           (listforms (mapcar #'cadr spec))
           (loopsym (make-symbol "loop"))
           (endsym (make-symbol "end"))
           (listsyms (mapcar (lambda (x) (cons x (make-symbol "list"))) vars)))
      `(let (,@(mapcar (lambda (var) `(,var nil)) vars)
             ,@(mapcar (lambda (ls val) `(,(cdr ls) ,val)) listsyms listforms))
         (tagbody
            ,loopsym
            ,@(mapcar (lambda (ls)
                        `(setq ,(car ls) (car ,(cdr ls))))
                      listsyms)
            ,@(mapcar (lambda (ls)
                        `(when (endp ,(cdr ls))
                           (go ,endsym)))
                      listsyms)
            (progn ,@body)
            ,@(mapcar (lambda (ls)
                        `(setq ,(cdr ls) (cdr ,(cdr ls))))
                      listsyms)
            (go ,loopsym)
            ,endsym))))


























  (declaim (ftype function assembly-place-p))           ;; définie dans compilation.lisp
  (declaim (ftype function immutable-assembly-place-p)) ;; définie dans compilation.lisp
  (declaim (ftype function mutable-assembly-place-p))   ;; définie dans compilation.lisp

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
                      (when (and (consp ?-clause) (member (car ?-clause) '(nil _ $ $$ $k $n $ap $iap $map $& @ @.)))
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
                ;; $ap
                ((eq '$ap pattern)
                 (when (assembly-place-p expr)
                   (acons-capture capture-name expr nil)))
                ;; $iap
                ((eq '$iap pattern)
                 (when (immutable-assembly-place-p expr)
                   (acons-capture capture-name expr nil)))
                ;; $ap
                ((eq '$map pattern)
                 (when (mutable-assembly-place-p expr)
                   (acons-capture capture-name expr nil)))
                ;; $n
                ((eq '$n pattern)
                 (when (numberp expr)
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



















































  (defmacro etat-local (etat)
    `(car ,etat))

  (defmacro etat-global (etat)
    `(cadr ,etat))

  (defmacro etat-special (etat)
    ;; Variables spéciales et constantes. (ou devrait-on mettre les constantes dans etat-global ?)
    ;; Note sur les constantes : On ne protège pas contre la modification de parties d'une constante non atomique (mais clisp non plus, donc ça va).
    `(caddr ,etat))

  (defun assoc-etat (var type etat)
    (let ((search (cons var type)))
      (or (assoc search (etat-special etat) :test #'equal)
          (assoc search (etat-local   etat) :test #'equal)
          (assoc search (etat-global  etat) :test #'equal))))

  (defun assoc-special (var type etat)
    (assoc (cons var type) (etat-special etat) :test #'equal))

  (defun replace-local (etat new-etat-local)
    (cons new-etat-local (cdr etat)))
  
  (defun push-local (etat var type value)
    (when (and (eq type 'variable) (assoc-etat var 'constant etat))
      (error "mini-meval : Can't bind ~w : it is a constant." var))
    (replace-local etat (acons (cons var type) value (etat-local etat))))

  (defun push-local-or-special (etat var type value immediate)
    (let ((association (assoc-special var type etat))
          (new-etat nil))
      (if association
          (progn
            (setq new-etat (push-local etat var 'special-bakcup (cons association (cdr association))))
            (if immediate
                (progn (setf (cdr association) value)
                       new-etat)
                (push-local new-etat var 'special-future-phantom (cons association value))))
          (push-local etat var 'variable value))))

  (defun affect-future-specials (new-etat etat)
    (setq new-etat (etat-local new-etat))
    (setq etat (etat-local etat))
    (tagbody
     loop
       (when (eq new-etat etat) (go fin))
       (when (eq (cdaar new-etat) 'special-future-phantom)
         (setf (cdr (cadar new-etat)) (cddar new-etat)))
       (setq new-etat (cdr new-etat))
       (go loop)
     fin))

  (defun pop-special-backups (new-etat etat)
    (setq new-etat (etat-local new-etat))
    (setq etat (etat-local etat))
    (tagbody
     loop
       (when (eq new-etat etat) (go fin))
       (when (eq (cdaar new-etat) 'special-bakcup)
         (setf (cdr (cadar new-etat)) (cddar new-etat)))
       (setq new-etat (cdr new-etat))
       (go loop)
     fin))

  (defun push-global! (etat name type value)
    (setf (etat-global etat) (acons (cons name type) value (etat-global etat)))
    etat)

  (defun push-special! (etat name type value)
    (setf (etat-special etat) (acons (cons name type) value (etat-special etat)))
    etat)

  (defun reduce-on-local-1 (new-etat-local callback lists)
    (let ((res nil))
      (tagbody
       loop
         (when (member nil lists) (go fin))
         (setq res (apply callback new-etat-local (mapcar #'car lists)))
         (setq new-etat-local (acons (cons (car res) (cadr res))
                                     (caddr res)
                                     new-etat-local))
         (setq lists (mapcar #'cdr lists))
         (go loop)
       fin)
      new-etat-local))

  (defun reduce-on-local (etat callback &rest lists)
    (if (null lists)
        etat
        (replace-local etat (reduce-on-local-1 (etat-local etat) callback lists))))

  ;; DONE
  ;; - loop
  ;; - dolist / dotimes
  ;; - match-automaton(tagbody+block)

  ;; HALF-DONE (TODO)
  ;; - read
  ;; - warn
  ;; - ` (quasiquote)

  ;; TODO (dans mini-meval et/ou compilateur) :
  ;; - syntaxe courte du let
  ;; - declaim
  ;; - format
  ;; - setf (écrire la macro)
  ;; - fdefinition, funcctionp, …
  ;; - symbolp, keywordp, keywords non mutables + nil et t, intern, make-symbol
  ;; - car / cdr, replaca replacad, cons, list (fonction), listp, consp, atom, null (ou eq nil), …
  ;; - and / or (macros => if)
  ;; - &rest
  ;; - eq (vérifier qu'on préserve bien l'égalité de pointeurs là où il faut) / = / eql / equal / equalp
  ;; - load / open / close
  ;; - defvar [done mini-meval] (gestion correcte des variables spéciales)
  ;; - array support (array-total-size, row-major-aref, copy-seq)
  ;; - string support (char=, map, string (symbol => string), format, print)
  ;; - coder un reverse rapide.
  ;; - transformation de la récursion terminale.

  ;; - vérifier qu'on a pas transformé certaines fonctions en formes spéciales (il faut qu'on puisse toujours les récupérer avec #').
  ;; - sortir le defun du mini-meval ?

  ;; cell (un seul pointeur, transparent (y compris pour le type),
  ;; avec trois fonctions spéciales pour le get / set / tester le type),
  ;; sera utilisé pour les closures et les variables spéciales.

  ;; TODO : bug : pourquoi les keyword-to-symbol ??? on devrait garder le keyword tel quel.
  (defun slice-up-lambda-list (lambda-list)
    (match-automaton lambda-list fixed
                     (fixed    accept)
                     (fixed    optional &optional)
                     (fixed    rest     &rest)
                     (fixed    key      &key)
                     (fixed    aux      &aux)
                     (fixed    reject   $&)
                     (fixed    fixed    (:var . $$) var)
                     (optional accept)
                     (optional rest     &rest)
                     (optional key      &key)
                     (optional aux      &aux)
                     (optional reject   $&)
                     (optional optional (:var . $$) `(,var nil nil))
                     (optional optional (:var $$ :default _? :svar $$?) `(,var ,(car default) ,(car svar)))
                     (rest     reject   $&)
                     (rest     rest2    (:var . $$) var)
                     (rest2    accept)
                     (rest2    key      &key)
                     (rest2    aux      &aux)
                     (rest2    reject   $&)
                     (key      accept)
                     (key      other    &allow-other-keys)
                     (key      aux      &aux)
                     (key      reject   $&)
                     (key      key      (:keyword . $k) `(,(keyword-to-symbol keyword) ,(keyword-to-symbol keyword) nil nil)) ;; Not in the standard !
                     (key      key      (:var . $$) `(,var ,var nil nil))
                     (key      key      (:keyword $$ :default _? :svar $$?) `(,(keyword-to-symbol keyword) ,(keyword-to-symbol keyword) ,(car default) ,(car svar))) ;; Not in the standard !
                     (key      key      (:var $$ :default _? :svar $$?) `(,var ,var ,(car default) ,(car svar)))
                     (key      key      ((:keyword $k :var $$) :default _? :svar $$?) `(,(keyword-to-symbol keyword) ,var ,(car default) ,(car svar)))
                     (other    collect  t)
                     (other    accept)
                     (other    aux      &aux)
                     (other    reject   $&)
                     (aux      accept)
                     (aux      reject   $&)
                     (aux      aux      (:var . $$) `(,var nil))
                     (aux      aux      (:var $$ :default _?) `(,var ,(car default)))
                     (reject (error "slice-up-lambda-list : ~w ne peut être ici." last-element))))

  (declaim (ftype function mini-meval)) ;; récursion mutuelle mini-meval-get-params-from-real -> mini-meval-params / mini-meval
  (defun mini-meval-params (params etat fixed optional rest key other aux)
    (let ((new-etat etat)
          (value nil)
          (svar nil)
          (current-key)
          (search-key)
          (seen-keys))
      (tagbody
       fixed
         (when (endp fixed) (go end-fixed))
         (when (endp params) (error "mini-meval-params : not enough parameters !"))
         (setq new-etat (push-local-or-special new-etat (car fixed) 'variable (car params) nil))
         (setq params (cdr params))
         (setq fixed (cdr fixed))
         (go fixed)
       end-fixed
         (affect-future-specials new-etat etat)
       optional
         (when (endp optional) (go rest))
         (if (endp params)
             (setq value (mini-meval (cadar optional) new-etat)) ;; default value
             (setq value (car params)))
         (setq new-etat (push-local-or-special new-etat (caar optional) 'variable value t))
         (setq svar (caddar optional))
         (when svar
           (setq new-etat (push-local-or-special new-etat svar 'variable (endp params) t)))
         (setq params (cdr params))
         (setq optional (cdr optional))
         (go optional)
       rest
         (unless rest (go key))
         (setq new-etat (push-local new-etat (car rest) 'variable params))
       key
         (when (or (endp key) (endp params)) (go defaults-keys))
         (when (endp (cdr params)) (error "mini-meval-params : odd number of key parameters"))
         (setq search-key (keyword-to-symbol (car params)))
         (when (eq search-key (caar key))
           (setq current-key (car key))
           (push (car current-key) seen-keys)
           (setq key (cdr key))
           (go end-assoc-key-loop))
       assoc-key-loop
         (when (endp (cdr key))
           (go unknown-key))
         (when (eq search-key (caadr key))
           (setq current-key (cadr key))
           (push (car current-key) seen-keys)
           (setf (cdr key) (cddr key))
           (go end-assoc-key-loop))
         (go assoc-key-loop)
       end-assoc-key-loop
         (setq new-etat (push-local-or-special new-etat (second current-key) 'variable (second params) t))
         (setq svar (fourth current-key))
         (when svar
           (setq new-etat (push-local-or-special new-etat svar 'variable t t)))
         (go after-unknown-key)
       unknown-key
         (unless (or other (member search-key seen-keys))
           (error "mini-meval-params : invalid key : ~w" (car params)))
       after-unknown-key
         (setq key (cdr key))
         (setq params (cddr params))
       defaults-keys
         (dolist (k key)
           (setq new-etat (push-local-or-special new-etat (second k) 'variable (mini-meval (third k) new-etat) t))
           (setq svar (fourth k))
           (when svar
             (setq new-etat (push-local-or-special new-etat svar 'variable nil t))))
       aux
         (when (endp aux) (go fin))
         (setq new-etat (push-local-or-special new-etat (caar aux) 'variable (mini-meval (cadar aux) new-etat) t))
         (setq aux (cdr aux))
       fin)
      new-etat))

  (defun mini-meval-get-params-from-real (etat lambda-list effective-parameters)
    "Lambda-list doit être déjà sliced."
    (mini-meval-params effective-parameters etat
                       (cdr (assoc 'fixed    lambda-list)) ;; TODO : optimiser ça peut-être...
                       (cdr (assoc 'optional lambda-list))
                       (cdr (assoc 'rest     lambda-list))
                       (cdr (assoc 'key      lambda-list))
                       (cdr (assoc 'other    lambda-list))
                       (cdr (assoc 'aux      lambda-list))))

  (defun splice-up-tagbody-1 (remaining-body body result)
    (if (endp remaining-body)
        (acons nil body result)
        (if (or (symbolp (car remaining-body)) (numberp (car remaining-body)))
            (splice-up-tagbody-1 (cdr remaining-body)
                                 body
                                 (acons (car remaining-body) body result))
            (splice-up-tagbody-1 (cdr remaining-body)
                                 (cons (car remaining-body) body)
                                 result))))

  (defun splice-up-tagbody (body)
    (splice-up-tagbody-1 (reverse body) nil nil))

  (defun mini-meval-error (expr etat &rest message)
    (error "mini-meval (inner) : ~w~&expression = ~w~&etat-global = ~w~&etat-local = ~w~&etat-special = ~w"
           (apply #'format nil message)
           expr
           (etat-global etat)
           (etat-local etat)
           (etat-special etat)))

  (defun transform-quasiquote (expr)
    (cond
      ;; a
      ((atom expr)
       `',expr)
      ;; (a)
      ((atom (car expr))
       `(cons ',(car expr)
              ,(transform-quasiquote (cdr expr))))
      ;; (,a)
      ((eq 'unquote (caar expr))
       `(cons ,(cadar expr)
              ,(transform-quasiquote (cdr expr))))
      ;; (,@a)
      ((eq 'unquote-splice (caar expr))
       (if (endp (cdr expr))
           (cadar expr)
           `(append ,(cadar expr)
                    ,(transform-quasiquote (cdr expr)))))
      ;; ((a ...) ...)
      (T
       `(cons ,(transform-quasiquote (car expr))
              ,(transform-quasiquote (cdr expr))))))

  (defun mini-meval (expr &optional (etat (list nil nil nil)))
    (cond-match
     expr
     ((quasiquote :val . _)
      (mini-meval (transform-quasiquote val) etat))
     ((:name $$ :params _*)
      (let ((definition (assoc-etat name 'macro etat)))
        (if definition
            (mini-meval (apply (cdr definition) params) etat)
            (else))))
     ((eval-when :situations ($*) :body _*)
      (if (member :execute situations)
          (mini-meval `(progn ,@body) etat)
          nil))
     ((flet ((:name $ :lambda-list @ :fbody _*)*) :body _*)
      (mini-meval `(progn ,@body)
                  (reduce-on-local
                   etat
                   (lambda (ignore name lambda-list fbody) ignore
                           (list name 'function (mini-meval `(lambda ,lambda-list ,@fbody) etat)))
                   name lambda-list fbody)))
     ((labels ((:name $ :lambda-list @ :fbody _*)*) :body _*)
      (let* ((new-etat (reduce-on-local
                        etat
                        (lambda (ignore name) ignore (list name 'function nil))
                        name))
             (new-etat-local (etat-local new-etat)))
        (dolist* ((name name) (lambda-list lambda-list) (fbody fbody))
                 (setf (cdr (assoc `(,name . function) new-etat-local :test #'equal))
                       (mini-meval `(lambda ,lambda-list ,@fbody) new-etat)))
        (mini-meval `(progn ,@body) new-etat)))
     ((:type (? or (eq x 'let) (eq x 'let*)) :bindings (? and consp (find-if #'symbolp x)) :body . _)
      (mini-meval `(,type ,(mapcar (lambda (b) (if (consp b) b `(b nil))) bindings) ,@body)))
     ((let ((:name $ :value _)*) :body _*)
      (let ((new-etat etat)
            (res nil))
        (dolist* ((name name) (value value))
                 (setq new-etat (push-local-or-special new-etat name 'variable (mini-meval value etat) nil)))
        (affect-future-specials new-etat etat)
        (setq res (mini-meval `(progn ,@body) new-etat))
        (pop-special-backups new-etat etat)
        res))
     (((? (eq x 'let*)) ((:name $ :value _)*) :body _*)
      (let ((new-etat etat)
            (res nil))
        ;; pour chaque variable
        (dolist* ((name name) (value value))
                 (setq new-etat (push-local-or-special new-etat name 'variable (mini-meval value new-etat) t)))
        (setq res (mini-meval `(progn ,@body) new-etat))
        (pop-special-backups new-etat etat)
        res))
     ((macrolet ((:name $ :lambda-list @ :mbody _*)*) :body _*)
      (let ((new-etat
             (reduce-on-local
              etat
              (lambda (ignore name lambda-list mbody) ignore
                      ;; comme le flet sauf nil au lieu de new-etat-local
                      ;; CLTL 7.5 :
                      ;; The precise rule is that the macro-expansion functions defined
                      ;; by macrolet are defined in the global environment; lexically
                      ;; scoped entities that would ordinarily be lexically apparent
                      ;; are not visible within the expansion functions.
                      (list name 'macro
                            (mini-meval `(lambda ,lambda-list ,@mbody) (replace-local etat nil))))
              name lambda-list mbody))
            (get-etat (assoc-etat 'trapdoor 'squash-trapdoor etat)))
        (if (and get-etat (eq (car body) (cdr get-etat)))
            new-etat ;; Trapdoor pour récupérer l'etat avec les définitions du macrolet.
            (mini-meval `(progn ,@body) new-etat))))
     ((progn :body _*)
      (let ((res nil))
        (dolist (expr body res)
          (setq res (mini-meval expr etat)))))
     ((if :condition _ :si-vrai _ :si-faux _?)
      (if (mini-meval condition etat)
          (mini-meval si-vrai etat)
          (if si-faux
              (mini-meval (car si-faux) etat)
              nil)))
     ((lambda :lambda-list @ :body _*)
      (let ((sliced-lambda-list (slice-up-lambda-list lambda-list))
            (old-etat etat))
        (lambda (&rest effective-parameters)
          (let* ((new-etat (mini-meval-get-params-from-real old-etat sliced-lambda-list effective-parameters))
                 (res (mini-meval `(progn ,@body) new-etat)))
            (pop-special-backups new-etat etat)
            res))))
     ;; Lorsqu'une fonction "littérale" est présente dans le code, on la renvoie telle qu'elle.
     ((? functionp)
      expr)
     ((defun :name $ :lambda-list @ :body _*)
      (push-global! etat name 'function
                    (mini-meval `(lambda ,lambda-list ,@body) etat))
      name)
     ((defmacro :name $ :lambda-list @ :body _*)
      (push-global! etat name 'macro
                    (mini-meval `(lambda ,lambda-list ,@body) etat))
      name)
     ((defvar :name $ :value _)
      (when (assoc-etat name 'constant etat) (mini-meval-error expr etat "Can't bind ~w : it is a constant." name))
      (let ((definition (assoc-etat name 'variable etat)))
        ;; NOTE : if you do a "defvar" while in a "let" that binds the same variable, the result is gibberish and nonsensical.
        ;; But that case is fairly rare and not worth the effort and run-time cost.
        (push-special! etat name 'variable
                       (if definition
                           (cdr definition)
                           (mini-meval value etat))))
      name)
     ((setq :name $ :value _)
      (let ((definition (assoc-etat name 'variable etat))
            (real-value (mini-meval value etat))) ;; Faut-il vérifier que NAME n'est pas une constante *avant* de calculer la valeur ?
        (if definition
            (setf (cdr definition) real-value)
            (progn
              (when (assoc-etat name 'constant etat) (mini-meval-error expr etat "Can't set ~w : it is a constant." name))
              (push-global! etat name 'variable (mini-meval value etat))))
        real-value))
     ((declaim _*)
      nil)
     ((error :format _ :args _*)
      (error "mini-meval : fonction error appellée par le code, le message est :~&~w" (apply #'format nil format args)))
     ((warn :format _ :args _*)
      (warn "mini-meval : fonction warn appellée par le code, le message est :~&~w" (apply #'format nil format args)))
     ((go :target (? or symbolp numberp))
      (when (null target)
        (mini-meval-error expr etat "nil ne peut pas être une étiquette pour un go."))
      (let ((association (assoc `(,target . tagbody-tag) (etat-local etat) :test #'equal)))
        (if association
            (funcall (cdr association))
            (mini-meval-error expr etat "tentative de faire un go sur une étiquette qui n'existe pas ou plus : ~w.~&~w" target))))
     ((tagbody :body _*)
      (let ((spliced-body (splice-up-tagbody body))
            (next-tag nil)
            (new-etat nil))
        (tagbody
         init
           (setq new-etat
                 (reduce-on-local
                  etat
                  (lambda (ignore tag) ignore
                          (list (car tag) 'tagbody-tag
                                (lambda () (setq next-tag (car tag)) (go go-to-tag))))
                  spliced-body))
         go-to-tag
           (mini-meval `(progn ,@(cdr (assoc next-tag spliced-body)))
                       new-etat))))
     ((return-from :block-name $$ :value _)
      (let ((association (assoc `(,block-name . block-name) (etat-local etat) :test #'equal)))
        (if association
            (funcall (cdr association) value)
            (mini-meval-error expr etat "tentative de faire un return-from pour un bloc qui n'existe pas ou plus : ~w." block-name))))
     ((block :block-name $$ :body _*)
      (block block-catcher
        (mini-meval `(progn ,@body)
                    (push-local etat block-name 'block-name (lambda (x) (return-from block-catcher x))))))
     ((quote :val _)
      val)
     ((function :name $$)
      (let ((definition (assoc-etat name 'function etat)))
        (if definition
            (cdr definition)
            (mini-meval-error expr etat "Undefined function : ~w." name))))
     ((function :fun (lambda _ . _))
      (mini-meval fun etat))
     ((funcall :name _ :params _*)
      (apply (mini-meval name etat)
             (mapcar (lambda (x) (mini-meval x etat)) params)))
     ((apply :name _ :p1 _ :params _*)
      (let ((fun (mini-meval name etat))
            (args (mapcar (lambda (x) (mini-meval x etat)) (cons p1 params))))
        (apply fun (append (butlast args) (car (last args))))))
     ((:lambda (lambda @ _*) :params _*)
      (apply (mini-meval lambda etat) (mapcar (lambda (x) (mini-meval x etat)) params)))
     (((function :fun (lambda _ . _)) :params . _)
      (mini-meval `(,fun ,@params) etat))
     ((:name (function $$) :params _*)
      (apply (mini-meval name etat) params))
     ((:name $$ :params _*)
      (let ((definition (assoc-etat name 'function etat)))
        (if definition
            (apply (cdr definition) (mapcar (lambda (x) (mini-meval x etat)) params))
            (mini-meval-error expr etat "Undefined function : ~w." name))))
     ((? or numberp stringp)
      expr)
     ;; TODO : nil et t devraient être des defconst
     (nil
      nil)
     ($$
      (let ((definition (assoc-etat expr 'variable etat)))
        (if definition
            (cdr definition)
            (mini-meval-error expr etat "Undefined variable : ~w." expr))))))

  (defun push-functions (etat functions)
    (dolist (f functions)
      (push-global! etat f 'function (fdefinition f)))
    etat)

  (defmacro make-etat (&rest functions)
    `(push-functions (list nil nil nil) ',functions))

  (defun etat-exemple ()
    (make-etat list + - cons car cdr < > <= >= =))
  (mini-meval '(+ 2 3))
)
