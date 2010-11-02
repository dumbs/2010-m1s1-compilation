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

(defun pattern-match-do-lambdas-? (pattern)
  (mapcar (lambda (pred)
            (cond ((atom pred)               (list 'quote pred))
                  ((eq (car pred) 'function) pred)
                  ((eq (car pred) 'lambda)   pred)
                  (t
                   `(lambda (x) ,pred))))
          (cdr pattern)))

(defmacro pattern-match-do-lambdas (pattern)
  "Transforme les (? x <code>) et (? (lambda ...)) en vrais lambdas."
  (cond
    ;; (? x <code>)
    ((and (n-consp 2 pattern)
          (eq '? (first pattern)))
     (cond ((eq 'and (second pattern)) `(list '? 'and ,@(pattern-match-do-lambdas-? (cdr pattern))))
           ((eq 'or  (second pattern)) `(list '? 'or  ,@(pattern-match-do-lambdas-? (cdr pattern))))
           (t                          `(list '? 'and ,@(pattern-match-do-lambdas-? pattern)))))
    ;; (p1 p2 ... pn)
    ((consp pattern)
     ;; Transformation de chaque pattern de la liste y compris
     ;; le dernier cdr si ce n'est pas nil.
     (labels ((recurse (pat)
                (cond ((null pat) nil)
                      ((atom pat) `(pattern-match-do-lambdas ,pat))
                      (t          `(cons (pattern-match-do-lambdas ,(car pat))
                                         ,(recurse (cdr pat)))))))
       (recurse pattern)))
    ;; Autres cas
    (t
     (list 'quote pattern))))
(defun transform (pat)
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

;; TODO : peut-être virer cette fonction, elle est *très* moche
;; et pas très utile.
(defun pattern-match-preprocess (pattern)
  "Transforme les symbol*, symbol+ et symbol?
   en symbol *, symbol + et symbol ?"
  (cond ((and (consp pattern) (eq '? (first pattern)))
         pattern) ;; On ne touche pas les (? ...)
        ((consp pattern)
         (labels ((transform (pat)
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
                      ((symbolp pat) (transform pat))
                      ((atom pat) pat)
                      ((symbolp (car pat))
                       (let ((transf (transform (car pat))))
                         (if (consp transf)
                             `(,@transf ,@(recurse (cdr pat)))
                             `(,transf ,@(recurse (cdr pat))))))
                      (t (cons (pattern-match-preprocess (car pat))
                               (recurse (cdr pat)))))))
           (recurse pattern)))
        (t
         pattern)))

(defun pattern-match (pattern expr)
  (cond
    ;; (pattern * ...)
    ((and (n-consp 2 pattern) (eq '* (second pattern)))
     (or (pattern-match (cddr pattern) expr)
         (and (consp expr)
              (pattern-match (car pattern) (car expr))
              (pattern-match pattern (cdr expr)))))
    ;; (pattern + ...)
    ((and (n-consp 2 pattern) (eq '+ (second pattern)))
     (and (consp expr)
          (pattern-match (first pattern) (car expr))
          (pattern-match `(,(first pattern) * ,@(cddr pattern)) (cdr expr))))
    ;; (pattern ? ...)
    ((and (n-consp 2 pattern) (eq '? (second pattern)))
     (or (and (consp expr)
              (pattern-match (first pattern) (car expr))
              (pattern-match (cddr pattern) (cdr expr)))
         (pattern-match (cddr pattern) expr)))
    ;; (? <prédicat(s)>)
    ((and (n-consp 2 pattern) (eq '? (first pattern)))
     (cond 
       ;; (? and symbole-1 ... symbole-n)
       ((eq 'and (second pattern))
        (every (lambda (predicat) (funcall predicat expr)) (cddr pattern)))
       ;; (? or symbole-1 ... symbole-n)
       ((eq 'or (second pattern))
        (some (lambda (predicat) (funcall predicat expr)) (cddr pattern)))
       ;; (? (lambda (x) <code>))
       ((functionp (second pattern))
        (funcall (second pattern) expr)) ;; niy
       ;; (? symbole)
       ((symbolp (second pattern))
        (funcall (second pattern) expr))
       (t
        (error "Motif malformé."))))
    ;; (pattern . rest)
    ((consp pattern)
     (and (consp expr)
          (pattern-match (car pattern) (car expr))
          (pattern-match (cdr pattern) (cdr expr))))
    ;; ()
    ((null pattern)
     (null expr))
    ;; $
    ((eq '$ pattern)
     (and (atom expr)
          (not (null expr))))
    ;; @
    ((eq '@ pattern)
     (or (null expr)
         (and (consp expr)
              (pattern-match '@ (cdr expr)))))
    ;; _
    ((eq '_ pattern)
     t)
    ;; :symbole
    ((and (consp pattern) (keywordp (car pattern)))
     "niy")
    ;; symbole
    ((symbolp pattern)
     (eq pattern expr))
    ;; Autres valeurs
    (t
     (equal pattern expr))))

(defmacro match (pattern expr)
  `(pattern-match
    (pattern-match-preprocess
     (pattern-match-do-lambdas ,pattern))
    ,expr))

(load "test-unitaire")
(erase-tests match)

;;; Symboles, chiffres, etc

(deftest match (match a              'a)             t   #'booleq)
(deftest match (match 1              '1)             t   #'booleq)
(deftest match (match 1              '1.0)           nil #'booleq) ;; TODO : devrait être nil ou t ?
(deftest match (match a              'b)             nil #'booleq) ;; $ oui, a non
(deftest match (match 1              '2)             nil #'booleq) ;; $ oui, 1 non
(deftest match (match a              '())            nil #'booleq)
(deftest match (match a              '(a))           nil #'booleq)
(deftest match (match a              '(a b))         nil #'booleq)
(deftest match (match a              '(a . b))       nil #'booleq)

;; Même chose que pour symboles, mais avec $

(deftest match (match $              'a)             t   #'booleq)
(deftest match (match $              '1)             t   #'booleq)
(deftest match (match $              '1.0)           t   #'booleq) ;; $ oui, 1 je sais pas
(deftest match (match $              'b)             t   #'booleq) ;; $ oui, a non
(deftest match (match $              '2)             t   #'booleq) ;; $ oui, 1 non
(deftest match (match $              '())            nil #'booleq)
(deftest match (match $              '(a))           nil #'booleq)
(deftest match (match $              '(a b))         nil #'booleq)
(deftest match (match $              '(a . b))       nil #'booleq)

;;; Listes et sous-lites
(deftest match (match ()             '())            t   #'booleq)
(deftest match (match ()             '(a))           nil #'booleq)
(deftest match (match ()             'a)             nil #'booleq)
(deftest match (match ()             '(a . b))       nil #'booleq)

(deftest match (match (a)            '(a))           t   #'booleq)
(deftest match (match (a)            '(b))           nil #'booleq)
(deftest match (match (a)            'a)             nil #'booleq)
(deftest match (match (a)            '(a b))         nil #'booleq)
(deftest match (match (a)            '())            nil #'booleq)

(deftest match (match (a b)          '(a b))         t   #'booleq)
(deftest match (match (a b)          '(a c))         nil #'booleq)
(deftest match (match (a b)          '(c b))         nil #'booleq)
(deftest match (match (a b)          '(a))           nil #'booleq)
(deftest match (match (a b)          '())            nil #'booleq)
(deftest match (match (a b)          'a)             nil #'booleq)
(deftest match (match (a b)          '(a b c))       nil #'booleq)

(deftest match (match (a (1 2 3) c)  '(a (1 2 3) c)) t   #'booleq)
(deftest match (match (a (1 2 3) c)  '(a (1 2) c))   nil #'booleq)
(deftest match (match (a (1 2 3) c)  '(a () c))      nil #'booleq)
(deftest match (match (a (1 2 3) c)  '(x (1 2 3) c)) nil #'booleq)

;;; _

(deftest match (match _              'a)             t   #'booleq)
(deftest match (match _              '(a b))         t   #'booleq)
(deftest match (match _              '())            t   #'booleq)

(deftest match (match (a _)          '(a b))         t   #'booleq)
(deftest match (match (a _)          '(a (1 2 3)))   t   #'booleq)
(deftest match (match (a _)          '(a (b c)))     t   #'booleq)
(deftest match (match (a _)          '(a (b)))       t   #'booleq)
(deftest match (match (a _)          '(a ()))        t   #'booleq)
(deftest match (match (a _)          '(a (b . c)))   t   #'booleq)
(deftest match (match (a _)          '(x b))         nil #'booleq)
(deftest match (match (a _)          '(x (b)))       nil #'booleq)
(deftest match (match (a _)          '(a))           nil #'booleq)
(deftest match (match (a _)          '(a b c))       nil #'booleq)
(deftest match (match (a _)          '())            nil #'booleq)
(deftest match (match (a _)          'a)             nil #'booleq)

(deftest match (match (a _ c)        '(a b c))       t   #'booleq)
(deftest match (match (a _ c)        '(a (1 2 3) c)) t   #'booleq)
(deftest match (match (a _ c)        '(a (b c) c))   t   #'booleq)
(deftest match (match (a _ c)        '(a (b) c))     t   #'booleq)
(deftest match (match (a _ c)        '(a () c))      t   #'booleq)
(deftest match (match (a _ c)        '(a (b . c) c)) t   #'booleq)
(deftest match (match (a _ c)        '(x (b) c))     nil #'booleq)
(deftest match (match (a _ c)        '(x b c))       nil #'booleq)
(deftest match (match (a _ c)        '(x (b) c))     nil #'booleq)
(deftest match (match (a _ c)        '(a c))         nil #'booleq)
(deftest match (match (a _ c)        '(a))           nil #'booleq)
(deftest match (match (a _ c)        '(a b c d))     nil #'booleq)
(deftest match (match (a _ c)        '())            nil #'booleq)
(deftest match (match (a _ c)        'a)             nil #'booleq)
(deftest match (match (a _ c)        '(a b b c))     nil #'booleq)
(deftest match (match (a _ c)        '(a b x c))     nil #'booleq)

(deftest match (match (a . _)        '(a b))         t   #'booleq)
(deftest match (match (a . _)        '(a b c))       t   #'booleq)
(deftest match (match (a . _)        '(a))           t   #'booleq)
(deftest match (match (a . _)        '(a b . c))     t   #'booleq)
(deftest match (match (a . _)        '(a . b))       t   #'booleq)
(deftest match (match (a . _)        '(a . (b c)))   t   #'booleq) ;; equivalent à '(a b c)
(deftest match (match (a . _)        '(a . ()))      t   #'booleq) ;; equivalent à '(a)
(deftest match (match (a . _)        '(x . b))       nil #'booleq)
(deftest match (match (a . _)        '(x b c))       nil #'booleq)
(deftest match (match (a . _)        '(x))           nil #'booleq)
(deftest match (match (a . _)        '())            nil #'booleq)
(deftest match (match (a . _)        'a)             nil #'booleq)

(deftest match (match (a _ . _)      '(a b . c))     t   #'booleq)
(deftest match (match (a _ . _)      '(a () . ()))   t   #'booleq)
(deftest match (match (a _ . _)      '(a (1) . (2))) t   #'booleq)
(deftest match (match (a _ . _)      '(a 1 . (2)))   t   #'booleq) ;; equivalent à '(a 1 2)
(deftest match (match (a _ . _)      '(a (1) . 2))   t   #'booleq)
(deftest match (match (a _ . _)      '(a))           nil #'booleq)
(deftest match (match (a _ . _)      'a)             nil #'booleq)

(deftest match (match (_)            '(a))           t   #'booleq)
(deftest match (match (_)            '(()))          t   #'booleq)
(deftest match (match (_)            '((a b)))       t   #'booleq)
(deftest match (match (_)            '(a b))         nil #'booleq)
(deftest match (match (_)            '())            nil #'booleq)
(deftest match (match (_)            'a)             nil #'booleq)

;;; @  Mêmes tests que _ , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest match (match @              'a)             nil #'booleq) ;; diff
(deftest match (match @              '(a b))         t   #'booleq)
(deftest match (match @              '())            t   #'booleq)

(deftest match (match (a @)          '(a b))         nil #'booleq) ;; diff
(deftest match (match (a @)          '(a (1 2 3)))   t   #'booleq)
(deftest match (match (a @)          '(a (b c)))     t   #'booleq)
(deftest match (match (a @)          '(a (b)))       t   #'booleq)
(deftest match (match (a @)          '(a ()))        t   #'booleq)
(deftest match (match (a @)          '(a (b . c)))   nil #'booleq) ;; diff
(deftest match (match (a @)          '(x b))         nil #'booleq)
(deftest match (match (a @)          '(x (b)))       nil #'booleq)
(deftest match (match (a @)          '(a))           nil #'booleq)
(deftest match (match (a @)          '(a b c))       nil #'booleq)
(deftest match (match (a @)          '())            nil #'booleq)
(deftest match (match (a @)          'a)             nil #'booleq)

(deftest match (match (a @ c)        '(a b c))       nil #'booleq) ;; diff
(deftest match (match (a @ c)        '(a (1 2 3) c)) t   #'booleq)
(deftest match (match (a @ c)        '(a (b c) c))   t   #'booleq)
(deftest match (match (a @ c)        '(a (b) c))     t   #'booleq)
(deftest match (match (a @ c)        '(a () c))      t   #'booleq)
(deftest match (match (a @ c)        '(a (b . c) c)) nil #'booleq) ;; diff
(deftest match (match (a @ c)        '(x (b) c))     nil #'booleq)
(deftest match (match (a @ c)        '(x b c))       nil #'booleq)
(deftest match (match (a @ c)        '(x (b) c))     nil #'booleq)
(deftest match (match (a @ c)        '(a c))         nil #'booleq)
(deftest match (match (a @ c)        '(a))           nil #'booleq)
(deftest match (match (a @ c)        '(a b c d))     nil #'booleq)
(deftest match (match (a @ c)        '())            nil #'booleq)
(deftest match (match (a @ c)        'a)             nil #'booleq)
(deftest match (match (a @ c)        '(a b b c))     nil #'booleq)
(deftest match (match (a @ c)        '(a b x c))     nil #'booleq)

(deftest match (match (a . @)        '(a b))         t   #'booleq)
(deftest match (match (a . @)        '(a b c))       t   #'booleq)
(deftest match (match (a . @)        '(a))           t   #'booleq)
(deftest match (match (a . @)        '(a b . c))     nil #'booleq) ;; diff
(deftest match (match (a . @)        '(a . b))       nil #'booleq) ;; diff
(deftest match (match (a . @)        '(a . (b c)))   t   #'booleq) ;; equivalent à '(a b c)
(deftest match (match (a . @)        '(a . ()))      t   #'booleq) ;; equivalent à '(a)
(deftest match (match (a . @)        '(x . b))       nil #'booleq)
(deftest match (match (a . @)        '(x b c))       nil #'booleq)
(deftest match (match (a . @)        '(x))           nil #'booleq)
(deftest match (match (a . @)        '())            nil #'booleq)
(deftest match (match (a . @)        'a)             nil #'booleq)

(deftest match (match (a @ . @)      '(a b . c))     nil #'booleq) ;; diff
(deftest match (match (a @ . @)      '(a () . ()))   t   #'booleq)
(deftest match (match (a @ . @)      '(a (1) . (2))) t   #'booleq)
(deftest match (match (a @ . @)      '(a 1 . (2)))   nil #'booleq) ;; diff  ;; equivalent à '(a 1 2)
(deftest match (match (a @ . @)      '(a (1) . 2))   nil #'booleq) ;; diff
(deftest match (match (a @ . @)      '(a))           nil #'booleq)
(deftest match (match (a @ . @)      'a)             nil #'booleq)

(deftest match (match (@)            '(a))           nil #'booleq) ;; diff
(deftest match (match (@)            '(()))          t   #'booleq)
(deftest match (match (@)            '((a b)))       t   #'booleq)
(deftest match (match (@)            '(a b))         nil #'booleq)
(deftest match (match (@)            '())            nil #'booleq)
(deftest match (match (@)            'a)             nil #'booleq)

;;; $  Mêmes tests que _ , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest match (match $              'a)             t   #'booleq)
(deftest match (match $              '(a b))         nil #'booleq) ;; diff
(deftest match (match $              '())            nil #'booleq) ;; diff

(deftest match (match (a $)          '(a b))         t   #'booleq)
(deftest match (match (a $)          '(a (1 2 3)))   nil #'booleq) ;; diff
(deftest match (match (a $)          '(a (b c)))     nil #'booleq) ;; diff
(deftest match (match (a $)          '(a (b)))       nil #'booleq) ;; diff
(deftest match (match (a $)          '(a ()))        nil #'booleq) ;; diff
(deftest match (match (a $)          '(a (b . c)))   nil #'booleq) ;; diff
(deftest match (match (a $)          '(x b))         nil #'booleq)
(deftest match (match (a $)          '(x (b)))       nil #'booleq)
(deftest match (match (a $)          '(a))           nil #'booleq)
(deftest match (match (a $)          '(a b c))       nil #'booleq)
(deftest match (match (a $)          '())            nil #'booleq)
(deftest match (match (a $)          'a)             nil #'booleq)

(deftest match (match (a $ c)        '(a b c))       t   #'booleq)
(deftest match (match (a $ c)        '(a (1 2 3) c)) nil #'booleq) ;; diff
(deftest match (match (a $ c)        '(a (b c) c))   nil #'booleq) ;; diff
(deftest match (match (a $ c)        '(a (b) c))     nil #'booleq) ;; diff
(deftest match (match (a $ c)        '(a () c))      nil #'booleq) ;; diff
(deftest match (match (a $ c)        '(a (b . c) c)) nil #'booleq) ;; diff
(deftest match (match (a $ c)        '(x (b) c))     nil #'booleq)
(deftest match (match (a $ c)        '(x b c))       nil #'booleq)
(deftest match (match (a $ c)        '(x (b) c))     nil #'booleq)
(deftest match (match (a $ c)        '(a c))         nil #'booleq)
(deftest match (match (a $ c)        '(a))           nil #'booleq)
(deftest match (match (a $ c)        '(a b c d))     nil #'booleq)
(deftest match (match (a $ c)        '())            nil #'booleq)
(deftest match (match (a $ c)        'a)             nil #'booleq)
(deftest match (match (a $ c)        '(a b b c))     nil #'booleq)
(deftest match (match (a $ c)        '(a b x c))     nil #'booleq)

(deftest match (match (a . $)        '(a b))         nil #'booleq) ;; diff
(deftest match (match (a . $)        '(a b c))       nil #'booleq) ;; diff
(deftest match (match (a . $)        '(a))           nil #'booleq) ;; diff
(deftest match (match (a . $)        '(a b . c))     nil #'booleq) ;; diff
(deftest match (match (a . $)        '(a . b))       t   #'booleq)
(deftest match (match (a . $)        '(a . (b c)))   nil #'booleq) ;; diff  ;; equivalent à '(a b c)
(deftest match (match (a . $)        '(a . ()))      nil #'booleq) ;; diff  ;; equivalent à '(a)
(deftest match (match (a . $)        '(x . b))       nil #'booleq)
(deftest match (match (a . $)        '(x b c))       nil #'booleq)
(deftest match (match (a . $)        '(x))           nil #'booleq)
(deftest match (match (a . $)        '())            nil #'booleq)
(deftest match (match (a . $)        'a)             nil #'booleq)

(deftest match (match (a $ . $)      '(a b . c))     t   #'booleq)
(deftest match (match (a $ . $)      '(a () . ()))   nil #'booleq) ;; diff
(deftest match (match (a $ . $)      '(a (1) . (2))) nil #'booleq) ;; diff
(deftest match (match (a $ . $)      '(a 1 . (2)))   nil #'booleq) ;; diff  ;; equivalent à '(a 1 2)
(deftest match (match (a $ . $)      '(a (1) . 2))   nil #'booleq) ;; diff
(deftest match (match (a $ . $)      '(a))           nil #'booleq)
(deftest match (match (a $ . $)      'a)             nil #'booleq)

(deftest match (match ($)            '(a))           t   #'booleq)
(deftest match (match ($)            '(()))          nil #'booleq) ;; diff
(deftest match (match ($)            '((a b)))       nil #'booleq) ;; diff
(deftest match (match ($)            '(a b))         nil #'booleq)
(deftest match (match ($)            '())            nil #'booleq)
(deftest match (match ($)            'a)             nil #'booleq)

;;; *

(deftest match (match (a *)             '(a a a a))                       t   #'booleq)
(deftest match (match (a *)             '(a))                             t   #'booleq)
(deftest match (match (a *)             '())                              t   #'booleq)
(deftest match (match (a *)             'a)                               nil #'booleq)
(deftest match (match (a *)             '(b b b b))                       nil #'booleq)
(deftest match (match (a *)             '(b))                             nil #'booleq)

(deftest match (match (a (_ *) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest match (match (a (_ *) c)       '(a ((1 2) 3 () (4 5)) c))        t   #'booleq)
(deftest match (match (a (_ *) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest match (match (a (_ *) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest match (match (a (_ *) c)       '(a (x) c))                       t   #'booleq)
(deftest match (match (a (_ *) c)       '(a () c))                        t   #'booleq)
(deftest match (match (a (_ *) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a (@ *) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest match (match (a (@ *) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest match (match (a (@ *) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest match (match (a (@ *) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest match (match (a (@ *) c)       '(a (x) c))                       nil #'booleq)
(deftest match (match (a (@ *) c)       '(a () c))                        t   #'booleq)
(deftest match (match (a (@ *) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a ($ *) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest match (match (a ($ *) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest match (match (a ($ *) c)       '(a ((1 2)) c))                   nil #'booleq)
(deftest match (match (a ($ *) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest match (match (a ($ *) c)       '(a (x) c))                       t   #'booleq)
(deftest match (match (a ($ *) c)       '(a () c))                        t   #'booleq)
(deftest match (match (a ($ *) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a (1 $ * 2)* c)  '(a (1 2) (1 b 2) (1 1 2) c))     t   #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a (1 x 2) (1 b 2) (1 1 1 2) c)) t   #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2) c))   t   #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a (1 2) (1 b)   (1 1 1 2) c))   nil #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a (1 2)  1 b 2  (1 1 1 2) c))   nil #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2)))     nil #'booleq)

(deftest match (match (a (1 $ * 2)* c)  '(a (1 b 2) (1 b 2) c))           t   #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a (1 b 2) c))                   t   #'booleq)
(deftest match (match (a (1 $ * 2)* c)  '(a c))                           t   #'booleq)

;;; + Mêmes tests que * , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest match (match (a +)             '(a a a a))                       t   #'booleq)
(deftest match (match (a +)             '(a))                             t   #'booleq)
(deftest match (match (a +)             '())                              nil #'booleq) ;; diff
(deftest match (match (a +)             'a)                               nil #'booleq)
(deftest match (match (a +)             '(b b b b))                       nil #'booleq)
(deftest match (match (a +)             '(b))                             nil #'booleq)

(deftest match (match (a (_ +) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest match (match (a (_ +) c)       '(a ((1 2) 3 () (4 5)) c))        t   #'booleq)
(deftest match (match (a (_ +) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest match (match (a (_ +) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest match (match (a (_ +) c)       '(a (x) c))                       t   #'booleq)
(deftest match (match (a (_ +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest match (match (a (_ +) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a (@ +) c)       '(a ((1 2) (3) () (4 5)) c))      t   #'booleq)
(deftest match (match (a (@ +) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest match (match (a (@ +) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest match (match (a (@ +) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest match (match (a (@ +) c)       '(a (x) c))                       nil #'booleq)
(deftest match (match (a (@ +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest match (match (a (@ +) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a ($ +) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest match (match (a ($ +) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest match (match (a ($ +) c)       '(a ((1 2)) c))                   nil #'booleq)
(deftest match (match (a ($ +) c)       '(a (1 2 3) c))                   t   #'booleq)
(deftest match (match (a ($ +) c)       '(a (x) c))                       t   #'booleq)
(deftest match (match (a ($ +) c)       '(a () c))                        nil #'booleq) ;; diff
(deftest match (match (a ($ +) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a (1 $ + 2)* c)  '(a (1 2) (1 b 2) (1 1 2) c))     nil #'booleq) ;; diff
(deftest match (match (a (1 $ + 2)* c)  '(a (1 x 2) (1 b 2) (1 1 1 2) c)) t   #'booleq)
(deftest match (match (a (1 $ + 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2) c))   nil #'booleq) ;; diff
(deftest match (match (a (1 $ + 2)* c)  '(a (1 2) (1 b)   (1 1 1 2) c))   nil #'booleq)
(deftest match (match (a (1 $ + 2)* c)  '(a (1 2)  1 b 2  (1 1 1 2) c))   nil #'booleq)
(deftest match (match (a (1 $ + 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2)))     nil #'booleq)

(deftest match (match (a (1 $ + 2)+ c)  '(a (1 b 2) (1 b 2) c))           t   #'booleq)
(deftest match (match (a (1 $ + 2)+ c)  '(a (1 b 2) c))                   t   #'booleq)
(deftest match (match (a (1 $ + 2)+ c)  '(a c))                           nil #'booleq) ;; diff

;;; ? Mêmes tests que * , on indique les différences avec ";; diff" à la fin de la ligne.

(deftest match (match (a ?)             '(a a a a))                       nil #'booleq) ;; diff
(deftest match (match (a ?)             '(a))                             t   #'booleq)
(deftest match (match (a ?)             '())                              t   #'booleq)
(deftest match (match (a ?)             'a)                               nil #'booleq)
(deftest match (match (a ?)             '(b b b b))                       nil #'booleq)
(deftest match (match (a ?)             '(b))                             nil #'booleq)

(deftest match (match (a (_ ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq) ;; diff
(deftest match (match (a (_ ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq) ;; diff
(deftest match (match (a (_ ?) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest match (match (a (_ ?) c)       '(a (1 2 3) c))                   nil #'booleq) ;; diff
(deftest match (match (a (_ ?) c)       '(a (x) c))                       t   #'booleq)
(deftest match (match (a (_ ?) c)       '(a () c))                        t   #'booleq)
(deftest match (match (a (_ ?) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a (@ ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq) ;; diff
(deftest match (match (a (@ ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest match (match (a (@ ?) c)       '(a ((1 2)) c))                   t   #'booleq)
(deftest match (match (a (@ ?) c)       '(a (1 2 3) c))                   nil #'booleq)
(deftest match (match (a (@ ?) c)       '(a (x) c))                       nil #'booleq)
(deftest match (match (a (@ ?) c)       '(a () c))                        t   #'booleq)
(deftest match (match (a (@ ?) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a ($ ?) c)       '(a ((1 2) (3) () (4 5)) c))      nil #'booleq)
(deftest match (match (a ($ ?) c)       '(a ((1 2) 3 () (4 5)) c))        nil #'booleq)
(deftest match (match (a ($ ?) c)       '(a ((1 2)) c))                   nil #'booleq)
(deftest match (match (a ($ ?) c)       '(a (1 2 3) c))                   nil #'booleq) ;; diff
(deftest match (match (a ($ ?) c)       '(a (x) c))                       t   #'booleq)
(deftest match (match (a ($ ?) c)       '(a () c))                        t   #'booleq)
(deftest match (match (a ($ ?) c)       '(a x c))                         nil #'booleq)

(deftest match (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b 2) (1 1 2) c))     t   #'booleq)
(deftest match (match (a (1 $ ? 2)* c)  '(a (1 x 2) (1 b 2) (1 1 1 2) c)) nil #'booleq) ;; diff
(deftest match (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2) c))   nil #'booleq) ;; diff
(deftest match (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b)   (1 1 1 2) c))   nil #'booleq)
(deftest match (match (a (1 $ ? 2)* c)  '(a (1 2)  1 b 2  (1 1 1 2) c))   nil #'booleq)
(deftest match (match (a (1 $ ? 2)* c)  '(a (1 2) (1 b 2) (1 1 1 2)))     nil #'booleq)

(deftest match (match (a (1 $ ? 2)? c)  '(a (1 b 2) (1 b 2) c))           nil #'booleq) ;; diff
(deftest match (match (a (1 $ ? 2)? c)  '(a (1 b 2) c))                   t   #'booleq)
(deftest match (match (a (1 $ ? 2)? c)  '(a c))                           t   #'booleq)

;; (? tests...)

(deftest match (match (? numberp)                1)  t   #'booleq)
(deftest match (match (? numberp)               'a)  nil #'booleq)

(deftest match (match (? plusp)                  1)  t   #'booleq)
(deftest match (match (? plusp)                 -1)  nil #'booleq)

;; and par défaut
(deftest match (match (? integerp plusp)         1)  t   #'booleq)
(deftest match (match (? integerp plusp)       0.1)  nil #'booleq)
(deftest match (match (? integerp plusp)        -1)  nil #'booleq)
(deftest match (match (? integerp plusp)      -0.1)  nil #'booleq)

(deftest match (match (? and integerp plusp)     1)  t   #'booleq)
(deftest match (match (? and integerp plusp)   0.1)  nil #'booleq)
(deftest match (match (? and integerp plusp)    -1)  nil #'booleq)
(deftest match (match (? and integerp plusp)  -0.1)  nil #'booleq)

(deftest match (match (? or integerp plusp)      1)  t   #'booleq)
(deftest match (match (? or integerp plusp)    0.1)  t   #'booleq)
(deftest match (match (? or integerp plusp)     -1)  t   #'booleq)
(deftest match (match (? or integerp plusp)   -0.1)  nil #'booleq)

(deftest match (match (? numberp (> x 3))        5)  t   #'booleq)
(deftest match (match (? numberp (> x 3))        2)  nil #'booleq)

(deftest match (match (? numberp (lambda (y) (> y 3))) 5)  t   #'booleq)
(deftest match (match (? numberp (lambda (y) (> y 3))) 3)  nil #'booleq)

;(run-tests match)
