(require 'match "match")

(defun squash-lisp-3 (expr local-env globals)
  "Lorsqu'une variable à l'intérieur d'une `lambda` référence une déclaration à l'extérieur de la `lambda`, on la marque comme étant *capturée*.
   
   On fusionne tous les `let` d'une `lambda` en les remontant dans un `let` unique à la racine de la `lamdba`.
   
   On sort toutes les lambdas (fonctions anonymes), on les nomme avec un symbole unique, et on les met au top-level."
  (macrolet ((transform (expr &optional (local-env 'local-env)) `(squash-lisp-3 ,expr ,local-env globals)))
    (cond-match
     expr
     ;; simple-tagbody est équivalent à un progn, mais nécessaire pour les macrolet.
     (((? (member x '(progn simple-tagbody))) :body _*)
      (let ((res (list 'progn)))
        (labels ((squash-progn (body)
                   (dolist (e body)
                     (if (and (consp e) (eq 'progn (car e)))
                         (squash-progn (cdr e))
                         (push (squash-lisp-3 e local-env globals) res)))))
          (squash-progn body))
        ;; TODO : ici : filtrer les expressions de `res' qui sont sans effet de bord, sauf la dernière.
        (print res)
        (if (cdr res) ;; res != '(progn)
            (if (cddr res) ;; res != '(single-expr progn)
                (reverse res)
                (car res))
            '(quote nil))))
     ((if :condition _ :si-vrai _ :si-faux _)
      (and (squash-lisp-3 condition)
           (squash-lisp-3 si-vrai)
           (squash-lisp-3 si-faux)))
     ((unwind-protect :body _ :cleanup _)
      (and (squash-lisp-3 body)
           (squash-lisp-3 cleanup)))
     ;; tagbody-unwind-catch est équivalent à unwind-catch, mais nécessaire pour les macrolet.
     (((? (member x '(unwind-catch tagbody-unwind-catch))) :object _ :body (progn _*) :catch-code _)
      (and (squash-lisp-3 object)
           (squash-lisp-3 body)
           (squash-lisp-3 catch-code)))
     ((unwind :object _)
      (squash-lisp-3 object))
     ((unwind-for-tagbody :object _ :post-unwind-code _)
      (and (squash-lisp-3 object)
           (squash-lisp-3 post-unwind-code)))
     ((jump-label :name $$)
      t)
     ((jump :dest $$)
      t)
     ((let ($$*) :body _)
      (squash-lisp-3 body))
     ((lambda :params (&rest $$) :unused _ :body (let ($$*) _*))
      (push let-vars stack)
      (push `(lambda ,params
               ,unused
               (let (,let-vars)
                 ,(squash-lisp-3 body)))
            top-level)
      (setq let-vars (pop stack)))
     ((funcall :fun _ :params _*)
      (every #'squash-lisp-3 (cons fun params)))
     ((quote _)
      t)
     ((get-var :var $$)
      ;; chercher si var est dans local-env ou bien dans global
      ;; si oui -> get-var
      ;; sinon, -> get-captured-var
      t)
     ((setq :name $$ :value _)
      ;; comme ci-dessus
      (squash-lisp-3 value))
     ((fdefinition (quote $$))
      t)
     ((symbol-value (quote $$))
      t)
     ((set (quote $$) :value _)
      (squash-lisp-3 value)))))

(require 'test-unitaire "test-unitaire")
(erase-tests squash-lisp-3)

(deftest (squash-lisp-3 progn)
    (squash-lisp-3 '(progn
                     (progn (progn) (progn))
                     (progn)
                     (progn (progn) (progn) (progn))))
  ''nil)

(deftest (squash-lisp-3 progn)
    (squash-lisp-3 '(progn))
  ''nil)

(deftest (squash-lisp-3 progn)
    (squash-lisp-3 '(progn (symbol-value 'a)))
  '(symbol-value 'a))

(deftest (squash-lisp-3 progn)
    (squash-lisp-3 '(progn
                     (progn (progn (symbol-value 'a)) (progn))
                     (progn)
                     (progn (progn) (progn) (progn))))
  '(symbol-value 'a))

;(run-tests squash-lisp-3)