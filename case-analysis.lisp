;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(defun case-analysis (expr env)
  (if (atom expr)
      (if (constantp expr)
          ;; cas literal
          (let ((cell (<search> expr env)))
            (if cell
                ;; cas d'une variable
                (<signal> "~S n'est pas une variable" expr))))
    (cond ((and (listp (car expr)) (eq (caar expr) 'lambda))
           ;; cas lambda fonction => recursivite
           )
          ((not (symbolp (car expr)))
           (<signal> "~S n'est pas une fonction" (car expr)))
          ((not (fboundp (car expr)))
           (<signal> "le symbole ~S n'a pas de definition fonctionnelle" (car expr)))
          ((special-form-p (car expr))
           (case (car expr)
             (quote `(:const . ,(cadr expr))) ;cas quote
             (if `(:if ,(case-analysis (cadr expr) env)
                       ,(case-analysis (caddr expr) env)
                       . ,(case-analysis (cadddr expr) env)))
             (defun ;; traitement du cas defun pas de recusivite ou sur le corps du defun
               (case-analysis (fourth expr) (<build> (third expr))))
             (T (<signal> "~S NYI" (car expr)))))
          ((macro-function (car expr))
           ;; cas des macros
           )
          (;; cas des fonctions locales -> env fonctionnel
           )
          (T ;; cas des fonctions globales recursion
           (map-case-analysis (cdr expr) env)))))