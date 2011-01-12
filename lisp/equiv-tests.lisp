;(require 'squash-lisp "squash-lisp")
(require 'mini-meval "mini-meval")
(require 'test-unitaire "test-unitaire")

(defmacro deftest-equiv (module test expected)
  `(progn
     (deftest ,(append '(equiv eval expected)          module) (eval ,test) ,expected)
     (deftest ,(append '(equiv mini-meval expected)    module) (mini-meval ,test etat) ,expected)
     (deftest ,(append '(equiv squash-lisp-1 check)    module) (squash-lisp-1-check (squash-lisp-1 ,test t etat)) t)
     (deftest ,(append '(equiv squash-lisp-1 expected) module) (eval (squash-lisp-1-wrap (squash-lisp-1 ,test t etat))) ,expected)
     (deftest ,(append '(equiv squash-lisp-3 check)    module) (squash-lisp-3-check (squash-lisp-1+3 ,test etat)) t)
     (deftest ,(append '(equiv squash-lisp-3 expected) module) (eval (squash-lisp-3-wrap (squash-lisp-1+3 ,test etat))) ,expected)))

(erase-tests equiv)
     
(deftestvar (equiv) etat (push-local (make-etat list + - cons car cdr < > <= >= =) '*test-equiv-var-x* 'variable 42))
(defvar *test-equiv-var-x* 42)

(deftest-equiv (constante)
    42
  42)

(deftest-equiv (appel-fonction)
    '(+ 2 3)
  5)

(deftest-equiv (appel-fonction)
    '(+ 2 (+ 3 4))
  9)

(deftest-equiv (variable)
    '*test-equiv-var-x*
  42)

(deftest-equiv (appel-fonction-et-variable)
    '(+ *test-equiv-var-x* *test-equiv-var-x* 3)
  87)

(deftest-equiv (appel-fonction-et-variable)
    '(+ *test-equiv-var-x* (+ 3 *test-equiv-var-x*))
  87)

(deftest-equiv (lambda immédiat)
    '((lambda (x) (+ x 3)) 4)
  7)

(deftest-equiv (lambda sans-params)
    '((lambda () 42))
  42)

(deftest-equiv (lambda sans-body)
    '((lambda ()))
  nil)

(deftest-equiv (let)
    '(let ((x 3) (y 4)) (+ x y))
  7)

(deftest-equiv (let)
    '(let ((x 3) (y 4) (z 5)) (let ((z (+ x y)) (w z)) (list x y z w)))
  '(3 4 7 5))

(deftest-equiv (let*)
    '(let ((x 3) (y 4) (z 5)) z (let* ((z (+ x y)) (w z)) (list x y z w)))
  '(3 4 7 7))

;; TODO
;; (deftest-equiv (let-nil)
;;     '(let (a (x 3) y) (list a x y))
;;   '(nil 3 nil))

;; (deftest-equiv (let-nil)
;;     '(let* ((x 4) y (z 5)) (list a x y))
;;   '(4 nil 5))

(deftest-equiv (progn)
    '(progn 1 2 3 4)
  4)

(deftest-equiv (quote)
    ''x
  'x)

(deftest-equiv (macrolet)
    '(labels ((qlist (a b) (list a b)))
      (list
       (qlist 'a 'b)
       (macrolet ((qlist (x y) (list 'list (list 'quote x) (list 'quote y))))
         (qlist 'a 'b))
       (qlist 'a 'b)))
  '((a b) ('a 'b) (a b)))

(deftest-equiv (setf setq)
    '(let ((x 42)) (list x (setq x 123) x))
  '(42 123 123))

(deftest-equiv (funcall)
    '(funcall #'+ 1 2 3)
  '6)

(deftest-equiv (apply)
    '(apply #'+ 1 2 (list (+ 1 2) 4))
  '10)

(deftest-equiv (function extérieur)
    '(funcall #'+ 1 2 3)
  '6)

(deftest-equiv (lambda optional)
    '((lambda (x &optional (y 2)) (list x y)) 1)
  '(1 2))

(deftest-equiv (lambda closure single-instance)
    '(let ((foo (let ((y 1)) (cons (lambda (x) (list x y)) (lambda (z) (setq y (+ y z)) nil)))))
      (list (funcall (car foo) 4) (funcall (cdr foo) 5) (funcall (car foo) 4)))
  '((4 1) nil (4 6)))

(deftest-equiv (lambda closure multiple-instances)
    '(labels ((counter (&optional (ctr 0)) (cons (lambda () ctr) (lambda (&optional (x 1)) (setq ctr (+ ctr x)) nil))))
      (let ((foo0  (counter))
            (foo42 (counter 42)))
        (list
         (funcall (car foo0))    ;; show 0
         (funcall (car foo42))   ;; show 42
         (funcall (cdr foo0))    ;; add 0
         (funcall (car foo0))    ;; show 0
         (funcall (cdr foo42))   ;; add 42
         (funcall (car foo42))   ;; show 42
         (funcall (car foo0))    ;; shwo 0
         (funcall (car foo42))   ;; show 42
         (funcall (cdr foo42) 6) ;; add 42 (+ 6)
         (funcall (cdr foo0) 5)  ;; add 0  (+ 5)
         (funcall (car foo42))   ;; show 42
         (funcall (car foo0))))) ;; show 0
  '(0 42 nil 1 nil 43 1 43 nil nil 49 6))

(deftest-equiv (labels)
    '(labels ((foo (x) (+ x 1)))
      (list
       (foo 3)
       (labels ((foo (x) (+ x 3)))
         (foo 3))))
  '(4 6))

(deftest-equiv (labels)
    '(< 2 3)
  t)
 
(deftest-equiv (flet)
    '(labels ((foo (x) (+ x 1)))
      (list
       (foo 3)
       (flet ((foo (x) (+ x 3)))
         (foo 3))))
  '(4 6))

(deftest-equiv (labels)
    '(labels ((fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
      (list
       (fibo 5)
       (labels ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
         (fibo 5))
       (fibo 5)))
  '(8 5 8))

(deftest-equiv (flet)
    '(labels ((fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
      (list
       (fibo 5)
       (flet ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
         (fibo 5))))
  ;; Le flet ne permet pas les définitions récursives, donc le fibo
  ;; de l'extérieur est appellé après le 1er niveau de récursion.
  '(8 8))

(deftest-equiv (tagbody)
    '(let ((x 0)) (tagbody foo (setq x 1) (go baz) bar (setq x 2) baz) x)
  1)

(deftest-equiv (tagbody)
    '(let ((x 0)) (tagbody foo (setq x 1) (go 42) bar (setq x 2) 42) x)
  1)

(deftest-equiv (tagbody)
    '(tagbody foo (list 1) 42 (list 2) baz (list 3))
  nil)

(deftest-equiv (block)
    '(block foo 1 (return-from foo 4) 2)
  4)

(deftest-equiv (block)
    '(block foo 1 2)
  2)

(deftest-equiv (tagbody)
    '(let ((res nil))
      (tagbody
       a
       1
         (setq res (cons 'x res))
       b
         (setq res (cons 'y res))
         (go 3)
       d
         (setq res (cons 'z res))
       3
         (setq res (cons 'f res)))
      res)
  '(f y x))

(deftest-equiv (flet)
    '(flet ((foo (x) (+ x 1)) (bar (y z) (cons y z))) (list (foo 3) (bar 4 5)))
  '(4 (4 . 5)))
  
(deftest-equiv (function extérieur)
    '#'+
  #'+)

(deftest-equiv (lambda captures)
    '(funcall ((lambda (x y) x (lambda (x) (+ x y))) 1 2) 3)
  5)

(deftest-equiv (lambda captures ycombinator)
    '((lambda (fibo)
        (list (funcall fibo 0)
              (funcall fibo 1)
              (funcall fibo 2)
              (funcall fibo 3)
              (funcall fibo 4)
              (funcall fibo 5)
              (funcall fibo 6)
              (funcall fibo 7)))
      ((lambda (f) (lambda (x) (funcall f f x))) (lambda (f n) (if (<= n 1) n (+ (funcall f f (- n 1)) (funcall f f (- n 2)))))))
  '(0 1 1 2 3 5 8 13))

(provide 'equiv-tests)
