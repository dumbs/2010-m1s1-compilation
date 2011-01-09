(require 'squash-lisp "squash-lisp")
(require 'mini-meval "mini-meval")

(defun expr-equiv-p (expr &optional (expected nil expected-p))
  (let ((res-eval (eval expr))
        (temp nil))
    (setq temp (mini-meval expr))
    (unless (equalp res-eval temp)
      (return-from expr-equiv-p (format nil "mini-meval differs from eval : ~a vs ~a" res-eval temp)))
    (when expected-p
      (unless (equalp expected temp)
        (return-from expr-equiv-p "mini-meval differs from expected value")))
    (setq temp (squash-lisp-1 expr))
    (unless (squash-lisp-1-check temp)
      (return-from expr-equiv-p "squash-lisp-1-check failed"))
    (unless (equalp res-eval (eval (squash-lisp-1-wrap temp)))
      (return-from expr-equiv-p "squash-lisp-1 differs from eval"))
    ;; (setq temp (squash-lisp-2 (squash-lisp-1 expr)))
    ;; (unless (squash-lisp-2-check temp)
    ;;   (return-from expr-equiv-p "squash-lisp-2-check failed"))
    ;; (unless (equalp res-eval (eval (squash-lisp-2-wrap temp)))
    ;;   (return-from expr-equiv-p "squash-lisp-2 differs from eval"))
    t))

(defmacro deftest-equiv (module test &optional (expected nil expected-p))
  `(deftest ,module (expr-equiv-p ,test . ,(if expected-p (list expected) nil))
     t))

(deftest-equiv (mini-meval constante)
    (mini-meval 42 etat)
  42)

(deftest (mini-meval appel-fonction)
    (mini-meval '(+ 2 3) etat)
  5)

(deftest (mini-meval appel-fonction)
    (mini-meval '(+ 2 (+ 3 4)) etat)
  9)

(deftest (mini-meval variable)
    (mini-meval 'x (push-local etat 'x 'variable 42))
  42)

(deftest (mini-meval appel-fonction-et-variable)
    (mini-meval '(+ x x 3) (push-local etat 'x 'variable 42))
  87)

(deftest (mini-meval appel-fonction-et-variable)
    (mini-meval '(+ x (+ 3 x)) (push-local etat 'x 'variable 42))
  87)

(deftest (mini-meval lambda extérieur)
    (funcall (mini-meval '(lambda (x) x) etat) 3)
  3)

(deftest (mini-meval lambda extérieur)
    (funcall (mini-meval '(lambda (x) (+ x 3)) etat) 4)
  7)

(deftest (mini-meval lambda immédiat)
    (mini-meval '((lambda (x) (+ x 3)) 4) etat)
  7)

(deftest (mini-meval let)
    (mini-meval '(let ((x 3) (y 4)) (+ x y)) etat)
  7)

(deftest (mini-meval let)
    (mini-meval '(let ((x 3) (y 4) (z 5)) (let ((z (+ x y)) (w z)) (list x y z w))) etat)
  '(3 4 7 5))

(deftest (mini-meval let*)
    (mini-meval '(let ((x 3) (y 4) (z 5)) (let* ((z (+ x y)) (w z)) (list x y z w))) etat)
  '(3 4 7 7))

;; TODO
;; (deftest (mini-meval let-nil)
;;     (mini-meval '(let (a (x 3) y) (list a x y)) etat)
;;   '(nil 3 nil))

;; (deftest (mini-meval let-nil)
;;     (mini-meval '(let* ((x 4) y (z 5)) (list a x y)) etat)
;;   '(4 nil 5))

(deftest (mini-meval progn)
    (mini-meval '(progn 1 2 3 4) etat)
  4)

(deftest (mini-meval quote)
    (mini-meval ''x etat)
  'x)

(deftest (mini-meval macrolet)
    (mini-meval '(labels ((qlist (a b) (list a b)))
                  (list
                   (qlist 'a 'b)
                   (macrolet ((qlist (x y) (list 'list (list 'quote x) (list 'quote y))))
                     (qlist 'a 'b))
                   (qlist 'a 'b)))
                etat)
  '((a b) ('a 'b) (a b)))

(deftest (mini-meval setf setq)
    (mini-meval '(let ((x 42)) (list x (setq x 123) x) etat))
  '(42 123 123))

(deftest (mini-meval funcall)
    (mini-meval '(funcall #'+ 1 2 3) etat)
  '6)

(deftest (mini-meval apply)
    (mini-meval '(apply #'+ 1 2 (list (+ 1 2) 4)) etat)
  '10)

(deftest (mini-meval function external)
    (mini-meval '#'+ etat)
  #'+)

(deftest (mini-meval function external)
    (mini-meval '(funcall #'+ 1 2 3) etat)
  '6)

(deftest (mini-meval call-function external)
    (mini-meval '(#'+ 2 3) etat)
  5)

(deftest (mini-meval call-function lambda)
    (mini-meval '(#'(lambda (x) (+ x 40)) 2) etat)
  42)

(deftest (mini-meval lambda optional)
    (mini-meval '((lambda (x &optional (y 2)) (list x y)) 1) etat)
  '(1 2))

(deftest (mini-meval lambda closure single-instance)
    (mini-meval '(let ((foo (let ((y 1)) (cons (lambda (x) (list x y)) (lambda (z) (setq y (+ y z)) nil)))))
                  (list (funcall (car foo) 4) (funcall (cdr foo) 5) (funcall (car foo) 4))) etat)
  '((4 1) nil (4 6)))

(deftest (mini-meval lambda closure multiple-instances)
    (mini-meval '(labels ((counter (&optional (ctr 0)) (cons (lambda () ctr) (lambda (&optional (x 1)) (setq ctr (+ ctr x)) nil))))
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
                etat)
  '(0 42 nil 1 nil 43 1 43 nil nil 49 6))

(deftest (mini-meval labels)
    (mini-meval '(labels ((foo (x) (+ x 1)))
                  (list
                   (foo 3)
                   (labels ((foo (x) (+ x 3)))
                     (foo 3))))
                etat)
  '(4 6))

(deftest (mini-meval labels)
    (mini-meval '(< 2 3) etat)
  t)
 
(deftest (mini-meval flet)
    (mini-meval '(labels ((foo (x) (+ x 1)))
                  (list
                   (foo 3)
                   (flet ((foo (x) (+ x 3)))
                     (foo 3))))
                etat)
  '(foo 4 6))

(deftest (mini-meval labels)
    (mini-meval '(labels ((fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
                  (list
                   (fibo 5)
                   (labels ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
                     (fibo 5))
                   (fibo 5)))
                etat)
  '(fibo 8 5 8))

(deftest (mini-meval flet)
    (mini-meval '(labels ((fibo (n) (if (< n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 0 -> 1; 1 -> 1; 2 -> 2 ...
                  (list
                   (fibo 5)
                   (flet ((fibo (n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))) ;; fibo 1 -> 1; 2 -> 1; 3 -> 2 ...
                     (fibo 5))))
                etat)
  ;; Le flet ne permet pas les définitions récursives, donc le fibo
  ;; de l'extérieur est appellé après le 1er niveau de récursion.
  '(fibo 8 8))

(deftest (mini-meval tagbody)
  (mini-meval '(let ((x 0)) (tagbody foo (setq x 1) (go baz) bar (setq x 2) baz) x))
  1)

(deftest (mini-meval tagbody)
  (mini-meval '(let ((x 0)) (tagbody foo (setq x 1) (go 42) bar (setq x 2) 42) x))
  1)

(deftest (mini-meval tagbody)
  (mini-meval '(tagbody foo (list 1) 42 (list 2) baz (list 3)) etat)
  nil)

(deftest (mini-meval block)
  (mini-meval '(block foo 1 (return-from foo 4) 2))
  4)

(deftest (mini-meval block)
  (mini-meval '(block foo 1 2))
  2)


(provide 'equiv-tests)
