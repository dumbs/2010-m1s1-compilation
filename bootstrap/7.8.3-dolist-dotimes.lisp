(defmacro my-dolist (spec &rest body)
  (let ((var (car spec))
        (listform (cadr spec))
        (resultform (caddr spec))
        (loopsym (make-symbol "loop"))
        (endsym (make-symbol "end"))
        (listsym (make-symbol "list")))
    `(let ((,var nil)
           (,listsym ,listform))
       (tagbody
        ,loopsym
          (setq ,var (car ,listsym))
          (when (endp ,listsym)
            (go ,endsym))
          (progn ,@body)
          (setq ,listsym (cdr ,listsym))
          (go ,loopsym)
        ,endsym)
       ,resultform)))

;; (let ((foo 42)) (my-dolist (a '(1 2 3) foo) (print a)))
;; => 1
;; => 2
;; => 3
;; => 42
;; (my-dolist (a '(1 2 3)) (print a))
;; => 1
;; => 2
;; => 3
;; => nil
;; (my-dolist (a '(1 2 3) a) (print a))
;; => 1
;; => 2
;; => 3
;; => nil
;; (my-dolist (a '()) (print a))
;; => nil

(defmacro my-dotimes (spec &rest body)
  (let ((var (car spec))
        (countform (cadr spec))
        (resultform (caddr spec))
        (loopsym (make-symbol "loop"))
        (endsym (make-symbol "end"))
        (countersym (make-symbol "counter"))
        (maxsym (make-symbol "max")))
    `(let ((,var nil)
           (,maxsym ,countform)
           (,countersym 0))
       (tagbody
        ,loopsym
          (setq ,var ,countersym)
          (when (>= ,countersym ,maxsym)
            (go ,endsym))
          (progn ,@body)
          (setq ,countersym (+ ,countersym 1))
          (go ,loopsym)
        ,endsym
          (when (< 0 ,var) (setq ,var 0)))
       ,resultform)))

;; (my-dotimes (i 3) (print i))
;; => 0
;; => 1
;; => 2
;; => nil
;; (my-dotimes (i 3 i) (print i))
;; => 0
;; => 1
;; => 2
;; => 3
;; (my-dotimes (i -5 i) (print i))
;; => 0
;; (my-dotimes (i -5) (print i))
;; => nil
