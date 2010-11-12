;; lisp2li simpliste pour le compilateur. On fusionnera les deux plus tard.

(defmatch lisp2cli)

(defmatch lisp2cli (:num . (? numberp)) `(:const . ,num))
(defmatch lisp2cli (:str . (? stringp)) `(:const . ,str))
(defmatch lisp2cli (quote :val _)       `(:const . ,val))
(defmatch lisp2cli ()                   `(:const . nil))
(defmatch lisp2cli (:name _ :params _*) `(:call ,name ,@(mapcar #'lisp2cli params)))
(defmatch lisp2cli (:x . _)             (error "Lisp2cli ne sait pas gérer : ~w" x))
