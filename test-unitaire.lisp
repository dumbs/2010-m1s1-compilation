;; TODO : exploser tout ça en plein de petites fonctions, c'est trop gros...
;; Mutation cons.
(defvar all-tests nil "Liste de tous les tests")

(defmacro deftest (module test expected)
  `(progn
     (if (not (assoc ',module all-tests))
	 (setf all-tests (cons '(,module . nil) all-tests)))
     (setf (cdr (assoc ',module all-tests))
	   (cons
	    (lambda ()
	      (let ((res ,expected))
		(if (equal ,test res)
		    (progn
		      (format t "~&    [SUCCESS] ~w~&" ',test)
		      t)
		  (progn
		    (format t "~&    [FAILURE] Test     : ~w~&"
			    ',test)
		    (format t "~&              got      : ~w~&"
			    res)
		    (format t "~&              expected : ~w~&"
			    ',expected)
		    nil))))
	    (cdr (assoc ',module all-tests))))))

(defmacro run-tests (&rest modules)
  (if (or (not modules)
	  (eq (car modules) t))
      `(real-run-tests ',(mapcar #'car all-tests))
  `(real-run-tests ',modules)))

(defun real-run-tests (modules)
  (mapcar (lambda (module)
	    (if (member (car module) modules)
		(mapcar (cdr module) #'funcall)))
	  all-tests
  )

(let ((tests nil))
  (defmacro deftest (module test expected)
    (if (not (assoc module tests))
        (setf tests (cons `(,module . (() . ())) tests)))
    (let ((mod-tests (assoc module tests)))
      (setf (cddr mod-tests)
            (cons (cons test expected)
                  (cddr mod-tests))))
    nil)
  (defmacro run-test (&rest modules)
    (let ((failures 0)
          (modules (if (eq T (car modules))
                       (mapcar #'car tests)
                               modules)))
      (if (every
		   (lambda (mod)
			 (if (member (car mod) modules)
				 (progn
				   (format t "~&Module ~w :~&" (car mod))
				   (let ((vars (cadr mod)))
					 (mapcar (lambda (test)
							   (let* ((res (eval `(let ,vars ,(car test))))
									  (expect (eval `(let ,vars ,(cdr test)))))
								 (if (equal expect res)
									 (format t "~&    [SUCCESS] ~w~&" (car test))
								   (progn (format t "    [FAILURE] Test     : ~w~&              got      : ~w~&              expected : ~w~&" (car test) res expect)
										  (setf failures (+ failures 1))))))
							 (reverse (cddr mod))))))
			 (if (not (= failures 0))
				 (format t "Module ~w failed ~w tests. Stopping.~&" (car mod) failures))
			 (= failures 0))
		   tests)
          (progn (format t "All modules passed all tests successfully.~&")
                 t)
          nil)))
  (defun show-test ()
    tests)
  (defmacro deftestvar (module nom valeur)
    (if (not (assoc module tests))
        (setf tests (cons `(,module . (() . ())) tests)))
    (let ((mod-vars (assoc module tests)))
      (setf (cadr mod-vars)
            (cons (list nom valeur)
                  (cadr mod-vars))))
	nil))

;; Test de debugage du test unitaire
;(deftest environnement nil nil)
;(deftest environnement (eq 42 42) T) ;; Test qui fail
;(deftest vm T T)
;(deftest environnement2 (eq 42 42) nil)
;(show-test)
;(run-test environnement)
;; TODO : every ne mappe pas la liste dans le bon ordre, et vm est
;; exécuté avant environnement quel que soit l'ordre des paramètres.
;(run-test environnement vm)
;(run-test environnement vm environnement2)
;(run-test t) ;; t => tous
