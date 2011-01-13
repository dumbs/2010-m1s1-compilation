'(;; r0 := 42
  (mov (constant 42) (register r0))
  
  ;; mem[500] := 42
  (mov (constant 42) (memory 500))
  
  ;; mem[500] := mem[4+r0]
  (mov (indexed 4 r0) (memory 500))
  
  ;; mem[4+r0] := mem[r0]
  (mov (indirect-register r0) (indexed 4 r0))
  
  ;; r1 := mem[mem[42]]
  (mov (indirect-constant 42) (register r1))
  
  ;; r1 := mem[mem[4+r0]]
  ;; TODO : ou bien mem[4+mem[r0]] ???
  (mov (indirect-indexed 4 r0) (register r1))
  
  ;; mem[mem[4+r0]] := r1
  (mov (register r1) (indirect-indexed 4 r0))
  
  ;; mem[sp] := r0
  (push (register r0))
  
  ;; Au chargement, mem[5152] := 112
  ;; db === define byte
  ;; Au lieu d'insérer l'opcode, on insère 112.
  (db 112) ;; Ici c'est l'adresse 5152

  ;; r0 := mem[@label42] + r0
  ;; identique à : r0 := mem[1234] + r0
  ;; variable est juste un nom "pratique"
  (add (memory (label variable 3)) r0)

  ;; Traduction de tout le bloc qui suit :
  ;; Le segment de code contient après chargement, avant exécution :
  ;; 0 : [call (memory 2)]
  ;; 1 : [0]
  ;; 2 : [add (memory 1) (register r0)]
  ;; 
  ;; 0 : [Opcode de call]
  ;; 1 : [Typecode de memory et nil]
  ;; 2 : [2] ;; 1er param
  ;; 3 : [rien, valeur au pif...] ;; 2e param
  ;; ====
  ;; 4 : [0]
  ;; ====
  ;; 5 : [Opcode de add]
  ;; 6 : [Typecode de memory et register]
  ;; 7 : [1]
  ;; 8 : [Regcode de r0]
  ;; 
  ;; Le tableau des labels est (séquenciellement) :
  ;; #(nil nil nil nil nil) ;; avant ligne 0
  ;; #(nil nil nil (0) nil) ;; call
  ;; #(nil nil 1 (0) nil)   ;; label
  ;; #(nil nil 1 (0) nil)   ;; db
  ;; #(nil nil 1 2 nil)     ;; label, on remplace à l'adresse 0
  ;; #(nil nil 1 2 nil)     ;; add
  (mov (memory (label variable 0)) (memory (label variable 1)))
;;  (call (memory (label function 3))) ;; adresse 0
  (label variable 2)                 ;; vide
  (db (constant 0))                  ;; adresse 1
  (label function 3)                 ;; vide
  (add (memory (label variable 2)) (register r0))) ;; adresse 2

Pour les labels : Un seul tableau :
#(nil
  nil
  (56 12 1)
  1234
  nil
  nil)

Utilisation d'un (label x) :
(defun get-label-position (x)
  (if (listp (aref les-labels x))
      (progn (push adresse-courante (aref les-labels x))
             0)
    (aref les-labels x)))

(defun set-label-position (x pos)
  (when (numberp (aref les-labels x))
    (error "Label ~a a déjà été défini !" x))
  ;; "réparer" les références en avant
  (dolist (i (aref les-labels x))
    (setf (aref memory i) pos))
  ;; définir pour les utilisations futures (ref arrières).
  (setf (aref les-labels x) pos))