
;; TODO : Penser a cree une fonction qui initialise un tableau en fonction d'un parametre.
(setq memory (make-array 100))

(defun getValueInMemory (index)
  (aref memory index))

(defun setValueInMemory (index value)
  (setf (aref memory index) value))


;;On initialise les registres a rien (peut etre faut il mettre 0 ?)
(setf R0 nil)
(setf R1 nil)
(setf R2 nil)

;; On initialise le registre de pointeur de base a l'indice 0 du tableau memory
(setf BP 0)
;; On initialise le registre de pointeur de pile a la valeur du pointeur de base
(setf SP BP)
;; On initialise le registre du compteur ordinale a la taille du tableau - 1
;; TODO : Recupere la taille du tableau dynamiquement.
(setf PC 99)

;; On initialise les registres booleen a faux (nil)
(setf PP nil)
(setf EQ nil)
(setf PG nil)

(defun LOAD (address register)
  (setf register (getValueInMemory address)))

(defun STORE (register address)
  (setValueInMemory address register))

;; TODO : Remplir la fonction MOVE
(defun MOVE (reg1 reg2)
  )

(defun ADD (reg1 reg2)
  (setf reg2 (+ reg2 reg1)))

(defun SUB (reg1 reg2)
  (setf reg2 (- reg2 reg1)))

(defun MULT (reg1 reg2)
  (setf reg2 (* reg2 reg1)))

(defun DIV (reg1 reg2)
  (setf reg2 (/ reg2 reg1)))

(defun INCR (register)
  (setf register (+ register 1)))

(defun DECR (register)
  (setf register (- register 1)))

(defun PUSH (register)
  (INCR SP)
  (STORE register SP))

(defun POP (register)
  (LOAD SP register)
  (DECR SP))

;; TODO : Remplir la fonction JMP
(defun JMP (dst)
  )

;; TODO : Remplir la fonction JSR
(defun JSR (dst)
  )

;; TODO : Remplir la fonction RTN
(defun RTN ()
  )

(defun CMP (reg1 reg2)
  (cond ((= (getValueInMemory reg1) (getValueInMemory reg2))
         (setf EQ T)
         (setf PP nil)
         (setf PG nil))
        ((< (getValueInMemory reg1) (getValueInMemory reg2))
         (setf EQ nil)
         (setf PP T)
         (setf PG nil))
        (T
         (setf EQ nil)
         (setf PP nil)
         (setf PG T))))

(defun JEQ (label)
  (if EQ
      (JMP label))
  )

(defun JPG (label)
  (if PG
      (JMP label))
  )

(defun JPP (label)
  (if PP
      (JMP label))
  )

(defun JPE (label)
  (if (or PP EQ)
      (JMP label))
  )

(defun JGE (label)
  (if (or PG EQ)
      (JMP label))
  )

(defun JNE (label)
  (if (not EQ)
      (JMP label))
  )

;; TODO : Remplir la fonction NOP
(defun NOP ()
  )

;; TODO : Remplir la fonction HALT
(defun HALT ()
  )
