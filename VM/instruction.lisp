;; "objet" VM.
;; Instanciation :
;;     (defvar vm (make-vm 100))
;; Appels de méthode :
;;     (send vm get-memory 42)
;;     (send vm set-memory 42 5)
;;     (send vm get-register R1)
;;     (send vm set-register R2 (send vm get-register 42))
(defun make-vm (size)
  (let* ((memory (make-array size :initial-element 0))
         (registres `(;; Registres généraux.
                      (R0 . 0)
                      (R1 . 0)
                      (R2 . 0)
                      ;; Base de la pile.
                      (BP . 0)
                      ;; Sommet de la pile.
                      (SP . 0)
                      ;; Pointeur de code : fin de la mémoire.
                      (PC . ,(- size 1))
                      ;; registres booléens = faux (nil).
                      (PP . nil)
                      (EQ . nil)
                      (PG . nil)
                      ;; Quand HALT passe à t, on arrête la machine.
                      (HALT . nil)))
         (actions `((get-memory . ,(lambda (index) (aref memory index)))
                    (set-memory . ,(lambda (index value) (setf (aref memeory index) value)))
                    (get-register . ,(lambda (reg) (cdr (assoc reg registres))))
                    (set-register . ,(lambda (reg) (setf (cdr (get-register reg))))))))
    (lambda (message &rest params)
      (apply (assoc message actions) params))))

(defun send (obj &rest params)
  (apply obj params))

(defun get-memory (vm index) (send vm get-memory index))
(defun set-memory (vm index value) (send vm get-memory index value))
(defun get-register (vm register) (send vm get-register register))
(defun set-register (vm register value) (send vm set-register register value))

(defun LOAD (vm address register)
  (set-register vm register (get-memory vm address)))

(defun STORE (vm register address)
  (set-memory vm address (get-register vm register)))

(defun MOVE (vm reg1 reg2)
  (set-register vm reg2 (get-register vm reg1)))

(defun _OP_ (vm op reg1 reg2)
  (set-register vm reg2 (funcall op
                                 (get-register vm reg2)
                                 (get-register vm reg1))))

(defun ADD  (vm reg1 reg2) (_OP_ vm #'+ reg1 reg2))
(defun SUB  (vm reg1 reg2) (_OP_ vm #'- reg1 reg2))
(defun MULT (vm reg1 reg2) (_OP_ vm #'* reg1 reg2))
(defun DIV  (vm reg1 reg2) (_OP_ vm #'/ reg1 reg2))

(defun INCR (vm register)
  (set-register vm register (+ (get-register vm register) 1)))

(defun DECR (vm register)
  (set-register vm register (- (get-register vm register) 1)))

(defun PUSH (vm register)
  (INCR vm 'SP)
  (STORE vm register (get-register vm 'SP)))

(defun POP (vm register)
  (LOAD vm (get-register vm 'SP) register)
  (DECR vm 'SP))

(defun JMP (vm dst)
  (set-register vm 'PC dst))

(defun JSR (vm dst)
  (PUSH vm 'PC)
  (JMP vm dst))

;; TODO : Remplir la fonction RTN
(defun RTN (vm)
  (POP vm 'PC))

(defun CMP (vm reg1 reg2)
  (set-register vm 'EQ (= (get-register vm reg1) (get-register vm reg2)))
  (set-register vm 'PP (< (get-register vm reg1) (get-register vm reg2)))
  (set-register vm 'PG (> (get-register vm reg1) (get-register vm reg2))))

(defun _JCOND_ (vm pp eq pg vm dst)
  (if (or (and eq (get-register vm 'EQ))
          (and pg (get-register vm 'PG))
          (and pp (get-register vm 'PP)))
      (JMP vm dst)))

(defun JEQ (vm dst)
  (_JCOND_ vm nil t nil vm dst))

(defun JPG (vm dst)
  (_JCOND_ vm nil nil t vm dst))

(defun JPP (vm dst)
  (_JCOND_ vm t nil nil vm dst))

(defun JPE (vm dst)
  (_JCOND_ vm t t nil vm dst))

(defun JPE (vm dst)
  (_JCOND_ vm nil t t vm dst))

(defun JNE (vm dst)
  (_JCOND_ vm t nil t vm dst))

(defun NOP (vm))

(defun HALT (vm)
  (set-register vm 'HALT t))
