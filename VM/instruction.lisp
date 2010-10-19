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
         (registers `(;; Registres généraux.
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
                    (set-memory . ,(lambda (index value) (setf (aref memory index) value)))
                    (get-register-list . ,(lambda () (mapcar #'car registers)))
                    (get-register . ,(lambda (reg) (cdr (assoc reg registers))))
                    (set-register . ,(lambda (reg value) (setf (cdr (assoc reg registers)) value)))
                    (size-memory . ,(lambda () (length memory))))))
    (lambda (message &rest params)
      (apply (cdr (assoc message actions)) params))))

(defun send (obj &rest params)
  (apply obj params))

(defun get-memory (vm index) (send vm 'get-memory index))
(defun set-memory (vm index value) (send vm 'set-memory index value))
(defun get-register-list (vm) (send vm 'get-register-list))
(defun get-register (vm register) (send vm 'get-register register))
(defun set-register (vm register value) (send vm 'set-register register value))
(defun size-memory (vm) (send vm 'size-memory))

;;TODO : Faire les registres
(defun dump-vm (vm)
  (dotimes (i (size-memory vm))
    (let ((val (get-memory vm i)))
      (format T "~&~8,'0x ~2,'0x ~3d ~a" i val val (isn-decode val))))
    (mapcar (lambda (reg)
              (let ((val (get-register vm reg)))
                (format T "~&~4a ~2,'0x ~3d" (string reg) val val)))
            (get-register-list vm))
    (let ((isn (get-memory vm (get-register vm 'PC))))
      (format T "~&Current instruction : ~2,'0x ~a" isn (isn-decode isn))))

;;TODO : Penser a ajouter une table des opcodes
(defun isn-decode (opcode)
  opcode)

(defun isn-encode (instruction)
  instruction)

(defun ISN-LOAD (vm address register)
  (set-register vm register (get-memory vm address)))

(defun ISN-STORE (vm register address)
  (set-memory vm address (get-register vm register)))

(defun ISN-MOVE (vm reg1 reg2)
  (set-register vm reg2 (get-register vm reg1)))

(defun ISN--OP- (vm op reg1 reg2)
  (set-register vm reg2 (funcall op
                                 (get-register vm reg2)
                                 (get-register vm reg1))))

(defun ISN-ADD  (vm reg1 reg2) (ISN--OP- vm #'+ reg1 reg2))
(defun ISN-SUB  (vm reg1 reg2) (ISN--OP- vm #'- reg1 reg2))
(defun ISN-MULT (vm reg1 reg2) (ISN--OP- vm #'* reg1 reg2))
(defun ISN-DIV  (vm reg1 reg2) (ISN--OP- vm #'/ reg1 reg2))

(defun ISN-INCR (vm register)
  (set-register vm register (+ (get-register vm register) 1)))

(defun ISN-DECR (vm register)
  (set-register vm register (- (get-register vm register) 1)))

(defun ISN-PUSH (vm register)
  (ISN-INCR vm 'SP)
  (ISN-STORE vm register (get-register vm 'SP)))

(defun ISN-POP (vm register)
  (ISN-LOAD vm (get-register vm 'SP) register)
  (ISN-DECR vm 'SP))

(defun ISN-JMP (vm dst)
  (set-register vm 'PC (- dst 1)))

(defun JSR (vm dst)
  (ISN-PUSH vm 'PC)
  (ISN-JMP vm dst))

(defun ISN-RTN (vm)
  (ISN-POP vm 'PC))

(defun ISN-CMP (vm reg1 reg2)
  (set-register vm 'EQ (= (get-register vm reg1) (get-register vm reg2)))
  (set-register vm 'PP (< (get-register vm reg1) (get-register vm reg2)))
  (set-register vm 'PG (> (get-register vm reg1) (get-register vm reg2))))

(defun ISN--JCOND- (pp eq pg vm dst)
  (if (or (and eq (get-register vm 'EQ))
          (and pg (get-register vm 'PG))
          (and pp (get-register vm 'PP)))
      (ISN-JMP vm dst)))

(defun ISN-JEQ (vm dst)
  (ISN--JCOND- nil t nil vm dst))

(defun ISN-JPG (vm dst)
  (ISN--JCOND- nil nil t vm dst))

(defun ISN-JPP (vm dst)
  (ISN--JCOND- t nil nil vm dst))

(defun ISN-JPE (vm dst)
  (ISN--JCOND- t t nil vm dst))

(defun ISN-JPE (vm dst)
  (ISN--JCOND- nil t t vm dst))

(defun ISN-JNE (vm dst)
  (ISN--JCOND- t nil t vm dst))

(defun ISN-NOP (vm)
  vm)

(defun ISN-HALT (vm)
  (set-register vm 'HALT t))


;;Test Unitaire
;; TODO : Faire deftestvar
(load "test-unitaire")
(defvar vm (make-vm (+ 10 (random 10))))
(defvar t-address (random (size-memory vm)))
(defvar t-value (random 42))
(set-memory vm t-address t-value)

(deftest virtual-machine
  (progn (ISN-LOAD vm t-address 'R0)
         (get-register vm 'R0))
  (get-memory vm t-address))

(setf t-address (random (size-memory vm)))
(deftest virtual-machine
  (progn (ISN-STORE vm 'R0 t-address)
         (get-memory vm t-address))
  (get-register vm 'R0))

(dump-vm vm)