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
                    (set-memory . ,(lambda (index value) (setf (aref memory index) value)))
                    (get-register . ,(lambda (reg) (cdr (assoc reg registres))))
                    (set-register . ,(lambda (reg value) (setf (cdr (assoc reg registres)) value))))))
    (lambda (message &rest params)
      (apply (assoc message actions) params))))

(defun send (obj &rest params)
  (apply obj params))

(defun get-memory (vm index) (send vm 'get-memory index))
(defun set-memory (vm index value) (send vm 'get-memory index value))
(defun get-register (vm register) (send vm 'get-register register))
(defun set-register (vm register value) (send vm 'set-register register value))

(defun ISN_LOAD (vm address register)
  (set-register vm register (get-memory vm address)))

(defun ISN_STORE (vm register address)
  (set-memory vm address (get-register vm register)))

(defun ISN_MOVE (vm reg1 reg2)
  (set-register vm reg2 (get-register vm reg1)))

(defun ISN__OP_ (vm op reg1 reg2)
  (set-register vm reg2 (funcall op
                                 (get-register vm reg2)
                                 (get-register vm reg1))))

(defun ISN_ADD  (vm reg1 reg2) (ISN__OP_ vm #'+ reg1 reg2))
(defun ISN_SUB  (vm reg1 reg2) (ISN__OP_ vm #'- reg1 reg2))
(defun ISN_MULT (vm reg1 reg2) (ISN__OP_ vm #'* reg1 reg2))
(defun ISN_DIV  (vm reg1 reg2) (ISN__OP_ vm #'/ reg1 reg2))

(defun ISN_INCR (vm register)
  (set-register vm register (+ (get-register vm register) 1)))

(defun ISN_DECR (vm register)
  (set-register vm register (- (get-register vm register) 1)))

(defun ISN_PUSH (vm register)
  (ISN_INCR vm 'SP)
  (ISN_STORE vm register (get-register vm 'SP)))

(defun ISN_POP (vm register)
  (ISN_LOAD vm (get-register vm 'SP) register)
  (ISN_DECR vm 'SP))

(defun ISN_JMP (vm dst)
  (set-register vm 'PC dst))

(defun JSR (vm dst)
  (ISN_PUSH vm 'PC)
  (ISN_JMP vm dst))

(defun ISN_RTN (vm)
  (ISN_POP vm 'PC))

(defun ISN_CMP (vm reg1 reg2)
  (set-register vm 'EQ (= (get-register vm reg1) (get-register vm reg2)))
  (set-register vm 'PP (< (get-register vm reg1) (get-register vm reg2)))
  (set-register vm 'PG (> (get-register vm reg1) (get-register vm reg2))))

(defun ISN__JCOND_ (pp eq pg vm dst)
  (if (or (and eq (get-register vm 'EQ))
          (and pg (get-register vm 'PG))
          (and pp (get-register vm 'PP)))
      (ISN_JMP vm dst)))

(defun ISN_JEQ (vm dst)
  (ISN__JCOND_ nil t nil vm dst))

(defun ISN_JPG (vm dst)
  (ISN__JCOND_ nil nil t vm dst))

(defun ISN_JPP (vm dst)
  (ISN__JCOND_ t nil nil vm dst))

(defun ISN_JPE (vm dst)
  (ISN__JCOND_ t t nil vm dst))

(defun ISN_JPE (vm dst)
  (ISN__JCOND_ nil t t vm dst))

(defun ISN_JNE (vm dst)
  (ISN__JCOND_ t nil t vm dst))

(defun ISN_NOP (vm)
  vm)

(defun ISN_HALT (vm)
  (set-register vm 'HALT t))
