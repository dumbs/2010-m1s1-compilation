;; NEED : stringp
;; NEED : string=

(defun equal (a b)
  (cond ((and (stringp a) (stringp b))
         (string= a b))
        (t
         nil)))