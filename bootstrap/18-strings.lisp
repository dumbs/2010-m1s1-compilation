;; PRIMITIVE : stringp
;; NEED : aref
;; NEED : cond
;; NEED : characterp
;; NEED : symbolp

;; 18.1-string-access
;; 18.2-string-comparison
;; 18.3-string-construction-and-manipulation

(defun char (string index)
  (aref string index))

(defun schar (string index)
  (aref string index))

(defun string (stuff )
  "STUFF must be a string, symbol or character."
  (cond ((stringp stuff) stuff)
        ((characterp stuff)
         ;; TODO
         )
        ((symbolp stuff)
         ;; TODO
         )))

(defun string= (string1 string2 &key (:start1 0) :end1 (:start2 0) :end2)
  ;; TODO
  )

(defun make-string (size &key :initial-element)
  ;; TODO
  )