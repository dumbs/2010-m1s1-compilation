(defun my-format (destination control-string &rest arguments)
  (let ((current nil)
        (pos -1)
        (length 0)
        (stack '()))
    (labels ((get-char () (setq current (char control-string (setq pos (+ pos 1))))))
      (tagbody
       (setq length (length control-string))
       
       main
       (get-char)
       (when (char= current #\~)
         (go special))
       (push 'main-after-write stack)
       (go write)
       main-after-write
       (setq pos (+ pos 1))
       (when (>= pos length)
         (go end-of-string))
       (go main)

       special
       (error "niy")
       
       write
       (write-char current destination)
       (go return)
       
       return
       (case stack
         (main-after-write (go main-after-write)))
       
       end-of-string))))