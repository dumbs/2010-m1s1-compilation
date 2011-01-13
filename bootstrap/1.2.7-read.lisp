;; TODO : ne gère pas les échappements "foo\"bar" etc. ni les #...

(defun my-read (input-stream)
  (let ((result-stack '())
        (result nil)
        (char nil)
        (stack '())
        (top-stack nil))
    (labels ((char-member (char chars)
               (some (lambda (c) (char= char c)) chars))
             (pop-result ()
               (setq result (car result-stack))
               (setq result-stack (cdr result-stack)))
             (push-val (val)
               (push val result))
             (push-result ()
               (push result result-stack)
               (setq result nil))
             (get-char ()
               (setq char (read-char input-stream nil nil))))
      (tagbody
       start
         (get-char)
         (push 'end stack)
         (go read-any)
         
       read-any
         (push 'end-read-any stack)
       read-any-loop
         (cond
           ((not char)       (go end-of-file))
           ((char= char #\() (go read-list))
           ((char= char #\)) (error "Paren mismatch. Stack :~&~a" stack))
           ((char= char #\') (go read-quote))
           ((char= char #\;) (push 'read-any-loop stack) (go read-comment))
           ((char= char #\") (go read-string))
           ((member char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'char=) (go read-number))
           ((char= char #\#) (go read-sharp))
           ((char= char #\`) (go read-backquote))
           ((char= char #\,) (go read-unquote))
           ((char= char #\ ) (get-char) (go read-any-loop))
           ((char= char #\newline) (get-char) (go read-any-loop))
           (t                (go read-symbol))) ;; \ and | and : fall into this.
       end-read-any
         (go return)
         
       read-list
         (push-result)
         (get-char)
       read-list-loop
         (when (or (char= char #\ ) (char= char #\newline))
           (get-char)
           (go read-list-loop))
         (when (or (not char) (char= char #\)))
           (go end-read-list-loop))
         (push 'read-list-loop stack)
         (go read-any)
       end-read-list-loop
         (when (not char)
           (error "EOF while reading a list"))
         (get-char)
         (push (reverse result) (car result-stack))
         (pop-result)
         ;(get-char)
         (go return)
         
       read-quote
         (push-result)
         (push-val 'quote)
         (get-char)
         (go read-quotes-content)

       read-quotes-content
         (push 'end-read-quotes stack)
         (go read-any)
       end-read-quotes
         (push (reverse result) (car result-stack))
         (pop-result)
         (go return)

       read-comment
       read-comment-loop
         (get-char)
         (unless (or (not char) (char= char #\newline))
           (go read-comment-loop))
         (unless (not char)
           (get-char))
         (go return)
         
       read-string
         (get-char)
         (push-result)
         (go read-string-loop-start)
       read-string-loop
         (push-val char)
         (get-char)
       read-string-loop-start
         (unless (or (not char) (char= char #\"))
           (go read-string-loop))
         (when (not char)
           (error "End of file while reading string."))
         (get-char)
         (push (format nil "~{~a~}" (reverse result)) (car result-stack))
         (pop-result)
         (go return)

       read-number
         (push 'end-read-number stack)
         (go read-symbol)
       end-read-number
         (setf (car result) (parse-integer (string (car result))))
         (go return)
         
       read-sharp
         (get-char)
         (cond
          ((char= char #\') (go read-quote-function))
          ((char= char #\\) (go read-sharp-char))
          (t (error "bootstrap : read : niy : syntax #~a not implemented yet." char)))
         
       read-quote-function
         (push-result)
         (push-val 'function)
         (get-char)
         (go read-quotes-content)

       read-sharp-char
         (get-char)
         (push 'end-read-sharp-char stack)
         (go read-symbol)
         end-read-sharp-char
         (case (car result)
           (newline (setf (car result) #\newline))
           (otherwise (setf (car result) (char (string (car result)) 0))))
         (go return)
         
       read-backquote
         (push-result)
         (push-val 'quasiquote)
         (get-char)
         (go read-quotes-content)
         
       read-unquote
         (push-result)
         (get-char)
         (cond ((char= char #\@)
                (get-char)
                (push-val 'unquote-splice))
               ((char= char #\.)
                (get-char)
                (push-val 'unquote-destructive-splice))
               (t
                (push-val 'unquote)))
         (go read-quotes-content)
         
       read-symbol
         (push-result)
       read-symbol-loop
         (push-val char)
         (get-char)
         ;; Pas le # : '(a#(1 2)) => '(|a#| (1 2)), pas '(a #(1 2))
         (unless (or (not char) (char-member char '(#\( #\) #\' #\; #\" #\` #\, #\  #\newline)))
           (go read-symbol-loop))
         (push (intern (format nil "~:@(~{~a~}~)" (reverse result))) (car result-stack))
         (pop-result)
         (go return)

       end-of-file
         (error "End of file not expected here !")
         
       return
         (setq top-stack (car stack))
         (setq stack (cdr stack))
         (case top-stack
           (start (go start))
           (read-any (go read-any))
           (read-any-loop (go read-any-loop))
           (end-read-any (go end-read-any))
           (read-list (go read-list))
           (read-list-loop (go read-list-loop))
           (read-quote (go read-quote))
           (end-read-quotes (go end-read-quotes))
           (end-read-sharp-char (go end-read-sharp-char))
           (end-read-number (go end-read-number))
           (read-symbol (go read-symbol))
           (read-symbol-loop (go read-symbol-loop))
           (end-of-file (go end-of-file))
           (end (go end))
           (otherwise (error "bootstrap : read : Invalid return point on the stack : ~w" top-stack)))
       end))
    (car result)))

;; (my-read (make-string-input-stream "foo"))
;; (my-read (make-string-input-stream "'foo"))
;; (my-read (make-string-input-stream "(foo)"))
;; (my-read (make-string-input-stream "(foo bar)"))
;; (my-read (make-string-input-stream "(foo bar baz)"))
;; (my-read (make-string-input-stream "'(foo)"))
;; (my-read (make-string-input-stream "'(foo bar baz)"))

;; (my-read (make-string-input-stream "`(,foo ,@bar ,.baz 'quux blop)"))

;; (my-read (make-string-input-stream "'(foo bar;;quux
;;  baz)"))

;; (my-read (make-string-input-stream "'(foo bar;;quux aa
;;  baz \"buz\" 'moo)"))

;; (my-read (make-string-input-stream "'(foo bar;;quux aa
;;  (baz #\\y \"buz\") 'moo)"))

(my-read (make-string-input-stream "(list '(+ 2 3))"))