;; TODO : ne gère pas les échappements "foo\"bar" etc. ni les #...

(defun read (input-stream)
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
         (print 'start)
         (get-char)
         (push 'end stack)
         (go read-any)
         
       read-any
         (print 'read-any)
         (push 'end-read-any stack)
       read-any-loop
         (print 'read-any-loop)
         (print char)
         (cond
           ((not char)       (go end-of-file))
           ((char= char #\() (go read-list))
           ((char= char #\)) (error "Paren mismatch."))
           ((char= char #\') (go read-quote))
           ((char= char #\;) (go read-comment))
           ((char= char #\") (go read-string))
           ((char= char #\#) (go read-sharp))
           ((char= char #\`) (go read-backquote))
           ((char= char #\,) (go read-unquote))
           ((char= char #\ ) (get-char) (go read-any-loop))
           ((char= char #\newline) (get-char) (go read-any-loop))
           (t                (go read-symbol))) ;; \ and | and : fall into this.
       end-read-any
         (print 'end-read-any)
         (go return)
         
       read-list
         (print 'read-list)
         (push-result)
       read-list-loop
         (print 'read-list-loop)
         (get-char)
         (push 'read-list-loop-2 stack)
         (go read-any)
       read-list-loop-2
         (print 'read-list-loop-2)
         (unless (or (not char) (char= char #\)))
           (go read-list-loop))
         (when (not char)
           (error "EOF while reading a list"))
         (push (reverse result) (car result-stack))
         (pop-result)
         (go return)
         
       read-quote
         (print 'read-quote)
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
         (print 'read-symbol)
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
         
       read-sharp
         (error "niy")
         
       read-backquote
         (print 'read-quote)
         (push-result)
         (push-val 'quasiquote)
         (get-char)
         (go read-quotes-content)
         
       read-unquote
         (print 'read-quote)
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
         (print 'read-symbol)
         (push-result)
       read-symbol-loop
         (print 'read-symbol-loop)
         (push-val char)
         (get-char)
         ;; Pas le # : '(a#(1 2)) => '(|a#| (1 2)), pas '(a #(1 2))
         (unless (or (not char) (char-member char '(#\( #\) #\' #\; #\" #\` #\, #\  #\newline)))
           (go read-symbol-loop))
         (push (intern (format nil "~:@(~{~a~}~)" (reverse result))) (car result-stack))
         (pop-result)
         (format t "stack : ~a" stack)
         (go return)

       end-of-file
         (print 'eof)
         
       return
         (setq top-stack (car stack))
         (setq stack (cdr stack))
         (case top-stack
           (start (go start))
           (read-any (go read-any))
           (end-read-any (go end-read-any))
           (read-list (go read-list))
           (read-list-loop (go read-list-loop))
           (read-list-loop-2 (go read-list-loop-2))
           (read-quote (go read-quote))
           (end-read-quotes (go end-read-quotes))
           (read-symbol (go read-symbol))
           (read-symbol-loop (go read-symbol-loop))
           (end-of-file (go end-of-file))
           (end (go end))
           (otherwise (error "bootstrap : read : Invalid return point on the stack : ~w" top-stack)))
       end
         (print 'end)))
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
