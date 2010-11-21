;; TODO : remplacer les deux my-loop-finish par loop-finish à la fin.

(defun transform-loop (expr)
  (let* ((name nil)
         (acc (make-symbol "ACC"))
         (acc-tail (make-symbol "ACC-TAIL"))
         (first-sym (make-symbol "FIRST"))
         (variables nil)
         (all-variables nil)
         (result nil)
         (initialization nil)
         (loopbody nil)
         (loopbody-sym (make-symbol "LOOPBODY"))
         (finally nil)
         (finally-sym (make-symbol "FINALLY"))
         (loop-keywords '(named
                          with for as repeat
                          initially finally
                          collect append nconc sum count minimize maximize
                          while until
                          always never thereis
                          do return
                          doing
                          if when unless else end
                          and))
         (stack nil)
         (repeat-sym nil)
         (destination nil)
         (with-initial-values nil)
         (for-clause-type nil)
         (for-getter-fun nil)
         (for-initial-value nil)
         (for-step-fun nil)
         (for-end-predicate nil)
         (for-numeric-direction nil)
         (for-numeric-start nil)
         (for-numeric-end nil)
         (for-numeric-step nil)
         (for-numeric-limit nil)
         (for-initially-psetq nil)
         (for-initially-affect nil)
         (storage-sym nil)
         (vars-names nil)
         (get-vars-and-types-end-keywords nil)
         (destr-psetq nil)
         (left-destr nil)
         (right-destr nil)
         (destr-whole-sym (make-symbol "WHOLE")))
    (macrolet ((advance (x) `(setq ,x (cdr ,x))))
      (tagbody
       start
         (dbg 'start)
         (when (eq 'named (car expr))
           (if (and (consp (cdr expr)) (symbolp (cadr expr)))
               (setq name (cadr expr))
               (error "bootstrap : loop : expected a loop name but got ~w" (cadr expr))))
         ;;(go prologue)
       prologue
         (dbg 'prologue)
         (dbg expr)
         (when (endp expr) (go end-parse))
         (case (car expr)
           (with (go with))
           (for (go for))
           (as (go for))
           (repeat (go repeat))
           (initially (push 'prologue stack) (go initially))
           (finally (push 'prologue stack) (go finally))
           (otherwise (go main)))
         (go prologue)
       main
         (dbg 'main)
         (when (endp expr) (go end-parse))
         (case (car expr)
           (do (go do))
           (collect (go collect))
           (append (go append))
           (nconc (go nconc))
           (sum (go sum))
           (count (go count))
           (minimize (go minimize))
           (maximize (go maximize))
           (initially (push 'prologue stack) (go initially))
           (finally (push 'prologue stack) (go finally))
           (otherwise
            (when (member (car expr) loop-keywords) (error "bootstrap : loop : loop keyword ~w can't appear here." (car expr)))
            (error "bootstrap : invalid syntax in loop form : ~w." expr)))
         (go main)
         
       collect
         (advance expr)
         (if (endp expr) (error "bootstrap : loop : expected expression after collect but found the end of the loop form."))
         (if (member expr loop-keywords) (error "bootstrap : loop : loop keyword ~w can't appear here." (car expr)))
         (push `(if ,acc
                    (setq ,acc-tail (cdr (rplacd ,acc-tail (cons ,(car expr) nil))))
                    (setq ,acc-tail (setq ,acc (cons ,(car expr) nil))))
               loopbody)
         (advance expr)
         (go main)
       append
         (advance expr)
         (if (endp expr) (error "bootstrap : loop : expected expression after append but found the end of the loop form."))
         (if (member expr loop-keywords) (error "bootstrap : loop : loop keyword ~w can't appear here." (car expr)))
         (push `(if ,acc
                    (setq ,acc-tail (last (rplacd ,acc-tail (copy-list ,(car expr)))))
                    (setq ,acc-tail (last (setq ,acc (copy-list ,(car expr))))))
               loopbody)
         (advance expr)
         (go main)
       nconc
         (error "niy")
         (go main)
       sum
         (error "niy")
         (go main)
       count
         (error "niy")
         (go main)
       minimize
         (error "niy")
         (go main)
       maximize
         (error "niy")
         (go main)
         
       with
         (dbg 'with)
         (advance expr)
         (setq get-vars-and-types-end-keywords '(=))
         (push 'with-got-vars stack)
         (go get-vars-and-types)
       with-got-vars
         (setq with-initial-values nil)
         (when (eq '= (car expr))
           (advance expr)
           (setq with-initial-values (car expr))
           (advance expr))
       with-make-let
         (dbg 'with-make-let)
         (setq left-destr vars-names)
         (setq right-destr with-initial-values)
         (push 'end-with stack)
         (go destructuring-let)
       end-with
         (push variables all-variables)
         (push nil all-variables)
         (setq variables nil)
         (when (eq 'and (car expr))
           (go with))
         (go prologue)
         
       for
         (dbg 'for)
         (advance expr) ;; gobble for / and
         ;; (for vars in values)
         ;; (for vars on values)
         ;; (for vars = values [then expr])
         ;; (for vars across vector) ;; non implémenté
         ;; being : hash et package non supportés.
         ;; (for var [from/downfrom/upfrom expr1] [to/downto/upto/below/above expr2] [by expr3])
         (setq storage-sym (make-symbol "STORAGE-FOR"))
         (setq get-vars-and-types-end-keywords '(in on = across being from downfrom upfrom to downto upto below above by))
         (push 'for-got-vars stack)
         (go get-vars-and-types)
       for-got-vars
         (unless (member (car expr) '(in on = across being)) (go numeric-for))
         (setq for-clause-type (car expr))
       for-get-initial
         (advance expr)
         (when (endp expr) (error "bootstrap : loop : expected expression but found the end of the loop form."))
         (setq for-initial-value (car expr))
         (advance expr)
       for-select-clause-handler
         (case for-clause-type
           (in (go in-for))
           (on (go on-for))
           (= (go affect-then-for))
           (across (go vector-for))
           (being (go hash-package-for)))
         (error "bootstrap : loop : serious failure while parsing the for clause handler.")
         
       in-for
         (setq for-getter-fun `(car ,storage-sym))
         (go in-on-for)
         
       on-for
         (setq for-getter-fun storage-sym)
         (go in-on-for)
         
       in-on-for
         (setq for-step-fun `(cdr ,storage-sym))
         (setq for-end-predicate `(endp ,storage-sym))
         (when (eq 'by (car expr))
           (advance expr)
           (when (endp expr) (error "bootstrap : loop : expected expression after BY but found the end of the loop form."))
           (setq for-step-fun `(funcall ,(car expr) ,storage-sym))
           (advance expr))
         (go for-make-let)
         
       affect-then-for
         (setq for-getter-fun storage-sym)
         (setq for-step-fun storage-sym)
         (setq for-end-predicate t)
         (when (eq 'then (car expr))
           (advance expr)
           (when (endp expr) (error "bootstrap : loop : expected expression after BY but found the end of the loop form."))
           (setq for-step-fun (car expr))
           (advance expr))
         (go for-make-let)
         
       numeric-for
         (setq for-end-predicate t)
         (setq for-numeric-start 0)
         (setq for-numeric-step 1)
         (setq for-numeric-direction 0)
         (setq for-numeric-end 0)
         (when (member (car expr) '(from upfrom downfrom))
           (when (eq 'downfrom (car expr)) (setq for-numeric-direction -1))
           (when (eq 'upfrom (car expr)) (setq for-numeric-direction 1))
           (advance expr)
           (when (endp expr) (error "bootstrap : loop : expected expression after from but found the end of the loop form."))
           (setq for-initial-value (car expr))
           (advance expr))
         (when (member (car expr) '(to downto upto below above))
           (setq for-numeric-limit (car expr))
           (when (member (car expr) '(downto above))
             (unless (= for-numeric-direction 0)
               (error "bootstrap : loop : do you want to go up or down ? This is not a caroussel."))
             (setq for-numeric-direction -1))
           (when (member (car expr) '(upto below))
             (unless (= for-numeric-direction 0)
               (error "bootstrap : loop : do you want to go up or down ? This is not a caroussel."))
             (setq for-numeric-direction 1))
           (advance expr)
           (when (endp expr) (error "bootstrap : loop : expected expression after ~w but found the end of the loop form." for-numeric-limit))
           (setq for-numeric-end (car expr))
           (case for-numeric-limit
             (to (if (= for-numeric-direction -1)
                     (setq for-end-predicate `(<  (car ,storage-sym) (third ,storage-sym)))
                     (setq for-end-predicate `(>  (car ,storage-sym) (third ,storage-sym)))))
             (downto (setq for-end-predicate `(<  (car ,storage-sym) (third ,storage-sym))))
             (upto   (setq for-end-predicate `(>  (car ,storage-sym) (third ,storage-sym))))
             (below  (setq for-end-predicate `(>= (car ,storage-sym) (third ,storage-sym))))
             (above  (setq for-end-predicate `(<= (car ,storage-sym) (third ,storage-sym)))))
           (advance expr))
         (when (eq 'by (car expr))
           (advance expr)
           (when (endp expr) (error "bootstrap : loop : expected expression after by but found the end of the loop form."))
           (setq for-numeric-step (car expr))
           (advance expr))
         (setq for-initial-value `(list ,for-numeric-start ,for-numeric-step ,for-numeric-end))
         (if (= -1 for-numeric-direction)
             (setq for-step-fun `(cons (- (car ,storage-sym) (second ,storage-sym)) (cdr ,storage-sym)))
             (setq for-step-fun `(cons (+ (car ,storage-sym) (second ,storage-sym)) (cdr ,storage-sym))))
         (setq for-getter-fun `(car ,storage-sym))
         (go for-make-let)
         
       vector-for
         (error "bootstrap : loop : syntax (loop for var across vector) not implemented yet !")
         (go for-end)
         
       hash-package-for
         (error "bootstrap : loop : looping across hashes and packages is not implemented yet !")
         (go for-end)
         
       for-make-let
         (push `(,storage-sym nil) variables)
         (setq left-destr vars-names)
         (push 'for-make-initially-psetq stack)
         (go destructuring-empty-let)
         ;; (setq left-destr vars-names)
         ;; (setq right-destr `(funcall ,for-getter-fun (setq ,storage-sym ,for-initial-value)))
         ;; (push 'for-make-psetq stack)
         ;; (go destructuring-let)
       for-make-initially-psetq
         (push storage-sym for-initially-psetq)
         (push for-initial-value for-initially-psetq)
         (setq left-destr vars-names)
         (setq right-destr for-getter-fun)
         (psetq for-initially-affect destr-psetq destr-psetq for-initially-affect) ;; exchange
         (push 'for-make-body-psetq stack)
         (go destructuring-psetq)
       for-make-body-psetq
         (psetq destr-psetq for-initially-affect for-initially-affect destr-psetq) ;; re-exchange
         (unless (eq storage-sym for-step-fun)
           (push `(unless ,first-sym (setq ,storage-sym ,for-step-fun)) loopbody))
         (unless (eq t for-end-predicate)
           (push `(when ,for-end-predicate (go ,finally-sym)) loopbody))
         (setq left-destr vars-names)
         (setq right-destr for-getter-fun)
         (push 'for-end stack)
         (go destructuring-psetq)
       for-end
         (when (eq 'and (car expr)) (go for))
         (push variables all-variables)
         (push `((setq ,@(reverse for-initially-psetq))
                 (setq ,@(reverse for-initially-affect)))
               all-variables)
         (setq variables nil)
         (setq for-initially-psetq nil)
         (setq for-initially-affect nil)
         (push `(setq ,@(reverse destr-psetq)) loopbody)
         (setq destr-psetq nil)
         (go prologue)
         
       repeat
         (dbg 'repeat)
         (advance expr)
         (setq repeat-sym (make-symbol "REPEAT-COUNTER"))
         (push `((,repeat-sym ,(car expr))) all-variables)
         (push nil all-variables)
         (push `(setq ,repeat-sym (- ,repeat-sym 1)) loopbody)
         (push `(when (< ,repeat-sym 0) (go ,finally-sym)) loopbody)
         (advance expr)
         (go prologue)
         
       do
         (dbg 'do)
         (advance expr)
         (when (endp expr) (error "bootstrap : loop : expected an expression for DO, but encountered the end of the loop form."))
         (when (member (car expr) loop-keywords) (error "bootstrap : loop : keyword ~w can't appear here, expected expression for DO." (car expr)))
         (push (car expr) loopbody)
         (advance expr)
       do-loop
         (dbg 'do-loop)
         (when (endp expr) (go do-end))
         (when (member (car expr) loop-keywords) (go do-end))
         (push (car expr) loopbody)
         (advance expr)
         (go do-loop)
       do-end
         (go main)
         
         
       get-vars-and-types
         ;; params : get-vars-and-types-end-keywords
         ;; returns : vars-names, real-vars-names
         (dbg 'get-vars-and-types)
         ;; a [= 1] [and ...]
         ;; a type [= 1] [and ...]
         ;; a of-type type [= 1] [and ...]
         ;; (a b c) [= <list>] [and ...]
         ;; (a b c) (t1 t2 t3) [= <list>] [and ...]
         ;; (a b c) of-type (t1 t2 t3) [= <list>] [and ...]
         ;; (a b c) type [= <list>] [and ...]
         ;; (a b c) of-type type [= <list>] [and ...]
         (setq vars-names (car expr))
         (advance expr)
         (when (eq 'of-type (car expr))
           (advance expr)
           (when (endp expr) (error "Expected type after OF-TYPE, but found the end of the loop form."))
           (advance expr)
           (go get-vars-and-types-end))
         (unless (or (member (car expr) get-vars-and-types-end-keywords)
                     (member (car expr) loop-keywords))
           (advance expr))
       get-vars-and-types-end
         (go return)
         
       destructuring-let
         ;; params : left-destr right-destr
         ;; return : nothing
         ;; mutate : variables
         ;; modify : left-destr
         (dbg 'destructuring-let)
         
         ;; Cas sans destructuring
         (unless (consp left-destr)
           (push `(,left-destr ,right-destr) variables)
           (go destr-let-end))

         (push `(,(car left-destr) (car (setq ,destr-whole-sym ,right-destr))) variables)
         (advance left-destr)
       destr-let-loop
         (dbg 'destr-let-loop)
         (when (endp left-destr)
           (go destr-let-end))
         (when (atom left-destr)
           (push `(,left-destr ,destr-whole-sym) variables)
           (go destr-let-end))
         (push `(,(car left-destr) (car (setq ,destr-whole-sym (cdr ,destr-whole-sym)))) variables)
         (advance left-destr)
         (go destr-let-loop)
       destr-let-end
         (dbg 'destr-let-end)
         (go return)
         
       destructuring-psetq
         ;; params : left-destr right-destr
         ;; return : nothing
         ;; mutate : destr-psetq
         ;; modify : left-destr
         (dbg 'destructuring-psetq)
         
         ;; Cas sans destructuring
         (unless (consp left-destr)
           (push left-destr destr-psetq)
           (push right-destr destr-psetq)
           (go destr-psetq-end))
         
         (push (car left-destr) destr-psetq)
         (push `(car (setq ,destr-whole-sym ,right-destr)) destr-psetq)
         (advance left-destr)
       destr-psetq-loop
         (dbg 'destr-psetq-loop)
         (when (endp left-destr)
           (go destr-psetq-end))
         (when (atom left-destr)
           (push left-destr destr-psetq)
           (push destr-whole-sym destr-psetq)
           (go destr-psetq-end))
         (push (car left-destr) destr-psetq)
         (push `(car (setq ,destr-whole-sym (cdr ,destr-whole-sym))) destr-psetq)
         (advance left-destr)
         (go destr-psetq-loop)
       destr-psetq-end
         (dbg 'destr-psetq-end)
         (go return)
         
       destructuring-empty-let
         ;; params : left-destr
         ;; return : nothing
         ;; mutate : variables
         ;; modify : left-destr
         (dbg 'destructuring-empty-let)
         
         ;; Cas sans destructuring
         (unless (consp left-destr)
           (push `(,left-destr nil) variables)
           (go destr-empty-let-end))

         (push `(,(car left-destr) nil) variables)
         (advance left-destr)
       destr-empty-let-loop
         (when (endp left-destr)
           (go destr-empty-let-end))
         (when (atom left-destr)
           (push `(,left-destr nil) variables)
           (go destr-empty-let-end))
         (push `(,(car left-destr) nil) variables)
         (advance left-destr)
         (go destr-empty-let-loop)
       destr-empty-let-end
         (dbg 'destr-empty-let-end)
         (go return)
         
       initially
         (dbg 'initially)
         (advance expr)
         (when (endp expr) (error "bootstrap : loop : expected an expression for INITIALLY, but encountered the end of the loop form."))
         (when (member (car expr) loop-keywords) (error "bootstrap : loop : keyword ~w can't appear here, expected expression for INITIALLY." (car expr)))
         (push (car expr) initialization)
         (advance expr)
       initially-step
         (dbg 'initially-step)
         (when (endp expr) (go initially-end))
         (when (member (car expr) loop-keywords) (go initially-end))
         (push (car expr) initialization)
         (advance expr)
         (go initially-step)
       initially-end
         (dbg 'initially-end)
         (go return)
         
       finally
         (dbg 'finally)
         (advance expr)
         (when (eq 'return (car expr))
           (push `(return-from ,name ,(cadr expr)) finally)
           (advance expr)
           (advance expr)
           (go finally-end))
         (when (member (car expr) '(do doing))
           (advance expr))
         (when (endp expr) (error "bootstrap : loop : expected an expression for FINALLY, but encountered the end of the loop form."))
         (when (member (car expr) loop-keywords) (error "bootstrap : loop : keyword ~w can't appear here, expected expression for FINALLY." (car expr)))
         (push (car expr) finally)
         (advance expr)
         ;; (go finally-step)
       finally-step
         (dbg 'finally-step)
         (when (endp expr) (go finally-end))
         (when (member (car expr) loop-keywords) (go finally-end))
         (push (car expr) finally)
         (advance expr)
         (go finally-step)
       finally-end
         (dbg 'finally-end)
         (go return)
         
       return
         (dbg 'return)
         (when (endp stack) (error "bootstrap : loop : internal error : call stack is empty !"))
         (setq destination (car stack))
         (setq stack (cdr stack))
         (case destination
           (prologue (go prologue))
           (main (go main))
           (with-got-vars (go with-got-vars))
           (end-with (go end-with))
           (for-got-vars (go for-got-vars))
           (for-make-initially-psetq (go for-make-initially-psetq))
           (for-make-body-psetq (go for-make-body-psetq))
           (for-end (go for-end))
           (otherwise (error "bootstrap : loop : internal error : invalid return point on call stack : ~w" destination)))
         
       end-parse
         (dbg 'end-parse)
       make-body
         (dbg 'make-body)
         (setq finally `(progn ,@(reverse (cons `(return-from ,name ,acc) finally))))
         (setq initialization (reverse initialization))
         (setq loopbody (reverse loopbody))
         (setq result
               `(macrolet ((my-loop-finish () '(go ,finally-sym)))
                  (tagbody
                     (progn ,@initialization)
                   ,loopbody-sym
                     (progn ,@loopbody)
                     (setq ,first-sym nil)
                     (go ,loopbody-sym)
                   ,finally-sym
                     ,finally)))
       build-lets-loop
         (dbg 'build-lets-loop)
         (when (endp all-variables)
           (go build-block-and-let))
         (setq result `(let ,(reverse (cadr all-variables)) ,@(car all-variables) ,result))
         (advance all-variables)
         (advance all-variables)
         (go build-lets-loop)
       build-block-and-let
         (dbg 'build-block-and-lets)
         (setq result
               `(block ,name
                  (let ((,acc nil)
                        (,acc-tail nil)
                        (,destr-whole-sym nil)
                        (,first-sym t))
                    ,acc
                    ,acc-tail
                    ,destr-whole-sym
                    ,first-sym
                    ;; If you call loop-finish during variable declarations, and you use variables that haven't been initialized,
                    ;; then it will fail / use variables from the surrounding environment. But it's you freakin' problem if you do
                    ;; such bizarre things.
                    (macrolet ((my-loop-finish () ',finally))
                      ,result))))
       the-end
         ;; music
       rideau))
    result))

;; (eval (transform-loop '(for (i j) in '((1 1) (2 4) (3 9)) do (print i))))
;; (eval (transform-loop '(for (i j) in '((1 1) (2 4) (3 9)) for k from 0 to 5 do (format t "~&~a ~a ~a" i j k) initially (print 'i) finally (print 'f) (print i))))
;; (eval (transform-loop '(for i = 42 and j in (list 1 i 3) for k = i then (cons i j) collect (list i j k))))

#|
(loop (print 5))
=> boucle infinie

expansion : let, block et tagbody

(loop …)
expands into :
  <prologue>
  <body>
  <epilogue>

expands into :
(block nil …)
pour qu'on puisse faire (return …) et (return-from nil …)
(loop named X …)
=> (block X …)

do _*
initially _*
finally _*
Les autres ont une taille "fixe".

Attention, do peut être suivi de plusieurs tags et expressions. les tags sont comme ceux d'un tagbody, mais ne peuvent pas être des mots-clés de loop.

toutes les variables sont initialisées au début, même si elles sont déclarées au milieu
=> (block nil (let (vars) ...))

tous les initially sont rassemblés au début dans un progn, et tous les finally à la fin, avant le return implicite
=> (block nil
     (let (vars)
       (tagbody
        initialisation
          (progn initially1 i2 i3 ...)
        loopbody
          (...)
          (go loopbody)
        finally
          (progn finally1 f2 f3 ...)
        implicit-return
          (<implicit-return>))))

les "with …" créent des bindings chacun dans un let, leurs valeurs sont calculées dans l'ordre d'aparition des with.
(loop with a = b and c = d with e = f ...)
=> (let ((a b)
         (c d))
     (let ((e f))
       ...))
ou
=> (let* ((#:|a| b)
          (#:|c| d)
          (a #:|a|)
          (c #:|c|)
          (#:|e| f)
          (e #:|e|))
     ...)

"for …" ou "as …"
=> initialisation : set des valeurs initiales
=> loopbody : si on est à la fin de ce for, si oui, (go finally)
=>            si non, on exécute l'itération de ce for, et on stocke la valeur.

for x
=> for x = 0 then (+ x 1)

for x [up|down]from 5
=> for x = 0 then (+- x 1)

for x [up|down]from 5 [up|down]to/below/above 15 [by <step>]
=> itération + test

"repeat <n>"
=> initialisation d'une variable interne
=> test si on est à la fin de ce repeat, si oui, (go finally)
=> sinon, on incrémente cette variable interne.

"collect <expr> [into acc]"
(setf last-acc (setf (cdr last-acc) <expr>))
si acc est absent, on accumule sur l'accumulateur par défaut.

nconc, sum, count, minimize, maximize : voir la doc

|#