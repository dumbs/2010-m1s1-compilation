(loop for i from 10 downto 1 by 3 collect i) => (10 7 4 1)

(loop as i from 1 to 5 collect i) => (1 2 3 4 5)

(loop as i below 5 collect i) => (0 1 2 3 4)

(loop for item in '(1 2 3 4 5) collect item) => (1 2 3 4 5)

(loop for item in '(1 2 3 4 5) by #'cddr collect item) => (1 3 5)

(loop for (item . x) (t . fixnum) in '((A . 1) (B . 2) (C . 3)) unless (eq item 'B) sum x) => 4

(loop for sublist on '(a b c d) collect sublist) => ((A B C D) (B C D) (C D) (D))

(loop for (item) on '(1 2 3) collect item) => (1 2 3)

(loop for item in '(1 2 3) collect item) => (1 2 3)

(loop for item = 1 then (+ item 10) repeat 5 collect item) => (1 11 21 31 41)

(loop for x being each present-symbol of "COMMON-LISP-USER" collect x) => (TO BY FOUR ITEM PRESENT-SYMBOL BEING DOWNTO ODD SUBLIST FIND-MESSAGE FROM SUM ACROSS EACH REPEAT X BELOW ON AS IN ELSE THEN OF L END COLLECT J PORT I FOR D C B A)

(let ((stack '(a b c d e f))) (loop while stack for item = (length stack) then (pop stack) collect item)) => (6 A B C D E)

(loop for i fixnum from 3 when (oddp i) collect i while (< i 5)) => (3 5)

(loop for i from 0 to 10 never (> i 11)) => T

(loop for i from 0 thereis (when (> i 10) i)) => 11

(loop for i from 0 to 10 always (< i 11)) => T

(defstruct mountain height difficulty (why "because it is there")) 
(setq everest (make-mountain :height '(2.86e-13 parsecs))) 
(setq chocorua (make-mountain :height '(1059180001 microns)))
(defstruct desert area (humidity 0)) 
(setq sahara (make-desert :area '(212480000 square furlongs)))
(loop for x in (list everest sahara chocorua) thereis (and (mountain-p x) (mountain-height x))) => (2.86E-13 PARSECS)

(loop for i in '(1 2 3 stop-here 4 5 6) when (symbolp i) do (loop-finish) count i) => 3

(loop for i in '(1 2 3 stop-here 4 5 6) until (symbolp i) count i) => 3

(loop for name in '(fred sue alice joe june) 
      for kids in '((bob ken) () () (kris sunshine) ()) 
      collect name
      append kids) => (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE)

(loop for name in '(fred sue alice joe june)
      as age in '(22 26 19 20 10) 
      append (list name age) into name-and-age-list 
      count name into name-count 
      sum age into total-age 
      finally 
      (return (values (round total-age name-count)
		      name-and-age-list))) => 19 ;
(FRED 22 SUE 26 ALICE 19 JOE 20 JUNE 10)

(loop for i in '(bird 3 4 turtle (1 . 4) horse cat) 
      when (symbolp i) collect i) => (BIRD TURTLE HORSE CAT)

(loop for i from 1 to 10
      if (oddp i) collect i) => (1 3 5 7 9)

(loop for x in '((a) (b) ((c)))  append x) => (A B (C))

(loop for i upfrom 0 as x in '(a b (c)) nconc (if (evenp i) (list x) '())) => (A (C))

(loop for i in '(a b nil c nil d e) count i) => 5

(loop for i fixnum in '(1 2 3 4 5) sum i) => 15

(setq series '(1.2 4.3 5.7))
(loop for v in series sum (* 2.0 v)) => 22.4

(loop for i in '(2 1 5 3 4) maximize i) => 5

(loop for i in '(2 1 5 3 4) minimize i) => 1

(setq series '(1.2 4.3 5.7))
(loop for v in series maximize (round v) fixnum) => 6

(setq series '(1.2 4.3 5.7))
(loop for v float in series minimize (round v) into result fixnum finally (return result)) => 1

(loop with a = 1  
      with b = (+ a 2)  
      with c = (+ b 3) 
      with d = (+ c 4) 
      return (list a b c d)) => (1 3 6 10)

(loop with a = 1  
       and b = 2  
       and c = 3 
       and d = 4 
      return (list a b c d)) => (1 2 3 4)

(loop with a = 1  
      with b = (+ a 2)  
      with c = (+ b 3) 
      with d = (+ c 4) 
      return (list a b c d)) => (1 3 6 10)

(setq a 5 b 10 c 1729) 
(loop with a = 1 
       and b = (+ a 2) 
       and c = (+ b 3) 
       and d = (+ c 4) 
      return (list a b c d)) => (1 7 13 1733)

(loop for i in '(1 2 3 4 5 6) 
      when (and (> i 3) i) 
      collect it) => (4 5 6)

(loop for i in '(1 2 3 4 5 6) 
      when (and (> i 3) i) 
      return it) => 4

(loop for i in '(1 2 3 4 5 6) 
      thereis (and (> i 3) i)) => 4

(let ((printed nil)) (loop for (i j) in '((1 1) (2 4) (3 9)) do (push i printed)) printed) => (3 2 1)

(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4)) 
      for a integer = (first numlist) 
      and for b integer = (second numlist) 
      and for c float = (third numlist) 
      collect (list c b a)) => ((4.0 2 1) (8.3 6 5) (10.4 9 8))

(loop for (a b c) (integer integer float) in 
      '((1 2 4.0) (5 6 8.3) (8 9 10.4)) 
      collect (list c b a))) => ((4.0 2 1) (8.3 6 5) (10.4 9 8))

(loop for (a b c) float in 
      '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4)) 
      collect (list c b a)) => ((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0))

(loop with (a b) float = '(1.0 2.0) 
      and (c d) integer = '(3 4) 
      and (e f) 
      return (list a b c d e f)) => (1.0 2.0 3 4 NIL NIL)

(loop for (a nil b) = '(1 2 3) 
      do (return (list a b))) => (1 3)

(loop for (x . y) = '(1 . 2) 
      do (return y)) => 2

(loop for ((a . b) (c . d)) 
          of-type ((float . float) (integer . integer)) 
          in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6))) 
      collect (list a b c d)) => ((1.2 2.4 3 4) (3.4 4.6 5 6))

(loop for i from 1 to 10 
      when (> i 5) 
        collect i into number-list 
        and count i into number-count 
      finally (return (values number-count number-list))) => 5;
(6 7 8 9 10)

(loop named max 
      for i from 1 to 10 
      do (return-from max 'done)) => DONE




