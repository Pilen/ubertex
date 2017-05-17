
;;;;TEST;;;;
() ;=> nil
(list) ;=> nil
(list 1) ;=> (1)
(list 1 2 3) ;=> (1 2 3)


;;;;TEST;;;;
(cons 1 2) ;=> (1 . 2)
(cons 1 nil) ;=> (1)
(cons 1 (cons 2 (cons 3 nil))) ;=> (1 2 3)


;;;;TEST;;;;
(car '(1 2 3) 1) ;=> 1
(car (cons 1 2)) ;=> 1
;; Not the same as Emacs lisp:
(car nil) ;=> error

;;;;TEST;;;;
(cdr '(1 2 3) 1) ;=> (2 3)
(cdr (cons 1 2)) ;=> 2
;; Not the same as Emacs lisp:
(cdr nil) ;=> error

;;;;TEST;;;;
(nth 0 '(1 2 3)) ;=> 1
(nth 1 '(1 2 3)) ;=> 2
(nth 2 '(1 2 3)) ;=> 3
(nth 3 '(1 2 3)) ;=> error
(nth -1 '(1 2 3)) ;=> error
(nth 1.0 '(1 2 3)) ;=> error

;;;;TEST;;;;
(nth '(1 2 3) 0) ;=> 1
(nth '(1 2 3) 1) ;=> 2
(nth '(1 2 3) 2) ;=> 3
(nth '(1 2 3) 3) ;=> error
(nth '(1 2 3) -1) ;=> error
(nth '(1 2 3) 1.0) ;=> error
