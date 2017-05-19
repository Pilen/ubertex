
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
(setq x (list 1 2 3)) ;=> (1 2 3)
(setcar x 10) ;=> 10
x ;=> (10 2 3)

;;;;TEST;;;;
(setq x (cons 1 2)) ;=> (1 . 2)
(setcar x 10) ;=> 10
x ;=> (10 . 2)

;;;;TEST;;;;
(setq x (list 1 2 3)) ;=> (1 2 3)
(setcdr x 10) ;=> 10
x ;=> (1 . 10)

;;;;TEST;;;;
(setq x (cons 1 2)) ;=> (1 . 2)
(setcdr x 10) ;=> 10
x ;=> (1 . 10)
