
;;;;TEST;;;;
() ;=> nil
(list) ;=> nil
(list 1) ;=> (1)
(list 1 2 3) ;=> (1 2 3)


;;;;TEST;;;;
(cons 1 2) ;=> (1 . 2)
(cons 1 nil) ;=> (1)
(cons 1 (cons 2 (cons 3 nil))) ;=> (1 2 3)
