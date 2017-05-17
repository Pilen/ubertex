;; THIS FILE SHOULD MATCH `component_deflocal.wlt' IN CONTENT.

;;;;TEST;;;;
(setq x 1) ;=> 1
x ;=> 1

;;;;TEST;;;;
(setq x 1
      y 2) ;=> 2
x ;=> 1
y ;=> 2

;;;;TEST;;;;
(setq x 1
      y) ;=> error
x ;=> error


;;;;TEST;;;;
(setq foo 1
      bar 2
      baz 3) ;=> 3
foo ;=> 1
bar ;=> 2
baz ;=> 3

(setq foo 10
      bar 20
      baz) ;=> error
foo ;=> 1
bar ;=> 2
baz ;=> 3


;;;;TEST;;;;
(setq) ;=> nil

;;;;TEST;;;;
(setq x nil)
(setq foo (setq x (cons 1 x))
      bar (setq x (cons 2 x))
      baz (setq x (cons 3 x))) ;=> (3 2 1)
foo ;=> (1)
bar ;=> (2 1)
baz ;=> (3 2 1)


;;;;TEST;;;;
(setq foo 0 bar 0)
(setq foo 1
      t 3
      bar 10)
foo ;=> 1
bar ;=> 0


;;;;TEST;;;;
(setq x 1) ;=> 1
(setq 2 (setq x 3)) ;=> error
x ;=> 3


;;;;TEST;;;;
(setq t 1) ;=> error
t ;=> t

;;;;TEST;;;;
(setq nil 1) ;=> error
nil ;=> nil
