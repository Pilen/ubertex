;;;;TEST;;;;
(if (progn (setq foo 1) t)
    (setq foo (+ foo 1))
  (setq foo (+ foo 10))
  (setq foo (+ foo 100))) ;=> 2
foo ;=> 2

;;;;TEST;;;;
(if (progn (setq foo 1) nil)
    (setq foo (+ foo 1))
  (setq foo (+ foo 10))
  (setq foo (+ foo 100))) ;=> 111
foo ;=> 111


;;;;TEST;;;;
(setq x nil)
(and (setq x (cons 1 x))
     (setq x (cons 2 x))
     (setq x (cons 3 x))) ;=> (3 2 1)
x ;=> (3 2 1)

(setq x nil)
(and (setq x (cons 1 x))
     (setq x (cons 2 x))
     nil
     (setq x (cons 3 x))) ;=> nil
x ;=> (2 1)


;;;;TEST;;;;
(setq x nil)
(or (setq x (cons 1 x))
    (setq x (cons 2 x))
    (setq x (cons 3 x))) ;=> (1)
x ;=> (1)

(setq x nil)
(or (setq x (cons 1 x))
    (setq x (cons 2 x))
    nil
    (setq x (cons 3 x))) ;=> (1)
x ;=> (1)

(setq x nil)
(or (progn (setq x (cons 1 x)) nil)
    (setq x (cons 2 x))
    nil
    (setq x (cons 3 x))) ;=> (2 1)
x ;=> (2 1)
