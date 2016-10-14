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
(let ()) ;=> nil

(let ((x)) x) ;=> nil

(let ((x 1 2)) x) ;=> error
(let ((x 100)) x) ;=> 100
x ;=> error

;;;;TEST;;;;
(let* ()) ;=> nil

(let* ((x)) x) ;=> nil

(let* ((x 1 2)) x) ;=> error
(let* ((x 100)) x) ;=> 100
x ;=> error

;;;;TEST;;;;
(let ((x 1)
      (y 2)
      (x 3))
  (list x y x)) ;=> (3 2 3)

;;;;TEST;;;;
(let* ((x 1)
       (y 2)
       (x 3))
  (list x y x)) ;=> (3 2 3)
