;;;;TEST;;;;
(let ()) ;=> nil

(let ((x)) x) ;=> nil

(let ((x 1 2)) x) ;=> error
(let ((x 100)) x) ;=> 100
x ;=> error


;;;;TEST;;;;
(setq y 100)
(let ((x 1)
      y
      (z 3))
      (list x y z)) ;=> (1 nil 3)
y ;=> 100
(let ((x 1)
      y
      (z 3))
      (setq y 2)
      (list x y z)) ;=> (1 2 3)
y ;=> 100

;;;;TEST;;;;
(let ((x 1)
      (y 2)
      (x 3))
  (list x y x)) ;=> (3 2 3)


;;;;TEST;;;;
(let* ()) ;=> nil

(let* ((x)) x) ;=> nil

(let* ((x 1 2)) x) ;=> error
(let* ((x 100)) x) ;=> 100
x ;=> error


;;;;TEST;;;;
(let* ((x 1)
       (y 2)
       (x 3))
  (list x y x)) ;=> (3 2 3)
