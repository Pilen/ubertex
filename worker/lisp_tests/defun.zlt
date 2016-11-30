;;;;TEST;;;;
(defun foo (x) (+ x 10)) ;=> foo
(foo 20) ;=> 30

;;;;TEST;;;;
(defun foo () 10)
(foo) ;=> 10

;;;;TEST;;;;
(defun foo (x y) (+ x y 10))
(defun bar (z) (+ z (foo z 5)))
(bar 7) ;=> 29

;;;;TEST;;;;
(defun foo () (+ x y))
(defun bar (x y) (foo))
(bar 1 10) ;=> 11

;;;;TEST;;;;
(defun foo (a b) (list a b))
(foo 1 2 3 4) ;=> error
(foo) ;=> error
(foo 1) ;=> error

;;;;TEST;;;;
(defun foo (a &rest b) (list a b))
(foo 1 2 3 4 5) ;=> (1 (2 3 4 5))

;;;;TEST;;;;
(defun foo (&rest a b) (list a b))
(foo 1 2 3 4 5) ;=> ((1 2 3 4 5) nil)

;;;;TEST;;;;
(defun foo (&rest) 3)
(foo) ;=> 3

;;;;TEST;;;;
(defun foo (a b a) (list a b a))
(foo 1 2 3) ;=> (3 2 3)

;;;;TEST;;;;
(defun foo (a) (+ 1 a))
(defun bar (a) (+ (* 10 (foo a)) a))
(defun baz (a) (+ (* 100 (bar a)) a))
(baz 1) ;=> 2101

;;;;TEST;;;;
(defun foo (a a a) (list a a a))
(defun bar (a a a) (list (foo 4 5 6) a a a))
(bar 1 2 3) ;=> ((6 6 6) 3 3 3)


;;;;TEST;;;;
(defun foo (x)
  x)
(defun bar (x)
  (list (foo 1) x))
(defun baz (x)
  (list (bar 2) x))
(baz 3) ;=> ((1 2) 3)





;;;;TEST;;;;
(defun mlist (&rest l) l)
(mlist 1 2 3) ;=> (1 2 3)

(setq x (mlist 1 2 3)) ;=> (1 2 3)
(setq y (mlist 1 2 3)) ;=> (1 2 3)
(equal x y) ;=> t
(eq x y) ;=> nil
(mlist) ;=> nil


;;;;TEST;;;;
(defun my-list-atleast-2 (a b &rest l)
  (cons a (cons b l)))
(my-list-atleast-2 1 2 3 4) ;=> (1 2 3 4)
(my-list-atleast-2 1 2) ;=> (1 2)
(my-list-atleast-2) ;=> error
(my-list-atleast-2 1) ;=> error

;;;;TEST;;;;
(defun my-optional (a b &optional c &rest l)
  (cons a (cons b (cons c l))))
(my-optional 1 2 3 4 5) ;=> (1 2 3 4 5)
(my-optional 1 2 3) ;=> (1 2 3)
(my-optional 1 2) ;=> (1 2 nil)
(my-optional) ;=> error
(my-optional 1) ;=> error
