
;;;;TEST;;;;
(setq g 0)
(defcomp foo ()
    (update (setq g (+ 1 g))))
(create foo)
g ;=> 0
(component_update_all)
g ;=> 1
(component_update_all)
g ;=> 2



;;;;TEST;;;;
(setq g1 0)
(setq g2 0)
(defcomp foo ()
    (update (setq g1 (+ 1 g1))
            (setq g2 (+ 1 g2))))
(create foo)
g1 ;=> 0
g2 ;=> 0
(component_update_all)
g1 ;=> 1
g2 ;=> 1
(component_update_all)
g1 ;=> 2
g2 ;=> 2

;;;;TEST;;;;
(setq x 10)
(setq y 20)
(defcomp foo ()
    (deflocal l x)
    (update (setq y l)))
(create foo)
x ;=> 10
y ;=> 20
(component_update_all)
x ;=> 10
y ;=> 10

;;;;TEST;;;;
(setq l 100)
(defcomp foo (x y)
    (deflocal x x)
    (deflocal y y)
    (deflocal z 10)
    (update (setq l (list x y z l))))
(create foo 1 2)
(component_update_all)
l ;=> (1 2 10 100)

;;;;TEST;;;;
(setq l 100)
(defcomp foo (x y)
    (deflocal z 10)
    (update (setq l (list x y z l))))
(create foo 1 2)
(component_update_all)
l ;=> (1 2 10 100)

;;;;TEST;;;;
(setq l 100)
(setq g nil)
(defcomp foo (x)
    (deflocal l x)
    (update (setq l (+ 1 l))
            (setq g (cons l g))))
(create foo 0)
(create foo 10)
(component_update_all)
g ;=> (11 1)
(component_update_all)
g ;=> (12 2 11 1)
(component_update_all)
g ;=> (13 3 12 2 11 1)

;;;;TEST;;;;
(setq g 0)
(defcomp foo (x)
    (deflocal l x)
    (update (setq g l)))
(create foo (+ 10 20))
(component_update_all)
g ;=> 30


;;;;TEST;;;;
;;ignore;;
(defcomp foo ()
  (deflocal x 10)
  (assert x 10)
  ) ;=> foo
(create foo) ;=> #<component foo>


;;;;TEST;;;;
(defcomp foo ()
  (deflocal baz 10)
  (receive (bar baz) (assert baz 11))) ;=> foo
(create foo) ;=> #<component foo>
(broadcast 'bar 11) ;=> nil

;;;;TEST;;;;
(defcomp foo ()
  (deflocal baz 10)
  (deflocal qux 11)
  (receive (bar baz) (assert baz 12) (assert qux 11))) ;=> foo
(create foo) ;=> #<component foo>
(broadcast 'bar 12) ;=> nil