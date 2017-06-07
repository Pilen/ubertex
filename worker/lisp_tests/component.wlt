
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


;;;;TEST;;;;
(defcomp foo (x x)
  (assert x 100)) ;=> foo
(create foo 10 100) ;=> #<component foo>

;;;;TEST;;;;
(defcomp foo (x x)
  (deflocal x 1000)
  (assert x 1000)) ;=> foo
(create foo 10 100) ;=> #<component foo>



;;;;TEST;;;;
(defcomp foo ()
  (update (print 3)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> 3nil

;;;;TEST;;;;
(defcomp foo ()
  (update (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> errornil

;;;;TEST;;;;
(defcomp foo ()
  (update (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> errornil
(setq x 3) ;=> 3
(component_update_all) ;=> 3nil

;;;;TEST;;;;
(defcomp foo ()
  (deflocal y 10)
  (update (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> errornil
(setq x 3) ;=> 3
(component_update_all) ;=> 3nil

;;;;TEST;;;;
(defcomp foo ()
  (deflocal x 10)
  (update (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> 10nil
(setq x 3) ;=> 3
(component_update_all) ;=> 10nil

;;;;TEST;;;;
(defcomp foo ()
  (update error (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> errornil
(setq x 3) ;=> 3
(component_update_all) ;=> 3nil










;;;;TEST;;;;
(defcomp foo ()
  (deflocal x 10)
  (update
   (funcall xe)
   (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> 10nil
(setq frames (* (/ 1.0 30.0) 3))
(setq xe (ease 'x 200 frames)) ;=> error
(component_update_all) ;=> 10nil

;;;;TEST;;;;
;; Wow, this is hackish
(defcomp foo ()
  (deflocal x 10)
  (update
   (funcall xe)
   (print x)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> 10nil
(setq frames (* (/ 1.0 30.0) 3))
(setq xe (let ((x 100)) (ease 'x 200 frames)))
(component_update_all) ;=> 100nil
(force_frame)
(component_update_all) ;=> 133nil
(force_frame)
(component_update_all) ;=> 167nil
(force_frame)
(component_update_all) ;=> 200nil
(force_frame)
(component_update_all) ;=> 200nil





;;;;TEST;;;;
(defcomp foo (x)
  (update (print x)))
(setq x 10) ;=> 10
(create foo 20)
(component_update_all) ;=> 20nil
(component_update_all) ;=> 20nil
(component_update_all) ;=> 20nil


;;;;TEST;;;;
(defcomp foo ()
  (update (print 1)))
(defcomp bar ()
  (update (print 2)))
(defcomp baz ()
  (update (print 3)))
(create foo)
(create bar)
(create baz)
(component_update_all) ;=> 123nil
(component_update_all) ;=> 123nil
(component_update_all) ;=> 123nil

;;;;TEST;;;;
(defcomp foo ()
  (update (print 1)))
(defcomp bar ()
  (update (print 2)))
(defcomp baz ()
  (update (print 3)))
(create baz)
(create bar)
(create foo)
(component_update_all) ;=> 321nil
(component_update_all) ;=> 321nil
(component_update_all) ;=> 321nil

;;;;TEST;;;;
;;ignore;;
(defcomp foo ()
  (set-layer 1)
  (update (print 1)))
(defcomp bar ()
  (set-layer 2)
  (update (print 2)))
(defcomp baz ()
  (set-layer 3)
  (update (print 3)))
(create foo)
(create bar)
(create baz)
(component_update_all) ;=> 123nil
(component_update_all) ;=> 123nil
(component_update_all) ;=> 123nil

;;;;TEST;;;;
;;ignore;;
(defcomp foo ()
  (set-layer 1)
  (update (print 1)))
(defcomp bar ()
  (set-layer 2)
  (update (print 2)))
(defcomp baz ()
  (set-layer 3)
  (update (print 3)))
(create baz)
(create bar)
(create foo)
(component_update_all) ;=> 123nil
(component_update_all) ;=> 123nil
(component_update_all) ;=> 123nil
