;;;;TEST;;;;
(defcomp foo ()
  (set-layer 1)
  (update (print 1)))
(defcomp bar ()
  (set-layer 1)
  (update (print 2)))

(create foo)
(create bar)
(component_update_all) ;=> 12nil


;;;;TEST;;;;
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

;;;;TEST;;;;
(defcomp foo (i)
  (set-layer i)
  (update (print "foo")))
(defcomp bar (i)
  (set-layer i)
  (update (print "bar")))
(defcomp baz (i)
  (set-layer i)
  (update (print "baz")))

(create foo 3)
(create bar 3)
(create baz 3)
(component_update_all) ;=> "foo""bar""baz"nil

;;;;TEST;;;;
(defcomp foo (i)
  (set-layer i)
  (update (print "foo")))
(defcomp bar (i)
  (set-layer i)
  (update (print "bar")))

(create foo 0)
(create bar 0)
(component_update_all) ;=> "foo""bar"nil

;;;;TEST;;;;
(defcomp foo (i)
  (set-layer i)
  (update (print "foo")))
(defcomp bar (i)
  (set-layer i)
  (update (print "bar")))
(defcomp baz (i)
  (set-layer i)
  (update (print "baz")))

(create foo 0)
(create bar 0)
(create baz 0)
(component_update_all) ;=> "foo""bar""baz"nil

;;;;TEST;;;;
(defcomp foo (i)
  (set-layer i)
  (update (print i)))
(create foo 7)
(create foo 2)
(create foo 1)
(create foo 0)
(create foo 3)

(component_update_all) ;=> 01237nil

;;;;TEST;;;;
;;ignore;;
(defcomp foo (i)
  (set-layer i)
  (update (print i)))
(defcomp bar (i)
  (set-layer i)
  (update (print i)))
(defcomp baz (i)
  (set-layer i)
  (update (print i)))
(create foo 0)
(create baz -1)
(create bar -4)

(component_update_all) ;=> -4-10nil


;;;;TEST;;;;
;;ignore;;
(defcomp foo (i)
  (set-layer i)
  (update (print i)))
(defcomp bar (i)
  (set-layer i)
  (update (print i)))
(defcomp baz (i)
  (set-layer i)
  (update (print i)))
(create foo 0)
(create bar -4)
(create baz -1)

(component_update_all) ;=> -4-10nil


;;;;TEST;;;;
;;ignore;;
(defcomp foo (i)
  (set-layer i)
  (update (print i)))
(create foo 7)
(create foo 2)
(create foo 1)
(create foo 0)
(create foo 3)
(create foo -4)
(create foo -1)
(create foo -7)


(component_update_all) ;=> -7-4-101237nil
