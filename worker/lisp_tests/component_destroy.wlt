;;;;TEST;;;;
(defcomp foo ()
  (update (print 100) (destroy)))
(create foo) ;=> #<component foo>
(component_update_all) ;=> 100nil
(component_update_all) ;=> nil

;;;;TEST;;;;
(defcomp foo ()
  (update (print 100)))
(setq f (create foo)) ;=> #<component foo>
(component_update_all) ;=> 100nil
(destroy f)
(component_update_all) ;=> nil

;;;;TEST;;;;
(defcomp foo ()
  (print 1)
  (destroy)
  (print 2))
(setq f (create foo)) ;=> 12#<component foo>

;;;;TEST;;;;
(defcomp foo ()
  (print 1)
  (destroy)
  (print 2)
  (update (print 3)))
(setq f (create foo)) ;=> 12#<component foo>
(component_update_all) ;=> nil

;;;;TEST;;;;
(defcomp foo ()
  (update (print 1) (destroy) (print 2)))
(setq f (create foo)) ;=> #<component foo>
(component_update_all) ;=> 12nil
(component_update_all) ;=> nil


;;;;TEST;;;;
(defcomp foo ()
  (deflocal x 10)
  (update (print (+ (or (destroy) 2) x))))
(setq f (create foo))
(component_update_all) ;=> 12nil
(component_update_all) ;=> nil


;;;;TEST;;;;
(defcomp foo ()
  (update (print 1)))
(defcomp bar ()
  (destroy f)
  (update (print 2)))
(setq f (create foo))
(component_update_all) ;=> 1nil
(setq b (create bar))
(component_update_all) ;=> 2nil

;;;;TEST;;;;
(defcomp foo ()
  (update (print 1)))
(defcomp bar ()
  (destroy f)
  (update (print 2)))
(setq f (create foo))
(component_update_all) ;=> 1nil
(setq b (create bar))
(component_update_all) ;=> 2nil
