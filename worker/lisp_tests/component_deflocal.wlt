;; THIS FILE SHOULD MATCH `base_setq.wlt' IN CONTENT.

;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal x 1) 1)
  (assert x 1)) ;=> foo

;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal x 1
            y 2)
          2)
  (assert x 1)
  (assert y 2)) ;=> foo
;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal x 1
            y)
          error)
  (assert x error)) ;=> foo


;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal foo 1
            bar 2
            baz 3)
          3)
  (assert foo 1)
  (assert bar 2)
  (assert baz 3)

  (assert (deflocal foo 10
            bar 20
            baz)
          error)
  (assert foo 1)
  (assert bar 2)
  (assert baz 3)) ;=> foo


;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal) nil)) ;=> foo

;;;;TEST;;;;
(defcomp foo ()
  (deflocal x nil)
  (assert (deflocal foo (deflocal x (cons 1 x))
            bar (deflocal x (cons 2 x))
            baz (deflocal x (cons 3 x)))
          '(3 2 1))
  (assert foo '(1))
  (assert bar '(2 1))
  (assert baz '(3 2 1))) ;=> foo


;;;;TEST;;;;
(defcomp foo ()
  (deflocal foo 0 bar 0)
  (deflocal foo 1
    t 3
    bar 10)
  (assert foo 1)
  (assert bar 0)) ;=> foo


;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal x 1)
          1)
  (assert (deflocal 2 (deflocal x 3)) error)
  (assert x 3)) ;=> foo


;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal t 1) error)
  (assert t t)) ;=> foo

;;;;TEST;;;;
(defcomp foo ()
  (assert (deflocal nil 1) error)
  (assert nil nil)) ;=> foo
