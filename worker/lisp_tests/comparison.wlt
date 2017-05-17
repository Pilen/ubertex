;;;;TEST;;;;
(not nil) ;=> t
(not t) ;=> nil
(not 'x) ;=> nil
(not 1) ;=> nil
(not 0) ;=> nil
(not 1.0) ;=> nil
(not 0.0) ;=> nil
(not "abc") ;=> nil
(not "") ;=> nil

;;;;TEST;;;;
(not) ;=> error
(not t t) ;=> error
(not nil nil) ;=> error


;;;;TEST;;;;
(>) ;=> error
(> 1 nil) ;=> error
(> 1) ;=> t
(> 1.0) ;=> t
;;;;TEST;;;;
;; This is how Emacs lisp does it
(> nil) ;=> t
(> t) ;=> t
(> 'x) ;=> t
(> "abc") ;=> t
;;;;TEST;;;;
(> 10 5) ;=> t
(> 10 5 2) ;=> t
(> 10 5.0) ;=> t
(> 10 5.0 2) ;=> t
(> 10 5.1 5) ;=> t
(> 10 5.1 5.0) ;=> t
;;;;TEST;;;;
(> 5 5.0) ;=> nil
(> 5.0 5) ;=> nil
;;;;TEST;;;;
(> 5 10) ;=> nil
(> 2 5 10) ;=> nil
(> 5.0 10) ;=> nil
(> 2 5.0 10) ;=> nil
(> 5 5.1) ;=> nil
(> 5.0 5.1) ;=> nil
(> 5 10 5) ;=> nil
