;;;;TEST;;;;
(defun foo () 3)
(funcall 'foo) ;=> 3
;;;;TEST;;;;
(defun foo (x) 3)
(funcall 'foo 1) ;=> 3
;;;;TEST;;;;
(defun foo (x) x)
(funcall 'foo 1) ;=> 1


;;;;TEST;;;;
(funcall (lambda () 3)) ;=> 3
;;;;TEST;;;;
(funcall (lambda (x) 3) 1) ;=> 3
;;;;TEST;;;;
(funcall (lambda (x) x) 1) ;=> 1

;;;;TEST;;;;
(setq foo (lambda () 3))
(funcall foo) ;=> 3
;;;;TEST;;;;
(setq foo (lambda (x) 3))
(funcall foo 1) ;=> 3
;;;;TEST;;;;
(setq foo (lambda (x) x))
(funcall foo 1) ;=> 1
