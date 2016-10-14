;;;;TEST;;;;
;;ignore;;
(defun foo ()
  (lambda (x) x))
((lambda (y) y) 3) ;=> 3


;;;;TEST;;;;
;;ignore;;
(car (lambda (x) x)) ;=> lambda
(cdr (lambda (x) x)) ;=> ((x) x)

;;;;TEST;;;;
;;ignore;;
(setq foo '(lambda (x) x))
(eq foo (eval foo)) ;=> nil
(equal foo (eval foo)) ;=> t
