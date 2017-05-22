;;;;TEST;;;;
((lambda () 3)) ;=> 3

;;;;TEST;;;;
(lambda (x) x)
;; Not the same as Emacs!
(type-of (lambda (x) x)) ;=> lambda

;;;;TEST;;;;
;;ignore;;
(setq foo (lambda (x) x))
(funcall foo 3) ;=> 3

;;;;TEST;;;;
(funcall (lambda (x) x) 3) ;=> 3

;;;;TEST;;;;
((lambda (x) x) 3) ;=> 3

;;;;TEST;;;;
(setq foo (lambda (x) x))
(foo) ;=> error

;;;;TEST;;;;
(lexical-let ((x 10))
  ((lambda (x) x) 2)) ;=> 10


;;;;TEST;;;;
;;ignore;;
(funcall (lexical-let ((x 10))
           (lambda (x) x))
         2) ;=> 2


;;;;TEST;;;;
(lexical-let ((x 10)
              (y 11))
  ((lambda (x) x) 2)) ;=> 10

;;;;TEST;;;;
(lexical-let ((x 10)
              (y 11))
  (lexical-let ((z 12))
    ((lambda (x) (list x y z)) 2))) ;=> (10 11 12)



;;;;TEST;;;;
(defun foo ()
  (lambda (x) x))
((lambda (y) y) 3) ;=> 3


;;;;TEST;;;;
;;ignore;;
;; How it works in Emacs
(car (lambda (x) x)) ;=> lambda
(cdr (lambda (x) x)) ;=> ((x) x)

;;;;TEST;;;;
;;ignore;;
(setq foo '(lambda (x) x))
(eq foo (eval foo)) ;=> nil
(equal foo (eval foo)) ;=> t
