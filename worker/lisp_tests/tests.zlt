;;;;TEST;;;;
;;ignore;;
1 ;=> 1

;;;;TEST;;;;
;;ignore;;
(setq x 0)
(setq y 0)
(setq z 0)
(set (progn (setq x 1) 't) (progn (setq y 1) 1))
x ;=> 1
y ;=> 1
z ;=> 0


;;;;TEST;;;;
;;ignore;;
(((lambda (x) (print x) (lambda (y) (print x) 1)) 10) 20) ;=> error
('(lambda (x) (print x) 1) 10) ;=> error


;;;;TEST;;;;
;;ignore;;
(setq x 0)
(let* ((a (setq x 1))
       (a (setq x 3)))
  a) ;=> 3
x ;=> 3

(setq x 0)
((lambda (a)
   ((lambda (a)
      a)
    (setq x 3)))
 (setq x 1)) ;=> 3
x ;=> 3


;;;;TEST;;;;
;;ignore;;
(lexical-let ((a 1)
          (a 2))
  a) ;=> 2

(lexical-let* ((a 1)
           (a 2))
  a) ;=> 2




;;;;TEST;;;;
;;ignore;;
(setq x 0)
(let ((x (setq x (1+ x)))
      (x (setq x (1+ x))))
  x) ;=> 2
x ;=> 2

(setq x 0)
(let* ((x (setq x (1+ x)))
       (x (setq x (1+ x))))
  x) ;=> 2
x ;=> 1


;;;;TEST;;;;
;;ignore;;

;; lambda executed inside lexical-let
(setq x 0)
(lexical-let ((x 1))
  ((lambda () x))) ;=> 1

;; funcall necessary
(setq x 0)
(funcall (lexical-let ((x 1))
       (lambda () x))) ;=> 1

(defun foo () 100000)
(defun bar () 100000)
(setq x 0)
(lexical-let ((x 1))
  (setq foo (lambda () (setq x (1+ x))))
  (setq bar (lambda () x)))
(funcall bar) ;=> 1
(funcall foo)
(funcall bar) ;=> 2



(eq (cons 1 2) (cons 1 2))
(= (cons 1 2) (cons 1 2))


(lexical-let ((x 0))
  (setq foo (lambda () x))
  (funcall foo))

(setq x 1000)
(funcall foo)





(setq foo (lambda (x) (+ 1 x)))
(funcall foo 4) ;=> 5
(setf (elt (elt foo 2) 1) 10)
(funcall foo 4) ;=> 14



(setq x 10)
(let (x)
  x) ;=> nil

(setq x 10)
(lexical-let (x)
  x) ;=> nil





;;;;TEST;;;;
;;ignore;;
(setq x (list 1 2 3))
(setf (car x) 4)
x ;=> (4 2 3)
(defun id (x) x)
(setf (id (car x)) 5) ;=> error
x ;=> (4 2 3)
(setq y (car x))
(setf y 5)
x ;=> (4 2 3)
y ;=> 5


;;;;TEST;;;;
;;ignore;;
(setq x (list 1 2 3))
(defun id (x) x)
(id (setf (car (id x)) 10)) ;=> 10
x ;=> 10


;;;;TEST;;;;
;;ignore;;
;;;;LATEX caption=eq-modification, label=test:eq-modification
(setq x '(1 2 3))
(eq x (progn (setf (car x) 10) x)) ;=> t

(setq x '(1 2 3))
(equal x (progn (setf (car x) 10) x)) ;=> t

(eq '(1 2 3) '(10 2 3)) ;=> nil
(equal '(1 2 3) '(10 2 3)) ;=> nil


;;;;TEST;;;;
;;ignore;;
(setq x (list 1 2 3))
(setq y (list x 4 5))
(setf (car x) 10)
x ;=> (10 2 3)
y ;=> ((10 2 3) 4 5)


;;;;TEST;;;;
;;ignore;;
(apply 'list (1 x 3)) ;=> (1 x 3)


;;;;LATEX caption=eval-fun, label=test:eval-fun1
(setq x '(+ (setf (caddr x) 30) 3)) ;=> (+ (setf (caddr x) 30) 3)
(caddr x) ;=> 3
(eval x) ;=> 60
x ;=> (+ (setf (caddr x) 30) 30)

;;;;LATEX caption=eval-fun2, label=test:eval-fun2
(setq x '(list 1 2 (setf (nthcdr (length x) x) (cons (nth (- (length x) 1) x) nil))))
(eval x) ;=> Emacs segfault (med det samme)

;;;;LATEX caption=lambda-modification, label=test:lambda-modification]
(setq foo (lambda () (list 1 2 3) -1))
(setf (nthcdr (length (nth 2 foo)) (nth 2 foo)) (cons 4 nil))
foo ;;=> (lambda () (list 1 2 3 4) -1)

(setq foo (lambda () (list 1 2 3) -1))
(setq foo (lambda () (list 1 2 3)
        (setf (nthcdr (length (nth 2 foo)) (nth 2 foo)) (cons 4 nil))
        (funcall foo)))

(funcall foo) ;=> stack overflow
foo ;=> (lambda nil (list 1 2 3 4 4 4 4 4 4 4 4 ...   ) ...)

;;;;TEST;;;;
;;ignore;;
(setq foo (lambda (x)
        (setf (nthcdr (- (length foo) 1) foo) (cons x (nthcdr (- (length foo) 1) foo)))
        (funcall foo (+ x 1))))

(funcall foo 1) ;=> stack overflow
foo ;=> (lambda (x) (setf ...) 1 2 3 4 5 6 7 8 9 ... (funcall foo (+ x 1)))


;;;;TEST;;;;
;;ignore;;
(setq x '(list 1 2 (eval x)))
(eval x) ;=> stack overflow



;;;;TEST;;;;
;;ignore;;
(setq x ())
(while t
  (push 1 x)) ;=> (uendelig løkke, langsom, fylder hukommelsen)



;;;;TEST;;;;
;;ignore;;
(setq x (list 1 2 3))
(nconc x x)
(setq x (cons '+ x))
(eval x) ;=> uendelig løkke


;;;;TEST;;;;
;;ignore;;
(setq x (list '+ 2 3))
(setcar (cddr x) x)
(eval x) ;=> stack overflow


;;;;TEST;;;;
;;ignore;;
(set 'x 10)
(set 'nil (progn (set 'x 11) 20))
x ;=> 11


;;;;TEST;;;;
;;ignore;;
(setq x 1
      y 2
      z 3) ;=> 3
x ;=> 1
y ;=> 2
z ;=> 3


;;;;TEST;;;;
;;ignore;;
(setq x 1
      y 2
      z 3
      q) ;=> nil
x ;=> 1
y ;=> 2
z ;=> 3
q ;=> nil


;;;;TEST;;;;
;;ignore;;
(setq x 1
      y 2
      z 3) ;=> 3
x ;=> 1
y ;=> 2
z ;=> 3

(setq x 10
      'y 20
      z 30) ;=> error
x ;=> 10
y ;=> 2
z ;=> 3



;;;;TEST;;;;
;;ignore;;
(setq x 1
      y 2
      z 3) ;=> 3
x ;=> 1
y ;=> 2
z ;=> 3

(setq x 10
      (progn (setq x 11) 'y) 20
      z 30) ;=> error
x ;=> 10
y ;=> 2
z ;=> 3


;;;;TEST;;;;
;;ignore;;
(setq x 1
      y 2
      z 3) ;=> 3
x ;=> 1
y ;=> 2
z ;=> 3

(setq x 10
      'y (progn (setq x 11) 20)
      z 30) ;=> error
x ;=> 11
y ;=> 2
z ;=> 3


;;;;TEST;;;;
;;ignore;;
(setq x 1)
(let ((x (+ 10 x))) x) ;=> 11
x ;=> 1

(setq x 1)
(let* ((x (+ 10 x))) x) ;=> 11
x ;=> 1

;;;;TEST;;;;
;;ignore;;
(setq x 10)
((lamdba ()) (setq x 11))
x ;=> 10
