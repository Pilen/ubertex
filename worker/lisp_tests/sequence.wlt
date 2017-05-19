;;;;TEST;;;;
(length nil) ;=> 0
(length "abc") ;=> 3
(length "") ;=> 0
(length (list 1 2 3)) ;=> 3

;(length (vector 1 2 3)) ;=> 3
;(length (vector)) ;=> 0
; TODO: Test with hashes

(length t) ;=> error
(length (cons 1 (cons 2 3)) ;=> error


;;;;TEST;;;;
(nth 0 '(1 2 3)) ;=> 1
(nth 1 '(1 2 3)) ;=> 2
(nth 2 '(1 2 3)) ;=> 3
(nth 3 '(1 2 3)) ;=> error
(nth -1 '(1 2 3)) ;=> error
(nth 1.0 '(1 2 3)) ;=> error

;;;;TEST;;;;
(nth '(1 2 3) 0) ;=> 1
(nth '(1 2 3) 1) ;=> 2
(nth '(1 2 3) 2) ;=> 3
(nth '(1 2 3) 3) ;=> error
(nth '(1 2 3) -1) ;=> error
(nth '(1 2 3) 1.0) ;=> error

;;;;TEST;;;;
(nth 0 (cons 1 (cons 2 (cons 3 4)))) ;=> 1
(nth 1 (cons 1 (cons 2 (cons 3 4)))) ;=> 2
(nth 2 (cons 1 (cons 2 (cons 3 4)))) ;=> 3
(nth 3 (cons 1 (cons 2 (cons 3 4)))) ;=> error
(nth 4 (cons 1 (cons 2 (cons 3 4)))) ;=> error
