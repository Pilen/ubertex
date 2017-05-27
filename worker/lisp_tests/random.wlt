;;;;TEST;;;;
(randint 0 0) ;=> error
(randint 3 3) ;=> error


;;;;TEST;;;;
(choice) ;=> error
(choice 1) ;=> 1
(choice 1 1) ;=> 1
(choice 1 1 1) ;=> 1
