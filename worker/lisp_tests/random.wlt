;;;;TEST;;;;
(randint 0 0) ;=> error
(randint 3 3) ;=> error

;;;;TEST;;;;
(randint 0) ;=> error

;;;;TEST;;;;
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0
(randint 1) ;=> 0

;;;;TEST;;;;
(choice) ;=> error
(choice 1) ;=> 1
(choice 1 1) ;=> 1
(choice 1 1 1) ;=> 1
