;;;;TEST;;;;
;;ignore;;
(read-file "lisp_tests/test-file.txt") ;=> "abc\n"

;;;;TEST;;;;
(read-file "lisp_tests/test-file-no-newline.txt") ;=> "foo"


;;;;TEST;;;;
(read-file "NON_EXISTING_FILE.txt") ;=> error

;;;;TEST;;;;
(length (read-file "lisp_tests/test-file-long.txt")) ;=> 66560
