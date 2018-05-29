
;;;;TEST;;;;
(f "{}") ;=> error
;;;;TEST;;;;
(let ((x 1))
  (f "{(+ x 2}")) ;=> error
;;;;TEST;;;;
(let ((x 1))
  (f "{error}")) ;=> error


;;;;TEST;;;;
(f "foo") ;=> "foo"
(f "foo{1}") ;=> "foo1"
(f "foo{1}bar") ;=> "foo1bar"
(f "{1}") ;=> "1"
(f "{1}{2}") ;=> "12"
(f "{1}x{2}") ;=> "1x2"
(f "foo{1}bar{2}baz") ;=> "foo1bar2baz"


;;;;TEST;;;;
(let ((x 1))
  (f "{x}")) ;=> "1"
(let ((x 1))
  (f "foo{x}")) ;=> "foo1"
(let ((x 1))
  (f "foo{x}bar")) ;=> "foo1bar"
(let ((x 1)
      (y 2))
  (f "{x}{y}")) ;=> "12"
(let ((x 1)
      (y 2))
  (f "{x}x{y}")) ;=> "1x2"
(let ((x 1)
      (y 2))
  (f "foo{1}bar{2}baz")) ;=> "foo1bar2baz"

;;;;TEST;;;;
(let ((x 1))
  (f "{(+ x 2)}")) ;=> "3"

;;;;TEST;;;;
(let ((x "foo")) (f "{x}")) ;=> "foo"



;;;;TEST;;;;
(split-lines "abc\ndef") ;=> ("abc" "def")
(split-lines "abc\ndef\n") ;=> ("abc" "def" "")

;;;;TEST;;;;
;;ignore;;
(split-lines "abc\ndef" t) ;=> ("abc\\n" "def")
(split-lines "abc\ndef\n" t) ;=> ("abc\\n" "def\n")
