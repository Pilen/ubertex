
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
(defcomp foo (x)
  (setq g1 (f "-{x}-")))
(create foo "a")
g1 ;=> "-a-"

;;;;TEST;;;;
(defcomp foo ()
  (deflocal x "a")
  (setq g1 (f "-{x}-")))
(create foo "a")
g1 ;=> "-a-"


;;;;TEST;;;;
(split-lines "abc\ndef") ;=> ("abc" "def")
(split-lines "abc\ndef\n") ;=> ("abc" "def" "")

;;;;TEST;;;;
;;ignore;;
(split-lines "abc\ndef" t) ;=> ("abc\\n" "def")
(split-lines "abc\ndef\n" t) ;=> ("abc\\n" "def\n")


;;;;TEST;;;;
(remove-chars "aBcDeaBcDe" "BD") ;=> "aceace"
(remove-chars "aBcDeaBcDe" "acea") ;=> "BDBD"

;;;;TEST;;;;
(count-lines "abc") ;=> 1
(count-lines "abc\n") ;=> 2
(count-lines "abc\ndef") ;=> 2
(count-lines "abc\ndef\n") ;=> 3



;;;;TEST;;;;
(substring "abc" nil nil) ;=> "abc"
(substring "abc") ;=> "abc"
(substring "abc" 0 3) ;=> "abc"
(substring "abc" 1 3) ;=> "bc"
(substring "abc" 0 2) ;=> "ab"
(substring "abc" 1 2) ;=> "b"
(substring "abc" 1 1) ;=> ""
(substring "abc" 0 -1) ;=> "ab"
(substring "abc" 0 -2) ;=> "a"
(substring "abc" 0 -3) ;=> ""
(substring "abc" 0 -4) ;=> error
(substring "abc" -1 3) ;=> "c"
(substring "abc" -2 3) ;=> "bc"
(substring "abc" -3 3) ;=> "abc"
(substring "abc" -4 3) ;=> error
(substring "abc" -1 -3) ;=> error
(substring "abc" -3 -1) ;=> "ab"
(substring "abc" -3 -2) ;=> "a"
(substring "abc" -3 -3) ;=> ""
(substring "abc" -2 -1) ;=> "b"
(substring "abc" -2 -2) ;=> ""
