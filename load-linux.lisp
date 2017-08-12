(load "readline.lisp")
(load "orc-battle-linux.lisp")
(load "maze-test.lisp")

(main)
#|
(sb-ext:save-lisp-and-die "mogerpg.exe"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#