(load "orc-battle-linux.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)

(main)
#|
(sb-ext:save-lisp-and-die "mogerpg.exe"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#