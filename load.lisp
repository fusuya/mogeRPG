(load "orc-battle.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)

(sb-ext:save-lisp-and-die "mogerpg.exe"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)