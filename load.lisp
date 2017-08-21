(if (equal "Linux" (software-type))
    (load "io-linux.lisp" :external-format :utf-8))
    (load "io-win32.lisp" :external-format :utf-8))
(load "orc-battle.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)

(main)
(sb-ext:exit)
#|
(sb-ext:save-lisp-and-die "mogerpg.exe"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#
