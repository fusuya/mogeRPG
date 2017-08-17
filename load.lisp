(if (equal "Linux" (software-type))
    (load "io-linux.lisp")
    (load "io-win32.lisp"))
(load "orc-battle.lisp")
(load "maze-test.lisp")

(main)
(sb-ext:exit)
#|
(sb-ext:save-lisp-and-die "mogerpg.exe"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#
