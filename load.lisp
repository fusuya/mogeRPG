(if (equal "Linux" (software-type))
    (load "io-linux.lisp" :external-format :utf-8)
    (load "io-win32.lisp" :external-format :utf-8))
(load "orc-battle.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)

#|
;; デバッガフックを設定
(setf sb-ext:*invoke-debugger-hook*  
      (lambda (condition hook) 
        (declare (ignore conditoin hook))
        ;; デバッガが呼ばれたら、単にプログラムを終了する
        (sb-ext:quit)))
|#

(main)
(sb-ext:exit)
#|
(sb-ext:save-lisp-and-die "mogerpg.exe"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#
