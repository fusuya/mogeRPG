(if (equal "Linux" (software-type))
    (load "io-linux.lisp" :external-format :utf-8)
    (load "io-win32.lisp" :external-format :utf-8))
(load "orc-battle.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)

;;デバッガ起動したくないときに使う
;; デバッガフックを設定
(setf sb-ext:*invoke-debugger-hook*  
      (lambda (condition hook) 
        (declare (ignore condition hook))
        ;; デバッガが呼ばれたら、単にプログラムを終了する
        ;; recklessly-p に t を指定して、後始末(標準出力ストリームのフラッシュ等)が行われないようにする
        ;;(sb-ext:quit :recklessly-p t)))
	;;デバッガの文字ずれないようにする
	(charms/ll:endwin)))


(main)
(sb-ext:exit)
#|
(sb-ext:save-lisp-and-die "mogerpg"
			  :toplevel #'main
			  :save-runtime-options t
			  :executable t)
|#

