(if t
    (progn
      (load "io-linux.lisp" :external-format :utf-8)
      (init-charms))
  (load "io-win32.lisp" :external-format :utf-8))
(load "orc-battle.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)

(scr-format "現在のランキング:~%")
(ranking-transaction (lambda (ranking)
                       (ranking-show ranking)
                       ranking))

(scr-format "タイム(秒)?: ~%")
(let ((total-seconds (read-from-string (read-string))))
  (ranking-dialog total-seconds))

(scr-format "エンターキーを押して終了~%")
(read-string)

(sb-ext:exit)
