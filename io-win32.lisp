(defun read-command-char ()
  (read))

(defun read-command-line ()
  (read-from-string (read-line)))

(defun read-string ()
  (read-line))

(defun init-charms ()
  )

(defun scr-format (&rest args)
 (apply #'format (append '(t) args)))

(defun scr-fresh-line ()
  (fresh-line))

(defun scr-princ (str)
  (princ str))

;;裏ワザ
(defun urawaza (p)
  (scr-format "~%「神の力を授かった！」~%")
  (setf (player-hp p)     999
	(player-maxhp p)  999
	(player-agi p)    999
	(player-maxagi p) 999
	(player-str p)    999
	(player-maxstr p) 999))

;; (x,y) の方向が通路だった場合は、何かに当たるか敵と遭遇するまで移動
;; する。通路でなかった場合は、その方向に普通に移動しようとしたように
;; 振る舞う。
(defun update-map-dash (map p y x &optional (first-move? t))
  (case (aref map (+ (player-posy p) y) (+ (player-posx p) x))
    (0
     (update-map map p y x)
     (unless *battle?*
       (update-map-dash map p y x nil)))
    (otherwise
     (when first-move?
       (update-map map p y x)))))
#|
;;移動先選択
(defun map-move (map p)
  (unless (or *battle?* (= *end* 2))
    ;;(show-fog-map map p)
    (show-map map p)
    (let ((cmd (read-line)))
      (cond
        ((equal cmd "w") (update-map map p -1 0))
        ((equal cmd "s") (update-map map p 1 0))
        ((equal cmd "d") (update-map map p 0 1))
        ((equal cmd "a") (update-map map p 0 -1))
        ((equal cmd "W") (update-map-dash map p -1 0))
        ((equal cmd "S") (update-map-dash map p 1 0))
        ((equal cmd "D") (update-map-dash map p 0 1))
        ((equal cmd "A") (update-map-dash map p 0 -1))
        ((equal cmd "q") (use-heal p))
        ((equal cmd "z") (setf *end* 2))
        ((equal cmd "mogezouisgod") (urawaza p))
        (t
         (format t "w,a,s,d,q,zの中から選んでください！~%"))))
    (map-move map p)))
|#
;;移動先選択
(defun map-move (map p)
  (unless (or *battle?* (= *end* 2))
    ;;(show-fog-map map p)
    (show-map map p)
    (case (read-command-char)
      (w (update-map map p -1 0))
      (s (update-map map p 1 0))
      (d (update-map map p 0 1))
      (a (update-map map p 0 -1))
      (ww (update-map-dash map p -1 0))
      (ss (update-map-dash map p 1 0))
      (dd (update-map-dash map p 0 1))
      (aa (update-map-dash map p 0 -1))
      (q (use-heal p))
      (z (setf *end* 2))
      (otherwise
       (scr-format "w,a,s,d,q,zの中から選んでください！~%")))

    (map-move map p)))

(defun show-map-key ()
  (scr-format "どちらに移動しますか？[w]上 [s]下 [d]右 [a]左 [q]薬を使う [r]終わる: ~%"))

(defun gamen-clear ()
  )
