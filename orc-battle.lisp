(load "item.lisp" :external-format :utf-8)

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)
(defparameter *player-pos* 85)
(defparameter *battle?* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *boss-builders* nil)
(defparameter *monster-num* 6)
(defparameter *monster-level* 0) ;;階数によるモンスターのレベル
(defparameter *boss?* nil)
(defparameter *end* nil)
(defparameter *tate* 11) ;;マップサイズ
(defparameter *yoko* 11)

(defstruct player
  (hp 30)
  (maxhp 30)
  (agi 30)
  (maxagi 30)
  (str 30)
  (maxstr 30)
  (posy 0)
  (posx 0)
  (map 1) ;;マップ深度
  (heal 2) ;;持ってる薬の数
  (hammer 5) ;;持ってるハンマーの数
  (level 1)
  (exp 0)
  (buki '("なし" 0 0 0))
  (monster-num 0))

(defun init-data ()
  (setf *monster-num* 6
	*monster-level* 0
	*boss?* nil
	*end* nil
	*battle?* nil
	*boss-builders* nil)
  (push-boss))
;;バトル開始
(defun orc-battle (p)
  (format t "~%敵が現れた！！~%")
  (init-monsters p)
  ;;(init-player)
  (game-loop p)
  (when (player-dead p)
    (format t "Game Over.~%")
    (format t "あなたは地下~d階で力尽きた。~%" (player-map p))
    (format t "もう一度挑戦しますか？(y or n)~%")
    (case (read)
          (y (main))))
  (when (monsters-dead)
    (if (>= (player-exp p) 100)
	(progn (format t "     「レベルアップ！！」~%")
	       (incf (player-level p))
	       (incf (player-maxhp p) 3)
	       (incf (player-maxagi p) 1)
	       (incf (player-maxstr p) 1)
	       (setf (player-hp p) (player-maxhp p)
		     (player-agi p) (player-maxagi p)
		     (player-str p) (player-maxstr p)
		     (player-exp p) (- (player-exp p) 100))))
    (format t "     「大勝利！」~%~%")))
;;ボスバトル
(defun boss-battle (p)
  (format t "~%もげぞうが現れた！！~%")
  (boss-monsters p)
  ;;(init-player)
  (game-loop p)
  (when (player-dead p)
    (format t "Game Over.~%")
    (format t "ボスに倒された！~%")
    (format t "もう一度挑戦しますか？(y or n)~%")
    (case (read)
          (y (main))))
  (when (monsters-dead)
    (if (>= (player-exp p) 100)
	(progn (format t "「レベルアップ！！」~%")
	       (incf (player-level p))
	       (incf (player-maxhp p) 3)
	       (incf (player-maxagi p) 1)
	       (incf (player-maxstr p) 1)
	       (setf (player-hp p) (player-maxhp p)
		     (player-agi p) (player-maxagi p)
		     (player-str p) (player-maxstr p)
		     (player-exp p) (- (player-exp p) 100))))
    (format t "「大勝利！」~%~%")
    (setf *end* t)))
;;バトル時、プレイヤーが死ぬかモンスターが全滅するまでループ
(defun game-loop (p)
  (unless (or (player-dead p) (monsters-dead))
    ;;(show-player p)
    (dotimes (k (1+ (truncate (/ (max 0 (player-agi p)) 15))))
      (unless (monsters-dead)
	(show-player p)
	(show-monsters)
	(player-attack p)))
    (format t "~%~%")
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m p)))
	 *monsters*)
    (format t "~%~%")
    (game-loop p)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead (p)
  (<= (player-hp p) 0))

(defun show-player (p)
  (fresh-line)
  (princ "あなたのステータス ")
  (princ "HP ")
  (princ (player-hp p))
  (princ ", 素早さ ")
  (princ (player-agi p))
  (princ ", 力 ")
  (princ (player-str p))
  (fresh-line)
  (format t "持ち物:回復薬 ~d個~%" (player-heal p)))

(defun player-attack (p)
  (fresh-line)
  ;;(show-player p)
  (format t "攻撃方法: [1]突く [2]ダブルスウィング [3]なぎ払う [q]回復薬を使う:~%")
  (case (read)
    (1 (monster-hit p (pick-monster p)
		    (+ 2 (randval (ash (player-str p) -1)))))
    (2 (let ((x (randval (truncate (/ (player-str p) 6)))))
	 (princ "ダブルスウィングのダメージは ")
	 (princ x)
	 (fresh-line)
	 (monster-hit p (pick-monster p) x)
	 (show-monsters)
	 (unless (monsters-dead)
	   (monster-hit p (pick-monster p) x))))
    (3 (dotimes (x (1+ (randval (truncate (/ (player-str p) 3)))))
	 (unless (monsters-dead)
	   (monster-hit p (random-monster) 1))))
    (q (use-heal p))
    (otherwise
     (format t "1,2,3の中から選んでください！~%")
     (player-attack p))))

(defun randval (n)
  (1+ (random (max 1 n))))


(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster (p)
  (fresh-line)
  (princ "攻撃したいモンスター番号を選択 #:")
  (fresh-line)
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x (player-monster-num p))))
	(progn (princ "有効なモンスター番号ではありません。")
	       (pick-monster p))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "そのモンスターはすでに死んでます。")
		     (pick-monster p))
	      m)))))

(defun init-monsters (p)
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array (setf (player-monster-num p) (randval (+ *monster-num* (if (>= (player-level p) 7)
										    6
										    (player-level p)))))))))

(defun boss-monsters (p)
  (let ((hoge 0))
    (setf *monsters*
	  (map 'vector
	       (lambda (x)
		 (if (= hoge 0)
		     (progn (incf hoge)
			    (funcall (nth 4 *boss-builders*)))
		     (funcall (nth (random (1- (length *boss-builders*)))
				   *boss-builders*))))
	       (make-array 10)))
    (setf (monster-health (aref *monsters* 0)) 200
	  (player-monster-num p) 10)))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "敵:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "  ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**死亡**")
	       (progn (princ "(体力=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval (+ 10 *monster-level*))))


(defmethod monster-hit (p m x)
  (decf (monster-health m) x)
  (format t "「~aに ~dのダメージを与えた！」~%" (type-of m) x)
  (if (monster-dead m)
      (case (type-of m)
	(orc (incf (player-exp p) 2)
	 (princ "「オークを倒しました！」 "))
	(hydra (incf (player-exp p) 4)
	 (princ "「ヒドラを倒しました！」 "))
	(slime-mold (incf (player-exp p) 3)
	 (princ "「スライムを倒しました！」 "))
	(brigand (incf (player-exp p) 5)
	 (princ "「ブリガンドを倒しました！」 ")))))
      #|
      (progn (princ "「")
	     (princ (type-of m))
	     (princ "に ")
	     (princ x)
	     (princ " のダメージを与えました！」 ")
	     (fresh-line))))
|#
(defmethod monster-show (m)
  (princ "凶暴な ")
  (princ (type-of m)))

(defmethod monster-attack (m p))

;;ボス
(defstruct (boss (:include monster)) (boss-atk 10))
(defmethod monster-show ((m boss))
  (princ "ボス：もげぞう"))
(defmethod monster-attack ((m boss) (p player))
  (let ((x (randval (+ (player-level p) (boss-boss-atk m)))))
    (case (random 3)
      (0
       (format t "「もげぞうの攻撃。~dのダメージをくらった。」~%" x)
       (decf (player-hp p) x))
      (1
       (format t "「もげぞうの不思議な踊り。素早さが~d下がった。」~%" x)
       (decf (player-agi p) x))
      (2
       (format t "「もげぞうのなんかすごい攻撃！すべてのステータスが~d下がった！」~%" x)
       (decf (player-hp p) x)
       (decf (player-agi p) x)
       (decf (player-str p) x)))))

(defun push-boss ()
  (push #'make-boss *boss-builders*)
  (push #'make-orc *boss-builders*)
  (push #'make-hydra *boss-builders*)
  (push #'make-slime-mold *boss-builders*)
  (push #'make-brigand *boss-builders*)
  )

(defstruct (orc (:include monster)) (club-level (randval (+ 8 *monster-level*))))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "レベル ")
  (princ (orc-club-level m))
  (princ " の邪悪なオーク。"))

(defmethod monster-attack ((m orc) (p player))
  (let ((x (randval (orc-club-level m))))
    (princ "「オークが棍棒で殴ってきて ")
    (princ x)
    (princ " のダメージをくらった。」")
    (fresh-line)
    (decf (player-hp p) x)))



(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ (monster-health m))
  (princ " 本の首を持つ意地悪なヒドラ。"))

(defmethod monster-hit ((p player) (m hydra) x)
  (decf (monster-health m) x)
  (format t "「~aに ~dのダメージを与えた！」~%" (type-of m) x)
  (if (monster-dead m)
      (princ "「首がなくなったヒドラは倒れた。」")
      (progn (princ "「")
	     (princ x)
	     (princ " 本のヒドラの首を斬った！」 "))))

(defmethod monster-attack ((m hydra) (p player))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "「ヒドラの攻撃 ")
    (princ x)
    (princ " のダメージを食らった。」")
    (fresh-line)
    (princ "「ヒドラの首が一本生えてきた！」")
    (fresh-line)
    (incf (monster-health m))
    (decf (player-hp p) x)))


(defstruct (slime-mold (:include monster)) (sliminess (randval (+ 5 *monster-level*))))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "ベタベタ度 ")
  (princ (slime-mold-sliminess m))
  (princ " のスライム"))

(defmethod monster-attack ((m slime-mold) (p player))
  (let ((x (randval (slime-mold-sliminess m))))
    (cond
      ((> (player-agi p) 0)
       (format t "「スライムは足に絡みついてきてあなたの素早さが ~d 下がった！~%" x)
       (decf (player-agi p) x)
       (if (< (player-agi p) 0)
	 (setf (player-agi p) 0)))
      (t
	(format t "「スライムが何か液体を吐きかけてきて ~d ダメージくらった」~%" x)
	(decf (player-hp p) x)))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand) (p player))
  (let ((x (max (player-hp p) (player-agi p) (player-str p))))
    (cond ((= x (player-hp p))
	   (princ "「ブリガンドのスリングショットの攻撃で2ダメージくらった！」")
	   (fresh-line)
	   (decf (player-hp p) 2))
	  ((= x (player-agi p))
	   (princ "「ブリガンドは鞭であなたの足を攻撃してきた！素早さが2減った！」")
	   (fresh-line)
	   (decf (player-agi p) 2))
	  ((= x (player-str p))
	   (princ "「ブリガンドは鞭であなたの腕を攻撃してきた！力が2減った！」")
	   (fresh-line)
	   (decf (player-str p) 2)))))


;;---------------------------------------------------------------------------------------



(defun map-type (num)
  (case num
    (30 "壁") ;; 壁
    (0  "　")
    (1  "主") ;; プレイヤーの位置
    (4  "薬") ;; 薬
    (5  "ボ") ;;ボス
    (3  "宝") ;; 宝箱
    (2  "下") ;; 下り階段
    ))
;;マップ表示
(defun show-map (map p)
  (format t "地下~d階~%" (player-map p))
  (format t "現在のステータス HP ~d, 素早さ ~d, 力 ~d, exp ~d~%" (player-hp p) (player-agi p)
	  (player-str p) (player-exp p))
  (format t "現在の武器:~a~%" (first (player-buki p)))
  (format t "持ち物:回復薬 ~d個 ハンマー~d個~%" (player-heal p) (player-hammer p))
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (princ (map-type (aref map i j)))

      (if (= j (- *yoko* 1))
	  (case i
	    (0 (format t " 主:プレイヤーの位置~%"))
	    (2 (format t " 宝:宝箱~%"))
	    (1 (format t " 下:下り階段~%"))
	    (3 (format t " 薬:回復薬~%"))
	    (4 (format t " ボ:ボス~%"))
	    (otherwise (fresh-line))))))
  (format t "どちらに移動しますか？[u]上 [d]下 [r]右 [l]左 [q]薬を使う: ~%"))
;;マップ表示 視界制限ver
(defun show-fog-map (map p)
  (format t "地下~d階~%" (player-map p))
  (format t "現在のステータス HP ~d, 素早さ ~d, 力 ~d, exp ~d~%" (player-hp p) (player-agi p)
	  (player-str p) (player-exp p))
  (format t "現在の武器:~a~%" (first (player-buki p)))
  (format t "持ち物:回復薬 ~d個 ハンマー~d個~%" (player-heal p) (player-hammer p))
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (cond
	((or (= i 0) (= i (- *tate* 1)))
	 (princ (map-type (aref map i j))))
	((or (= j 0) (= j (- *yoko* 1)))
	 (princ (map-type (aref map i j))))
	((and (>= (+ (player-posy p) 2) i (- (player-posy p) 2))
	      (>= (+ (player-posx p) 2) j (- (player-posx p) 2)))
	 (princ (map-type (aref map i j))))
	(t
	 (princ "暗")))
      (if (= j (- *yoko* 1))
	  (case i
	    (0 (format t " 主:プレイヤーの位置~%"))
	    (2 (format t " 宝:宝箱~%"))
	    (1 (format t " 下:下り階段~%"))
	    (3 (format t " 薬:回復薬~%"))
	    (4 (format t " 暗:見えてない場所~%"))
	    (otherwise (fresh-line)))))))
;;マップ設定
(defun set-map (map moto)
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (setf (aref map i j) (aref moto i j)))))
;;プレイヤーが死ぬか先頭に入るまでループ
(defun main-game-loop (map p)
  (setf *battle?* nil)
  (unless (player-dead p)
    (map-move map p)
    (if *boss?*
	(boss-battle p)
	(orc-battle p))
    (if *end*
        (progn
	(format t "~%「あなたは見事もげぞうの迷宮をクリアした！」~%
           もう一度挑戦しますか？(y or n)~%")
 (case (read)
       (y (main))))
	(main-game-loop map p))))


(defun main ()
  (setf *random-state* (make-random-state t))
  (let* ((p (make-player))
	 (map (maze p)))
    (init-data)
    (init-monsters p)
    ;;(init-player)
    ;;(setf *player-pos* 85)
    (setf *battle?* nil)
    ;;(setf (aref map (player-pos p)) 1)
    (main-game-loop map p)))
;;壁破壊
(defun kabe-break (map p y x)
  (format t "「ハンマーで壁を壊しますか？」[y or n]:~%")
  (case (read)
    (y
      (if (= (random 2) 0)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 0)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 3))
     (decf (player-hammer p))
     (format t "「壁を壊しました。」~%"))))


;;見つけた武器を装備するか
(defun equip? (p item-a)
  (let ((item (assoc item-a *buki* :test #'equal)))
  (format t "「~aを見つけた」~%" (first item))
  (format t "現在の装備品：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	  (first (player-buki p)) (second (player-buki p)) (third (player-buki p)) (fourth (player-buki p)))
  (format t "~a 攻撃力:~d HP:~d 素早さ:~d~%"
	  (first item) (second item) (third item) (fourth item))
  (format t "「装備しますか？」(y or n)~%")
  (case (read)
    (y
     (format t "「~aを装備した。」~%" (first item))

     (incf (player-hp p)     (- (third item) (third (player-buki p))))
     (incf (player-maxhp p)  (- (third item) (third (player-buki p))))
     (incf (player-str p)    (- (second item) (second (player-buki p))))
     (incf (player-maxstr p) (- (second item) (second (player-buki p))))
     (incf (player-agi p)    (- (fourth item) (fourth (player-buki p))))
     (incf (player-maxagi p) (- (fourth item) (fourth (player-buki p))))
     (setf (player-buki p) item))
    (n
     (format t "「~aを見なかったことにした。」~%" (first item)))
    (otherwise
     (equip? p item)))))

(defun hummer-get (p)
  (format t "「ハンマーを見つけた。」~%")
  (incf (player-hammer p)))
;;しょぼいものほど確率が高くなるように
(defun buki-get (p item-l)
  (let ((x (random 32)))
    (case x
      ((0 1 2 3 4 5)
       (equip? p (aref item-l 0)))
      ((6 7 8 9 10)
       (equip? p (aref item-l 1)))
      ((11 12 13 14)
       (equip? p (aref item-l 2)))
      ((15 16 17)
       (equip? p (aref item-l 3)))
      ((18 19 20)
       (equip? p (aref item-l 4)))
      ((21 22 23)
       (equip? p (aref item-l 5)))
      ((24 25 26)
       (equip? p (aref item-l 6)))
      ((27 28)
       (equip? p (aref item-l 7)))
      ((29 30)
       (equip? p (aref item-l 8)))
      (31
       (equip? p (aref item-l 9))))))
;;アイテムゲット
(defun item-get (p)
  (let ((x (random 3)))
    (case x
      ((0 2) ;;武器ゲット
       (cond
	 ((<= 1 (player-map p) 10)
	  (buki-get p *buki1-10*))
	 ((<= 11 (player-map p) 20)
	  (buki-get p *buki11-20*))
	 ((<= 21 (player-map p) 30)
	  (buki-get p *buki21-30*))
	 ((<= 31 (player-map p) 40)
	  (buki-get p *buki31-40*))
	 ((<= 41 (player-map p) 50)
	  (buki-get p *buki41-50*))
	 ((<= 51 (player-map p) 60)
	  (buki-get p *buki51-60*))
	 ((<= 61 (player-map p) 70)
	  (buki-get p *buki61-70*))
	 ((<= 71 (player-map p) 80)
	  (buki-get p *buki71-80*))
	 ((<= 81 (player-map p) 90)
	  (buki-get p *buki81-90*))
	 ((<= 91 (player-map p) 100)
	  (buki-get p *buki91-100*))))
      (1 ;;ハンマーゲット
       (hummer-get p)))))

;;プレイヤーの場所更新
(defun update-player-pos (p x y map)
  (setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 1)
  (setf (aref map (player-posy p) (player-posx p)) 0)
  (setf (player-posy p) (+ (player-posy p) y)
	(player-posx p) (+ (player-posx p) x)))
;;ラストマプ
(defun set-lastmap (map p)
  (set-map map *map100*)
  (setf (player-posx p) 5
	(player-posy p) 9))
;;移動後のマップ更新
(defun update-map (map p y x)
  (case (aref map (+ (player-posy p) y) (+ (player-posx p) x))
    (30 ;;壁
     (if (and (> (player-hammer p) 0)
	      (> (- *tate* 1) (+ (player-posy p) y) 0)
	      (> (- *yoko* 1) (+ (player-posx p) x) 0))
	 (kabe-break map p y x)
	 (format t "「そっちには移動できません！！」~%")))
    (4 ;;薬
     (format t "「回復薬を手に入れた！」~%")
     (incf (player-heal p))
     (update-player-pos p x y map))
    (2 ;;くだり階段
     (if (= (player-map p) 99)
	 (set-lastmap map p)
	 (set-map map (maze p)))
     (incf (player-map p))
     (incf (player-hammer p))
     (if (= (mod (player-map p) 5) 0)
	 (incf *monster-level*)))
    (3 ;;宝箱
     (item-get p)
     (update-player-pos p x y map))
    (5 ;;ボス
     (update-player-pos p x y map)
     (setf *battle?* t
	   *boss?* t))
    (otherwise
     (update-player-pos p x y map)
     (if (= (randval 10) 1) ;;敵との遭遇確率
	 (setf *battle?* t)))))
;;薬を使う
(defun use-heal (p)
  (cond
    ((>= (player-heal p) 1)
     (format t "~%「回復薬を使った。」~%")
     (decf (player-heal p))
     (setf (player-hp p)  (player-maxhp p)
	   (player-agi p) (player-maxagi p)
	   (player-str p) (player-maxstr p)))
    (t
      (format t "~% 「回復薬を持っていません！」~%"))))
;;裏ワザ
(defun urawaza (p)
  (format t "~%「神の力を授かった！」~%")
  (setf (player-hp p)     999
	(player-maxhp p)  999
	(player-agi p)    999
	(player-maxagi p) 999
	(player-str p)    999
	(player-maxstr p) 999))
;;移動先選択
(defun map-move (map p)
  (unless *battle?*
    ;;(show-fog-map map p)
    (show-map map p)
    ;;(format t "~%どちらに移動しますか？[u]上 [d]下 [r]右 [l]左 [q]薬を使う: ")
    (case (read)
      (u (update-map map p -1 0))
      (d (update-map map p 1 0))
      (r (update-map map p 0 1))
      (l (update-map map p 0 -1))
      (q (use-heal p))
      (mogezouisgod (urawaza p))
      (otherwise
       (format t "u,d,r,l,qの中から選んでください！~%")))

    (map-move map p)))
