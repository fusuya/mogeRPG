(load "item.lisp" :external-format :utf-8)


(defparameter *battle?* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 6)
(defparameter *monster-level* 0) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *tate* 11) ;;マップサイズ
(defparameter *yoko* 11)
(defparameter *lv-exp* 100)
(defparameter *start-time* 0)
(defparameter *ha2ne2* nil)

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
	*boss?* 0
	*end* 0
	*ha2ne2* nil
	*start-time* (get-internal-real-time)
	*battle?* nil))

;;バトル開始
(defun orc-battle (p)
  (scr-format "~%敵が現れた！！~%")
  (init-monsters p)
  ;;(init-player)
  (game-loop p)
  (scr-fresh-line)
  (setf *battle?* nil)
  (when (player-dead p)
    (scr-format "Game Over.~%")
    (scr-format "あなたは地下~d階で力尽きた。~%" (player-map p))
    (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%")
    (case (read-command-char)
          (1 (main))))
  (when (monsters-dead)
    (loop while (>= (player-exp p) *lv-exp*)
	  do (scr-format "     「レベルアップ！！」~%")
	     (incf (player-level p))
	     (incf (player-maxhp p) 3)
	     (incf (player-maxagi p) 1)
	     (incf (player-maxstr p) 1)
	     (setf (player-hp p) (player-maxhp p)
		   (player-agi p) (player-maxagi p)
		   (player-str p) (player-maxstr p)
		   (player-exp p) (- (player-exp p) *lv-exp*))
	     (incf *lv-exp* 10))
    (scr-format "     「大勝利！」~%~%")))
;;ボスバトル
(defun boss-battle (p)
  (scr-format "~%もげぞうが現れた！！~%")
  (boss-monsters p)
  ;;(init-player)
  (game-loop p)
  (setf *battle?* nil)
  (when (player-dead p)
    (scr-format "Game Over.~%")
    (scr-format "ボスに倒された！~%")
    (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%")
    (case (read-command-char)
          (1 (main))))
  (when (monsters-dead)
    (scr-format "「大勝利！」~%~%")
    (setf *end* 1)))
;;ハツネツバトル
(defun ha2ne2-battle (p)
  (scr-format "~%ハツネツエリアが現れた！！~%")
  (ha2ne2-monsters p)
  ;;(init-player)
  (game-loop p)
  (setf *battle?* nil)
  (when (player-dead p)
    (scr-format "Game Over.~%")
    (scr-format "ハツネツエリアに倒された！~%")
    (scr-format "もう一度最初から挑戦しますか？(yes=1 or no=2)~%")
    (case (read-command-char)
          (1 (main))))
  (when (monsters-dead)
    (setf *boss?* 0
	  *ha2ne2* t)
    (scr-format "「大勝利！」~%")
    (scr-format "「ハツネツの剣を拾った！装備しますか？」(yes=1 or no=2)~%")
      (case (read-command-char)
        (1 (equip-buki (assoc "ハツネツの剣" *buki* :test #'equal) p))
        (otherwise (scr-format "ハツネツの剣を捨てた。~%")))))

;;バトル時、プレイヤーが死ぬかモンスターが全滅するまでループ
(defun game-loop (p)
  (unless (or (player-dead p) (monsters-dead))
    ;;(show-player p)
    (dotimes (k (1+ (truncate (/ (max 0 (player-agi p)) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(show-player p)
        (player-attack p)))
    (cond 
      ((null (monsters-dead))
       (scr-format "~%~%-------------敵のターン----------------~%")
       (map 'list
            (lambda (m)
              (or (monster-dead m) (monster-attack m p)))
            *monsters*)
       (scr-format "~%")
       (game-loop p)))))

;;プレイヤーの生死判定
(defun player-dead (p)
  (<= (player-hp p) 0))
;;プレイヤーのステータス表示(バトル時)
(defun show-player (p)
  (scr-format "~%~%あなたのステータス:HP ~d, 素早さ ~d, 力 ~d,~%"
          (player-hp p) (player-agi p) (player-str p)) 
  (scr-format "持ち物:回復薬 ~d個~%" (player-heal p)))
;;
(defun atack-p (p x)
  (let ((m (pick-monster p)))
    (scr-fresh-line)
    (scr-format "-------------あなたの攻撃--------------~%")
    (monster-hit p m x)))
;;攻撃方法
(defun player-attack (p)
  (scr-fresh-line)
  ;;(show-player p)
  (scr-format "攻撃方法: [1]突く [2]ダブルスウィング [3]なぎ払う [q]回復薬を使う:~%")
  (case (read-command-char)
    (1 (atack-p p (+ 2 (randval (ash (player-str p) -1)))))
    (2 (let ((x (randval (truncate (/ (player-str p) 6)))))
	 (scr-princ "ダブルスウィングのダメージは ")
	 (scr-princ x)
	 (scr-fresh-line)
	 (atack-p p x)
	 (show-monsters)
	 (unless (monsters-dead)
	   (atack-p p x))))
    (3
     (scr-fresh-line)
     (scr-format "-------------あなたの攻撃--------------~%")
     (dotimes (x (1+ (randval (truncate (/ (player-str p) 3)))))
	 (unless (monsters-dead)
	   (monster-hit p (random-monster) 1))))
    (q (use-heal p))
    (otherwise
     (scr-format "1,2,3の中から選んでください！~%")
     (player-attack p))))

(defun randval (n)
  (1+ (random (max 1 n))))

;;ランダムでモンスターを選択
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))
;;a→ 1 b→ 2 c→ 3 ...
(defun ascii->number (x)
  (if (null (numberp x))
      (- (char-code (char (symbol-name x) 0)) 64)))
;;モンスター選択
(defun pick-monster (p)
  (scr-fresh-line)
  (scr-princ "攻撃したいモンスター番号を選択 #:")
  (scr-fresh-line)
  (let ((x (ascii->number (read-command-char))))
    (if (not (and (integerp x) (>= x 1) (<= x (player-monster-num p))))
	(progn (scr-princ "有効なモンスター番号ではありません。")
	       (pick-monster p))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (scr-princ "そのモンスターはすでに死んでます。")
		     (pick-monster p))
	      m)))))

;;ランダムなモンスターグループを作る
(defun init-monsters (p)
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       ;;(funcall (nth (random (length *monster-builders*)) *monster-builders*)))
               (let ((y (random 101)))
                 (cond
                   ((<= 0 y 25) (make-orc))
                   ((<= 26 y 50) (make-hydra))
                   ((<= 51 y 75) (make-slime-mold))
                   ((<= 76 y 99) (make-brigand))
                   (t (make-yote1 :health 3)))))
	     (make-array (setf (player-monster-num p)
			       (randval (+ *monster-num* (floor (player-level p) 4))))))))
;;配列の０番目にボス、あとはランダムなモンスター
(defun boss-monsters (p)
  (let ((hoge 0))
    (setf *monsters*
	  (map 'vector
	       (lambda (x)
		 (if (= hoge 0)
		     (progn (incf hoge)
			    (make-boss :health 200))
		     (funcall (nth (random (length *monster-builders*))
				   *monster-builders*))))
	       (make-array 10)))
    (setf (player-monster-num p) 10)))
;;
(defun ha2ne2-monsters (p)
  (let ((hoge 0))
    (setf *monsters*
	  (map 'vector
	       (lambda (x)
		 (if (= hoge 0)
		     (progn (incf hoge)
			    (make-ha2ne2 :health 120))
		     (funcall (nth (random (length *monster-builders*))
				   *monster-builders*))))
	       (make-array 10)))
    (setf (player-monster-num p) 10)))

;;モンスターの生死判定
(defun monster-dead (m)
  (<= (monster-health m) 0))
;;モンスターグループが全滅したか判定
(defun monsters-dead ()
  (every #'monster-dead *monsters*))
;; a→ 97 b→ 98 c→ 99 ...
(defun number->a (x)
  (code-char (+ x 96)))
;;モンスター表示
(defun show-monsters ()
  (scr-fresh-line)
  (scr-format "---------------------------------------~%")
  (scr-princ "敵:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (scr-fresh-line)
	   (scr-princ "  ")
	   (scr-princ (number->a (incf x)))
	   (scr-princ ". ")
	   (if (monster-dead m)
	       (scr-princ "**死亡**")
	       (progn (scr-princ "(体力=")
		      (scr-princ (monster-health m))
		      (scr-princ ") ")
		      (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval (+ 10 *monster-level*))))

(defmethod monster-hit (p m x)
  (decf (monster-health m) x)
  (scr-format "「~aに ~dのダメージを与えた！」~%" (type-of m) x)
  (if (monster-dead m)
      (case (type-of m)
        (boss (scr-format "もげぞうを倒した！~%"))
        (ha2ne2
          (incf (player-exp p) 99)
          (scr-format "「ハツネツエリアを倒した！」~%"))
	(orc
	 (incf (player-exp p) 2)
	 (scr-princ "「オークを倒しました！」 "))
	(hydra
	 (incf (player-exp p) 4)
	 (scr-princ "「ヒドラを倒しました！」 "))
	(slime-mold
	 (incf (player-exp p) 3)
	 (scr-princ "「スライムを倒しました！」 "))
	(brigand
	 (incf (player-exp p) 5)
	 (scr-princ "「ブリガンドを倒しました！」 ")))))

(defmethod monster-show (m)
  (scr-princ "凶暴な ")
  (scr-princ (type-of m)))

(defmethod monster-attack (m p))
;;中ボス
(defstruct (ha2ne2 (:include monster)) (h-atk 8))
(defmethod monster-show ((m ha2ne2))
  (scr-princ "ボス：ハツネツエリア"))
(defmethod monster-attack ((m ha2ne2) (p player))
  (let ((x (+ 3 (randval (+ (player-level p) (ha2ne2-h-atk m))))))
    (case (random 3)
      (0
       (scr-format "「ハツネツの攻撃。~dのダメージをくらった。」~%" x)
       (decf (player-hp p) x))
      (1
       (scr-format "「ネコPパンチ。力が~d下がった。」~%" x)
       (decf (player-str p) x))
      (2
       (scr-format "「ハツネツが料理してご飯を食べている。ハツネツのHPが~d回復した！」~%" x)
       (incf (monster-health m) x)))))

;;ボス
(defstruct (boss (:include monster)) (boss-atk 10))
(defmethod monster-show ((m boss))
  (scr-princ "ボス：もげぞう"))
(defmethod monster-attack ((m boss) (p player))
  (let ((x (+ 5 (randval (+ (player-level p) (boss-boss-atk m))))))
    (case (random 5)
      ((0 3)
       (scr-format "「もげぞうの攻撃。~dのダメージをくらった。」~%" x)
       (decf (player-hp p) x))
      ((1 4)
       (scr-format "「もげぞうの不思議な踊り。素早さが~d下がった。」~%" x)
       (decf (player-agi p) x))
      (2
       (scr-format "「もげぞうのなんかすごい攻撃！すべてのステータスが~d下がった！」~%" x)
       (decf (player-hp p) x)
       (decf (player-agi p) x)
       (decf (player-str p) x)))))
;;-------------------メタル------------------------------
(defstruct (yote1 (:include monster))
  (atk    (randval (+ 10 *monster-level*))))
;;(push #'make-yote1 *monster-builders*)

(defmethod monster-show ((m yote1))
  (scr-princ "レアモンスター メタルヨテイチ"))

(defmethod monster-attack ((m yote1) (p player))
  (let ((atk (randval (yote1-atk m))))
    (case (random 2)
      (0 (scr-format "「メタルヨテイチは何もしていない。」~%"))
      (1 (scr-format "「メタルヨテイチが突然殴り掛かってきた。~dのダメージを受けた。」~%" atk)
       (decf (player-hp p) atk)))))

(defmethod monster-hit ((p player) (m yote1) x)
  (decf (monster-health m) (floor x x))
  (scr-format "「ヨテイチに 1のダメージを与えた！」~%")
  (if (monster-dead m)
      (progn (incf (player-exp p) 100)
             (scr-format "「ヨテイチを倒した！。」~%"))))

;;-------------------オーク------------------------------
(defstruct (orc (:include monster)) (club-level (randval (+ 8 *monster-level*))))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (scr-princ "レベル ")
  (scr-princ (orc-club-level m))
  (scr-princ " の邪悪なオーク。"))

(defmethod monster-attack ((m orc) (p player))
  (let ((x (randval (orc-club-level m))))
    (scr-princ "「オークが棍棒で殴ってきて ")
    (scr-princ x)
    (scr-princ " のダメージをくらった。」")
    (scr-fresh-line)
    (decf (player-hp p) x)))



;;-------------------ヒドラ------------------------------
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (scr-princ (monster-health m))
  (scr-princ " 本の首を持つ意地悪なヒドラ。"))

(defmethod monster-hit ((p player) (m hydra) x)
  (decf (monster-health m) x)
  (scr-format "「~aに ~dのダメージを与えた！」~%" (type-of m) x)
  (if (monster-dead m)
      (scr-princ "「首がなくなったヒドラは倒れた。」")
      (progn (scr-princ "「")
	     (scr-princ x)
	     (scr-princ " 本のヒドラの首を斬った！」 "))))

(defmethod monster-attack ((m hydra) (p player))
  (let ((x (randval (ash (monster-health m) -1))))
    (scr-princ "「ヒドラの攻撃 ")
    (scr-princ x)
    (scr-princ " のダメージを食らった。」")
    (scr-fresh-line)
    (scr-princ "「ヒドラの首が一本生えてきた！」")
    (scr-fresh-line)
    (incf (monster-health m))
    (decf (player-hp p) x)))


;;-------------------スライム------------------------------
(defstruct (slime-mold (:include monster)) (sliminess (randval (+ 5 *monster-level*))))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (scr-princ "ベタベタ度 ")
  (scr-princ (slime-mold-sliminess m))
  (scr-princ " のスライム"))

(defmethod monster-attack ((m slime-mold) (p player))
  (let ((x (randval (slime-mold-sliminess m))))
    (cond
      ((> (player-agi p) 0)
       (scr-format "「スライムは足に絡みついてきてあなたの素早さが ~d 下がった！~%" x)
       (decf (player-agi p) x)
       (if (< (player-agi p) 0)
	 (setf (player-agi p) 0)))
      (t
	(scr-format "「スライムが何か液体を吐きかけてきて ~d ダメージくらった」~%" x)
	(decf (player-hp p) x)))))

;;-------------------ブリガンド------------------------------
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand) (p player))
  (let ((x (max (player-hp p) (player-agi p) (player-str p))))
    (cond ((= x (player-hp p))
	   (scr-princ "「ブリガンドのスリングショットの攻撃で2ダメージくらった！」")
	   (scr-fresh-line)
	   (decf (player-hp p) 2))
	  ((= x (player-agi p))
	   (scr-princ "「ブリガンドは鞭であなたの足を攻撃してきた！素早さが2減った！」")
	   (scr-fresh-line)
	   (decf (player-agi p) 2))
	  ((= x (player-str p))
	   (scr-princ "「ブリガンドは鞭であなたの腕を攻撃してきた！力が2減った！」")
	   (scr-fresh-line)
	   (decf (player-str p) 2)))))


;;---------------------------------------------------------------------------------------
;;マップ移動


(defun map-type (num)
  (case num
    (30 "ロ") ;; 壁
    (0  "　")
    (1  "主") ;; プレイヤーの位置
    (4  "薬") ;; 薬
    (5  "ボ") ;;ボス
    (3  "宝") ;; 宝箱
    (2  "下") ;; 下り階段
    (6  "イ") ;; イベント
    (7  "ハ") ;; 中ボス ハツネツエリア
    ))
;;マップ表示
(defun show-map (map p)
  (scr-format "地下~d階~%" (player-map p))
  (scr-format "現在のステータス Lv ~d, HP ~d, 素早さ ~d, 力 ~d, exp ~d~%" (player-level p) (player-hp p)
          (player-agi p) (player-str p) (player-exp p))
  (scr-format "現在の武器:~a~%" (first (player-buki p)))
  (scr-format "持ち物:回復薬 ~d個 ハンマー~d個~%" (player-heal p) (player-hammer p))
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (scr-format (map-type (aref map i j)))
      (if (= j (- *yoko* 1))
	  (case i
	    (0 (scr-format " 主:プレイヤーの位置~%"))
	    (2 (scr-format " 宝:宝箱~%"))
	    (1 (scr-format " 下:下り階段~%"))
	    (3 (scr-format " 薬:回復薬~%"))
	    (4 (scr-format " ボ:ボス~%"))
            (5 (scr-format " イ:イベント~%"))
            (6 (scr-format " ハ:中ボス~%"))
	    (otherwise (scr-fresh-line))))))
  (scr-format "どちらに移動しますか？[w]上 [s]下 [d]右 [a]左 [q]薬を使う [z]終わる: ~%"))
#|
;;マップ表示 視界制限ver
(defun show-fog-map (map p)
  (scr-format "地下~d階~%" (player-map p))
  (scr-format "現在のステータス HP ~d, 素早さ ~d, 力 ~d, exp ~d~%" (player-hp p) (player-agi p)
	  (player-str p) (player-exp p))
  (scr-format "現在の武器:~a~%" (first (player-buki p)))
  (scr-format "持ち物:回復薬 ~d個 ハンマー~d個~%" (player-heal p) (player-hammer p))
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (cond
	((or (= i 0) (= i (- *tate* 1)))
	 (scr-princ (map-type (aref map i j))))
	((or (= j 0) (= j (- *yoko* 1)))
	 (scr-princ (map-type (aref map i j))))
	((and (>= (+ (player-posy p) 2) i (- (player-posy p) 2))
	      (>= (+ (player-posx p) 2) j (- (player-posx p) 2)))
	 (scr-princ (map-type (aref map i j))))
	(t
	 (scr-princ "暗")))
      (if (= j (- *yoko* 1))
	  (case i
	    (0 (scr-format " 主:プレイヤーの位置~%"))
	    (2 (scr-format " 宝:宝箱~%"))
	    (1 (scr-format " 下:下り階段~%"))
	    (3 (scr-format " 薬:回復薬~%"))
	    (4 (scr-format " 暗:見えてない場所~%"))
	    (otherwise (scr-fresh-line)))))))
|#
;;マップ設定
(defun set-map (map moto)
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (setf (aref map i j) (aref moto i j)))))
;;プレイヤーが死ぬか先頭に入るまでループ
(defun main-game-loop (map p)
  (unless (player-dead p)
    (map-move map p)
    (if *battle?*
        (cond
          ((= *boss?* 1)
            (boss-battle p))
          ((= *boss?* 2)
           (ha2ne2-battle p))
          ((= *boss?* 0)
            (orc-battle p))))
    (cond
      ((= *end* 1)
       (let* ((ss (floor (- (get-internal-real-time) *start-time*) 1000))
	      (h (floor ss 3600))
	      (m (floor (mod ss 3600) 60))
	      (s (mod ss 60)))
	 (if *ha2ne2*
	     (scr-format "~%「あなたは見事もげぞうの迷宮を完全攻略した！」~%")
	     (scr-format "~%「もげぞうを倒したが、逃したハツネツエリアが新たな迷宮を作り出した・・・」~%
「が、それはまた別のお話。」~%"))
	 (scr-format "クリアタイムは~2,'0d:~2,'0d:~2,'0d でした！~%" h m s)
	 (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%"))
       (case (read-command-char)
         (1 (main))))
      ((= *end* 0)
       (main-game-loop map p)))))

(defun main ()
  (init-charms)
  (setf *random-state* (make-random-state t))
  (let* ((p (make-player))
	 (map (maze p)))
    (init-data)
    (init-monsters p)
    (main-game-loop map p)))

;;壁破壊
(defun kabe-break (map p y x)
  (scr-format "「ハンマーで壁を壊しますか？」[yes=1 or no=2]:~%")
  (case (read-command-char)
    (1
      (if (>= (random 10) 3)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 0)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 3))
     (decf (player-hammer p))
     (scr-format "「壁を壊しました。」~%"))))

;;武器装備してステータス更新
(defun equip-buki (item p)
  (incf (player-hp p)     (- (third item) (third (player-buki p))))
  (incf (player-maxhp p)  (- (third item) (third (player-buki p))))
  (incf (player-str p)    (- (second item) (second (player-buki p))))
  (incf (player-maxstr p) (- (second item) (second (player-buki p))))
  (incf (player-agi p)    (- (fourth item) (fourth (player-buki p))))
  (incf (player-maxagi p) (- (fourth item) (fourth (player-buki p))))
  (setf (player-buki p) item))



;;見つけた武器を装備するか
(defun equip? (p item-a)
  (let ((item (assoc item-a *buki* :test #'equal)))
  (scr-format "「~aを見つけた」~%" (first item))
  (scr-format "現在の装備品：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	  (first (player-buki p)) (second (player-buki p)) (third (player-buki p)) (fourth (player-buki p)))
  (scr-format "発見した装備：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	  (first item) (second item) (third item) (fourth item))
  (scr-format "「装備しますか？」(yes=1 or no=2)~%")
  (case (read-command-char)
    (1
     (scr-format "「~aを装備した。」~%" (first item))
     (equip-buki item p))
    (2
     (scr-format "「~aを見なかったことにした。」~%" (first item)))
    (otherwise
     (equip? p item-a)))))

(defun hummer-get (p)
  (scr-format "「ハンマーを見つけた。」~%")
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
  (let ((x (random 4)))
    (case x
      ((0 2 3) ;;武器ゲット
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
;;bossマップセット
(defun set-bossmap (map p boss-map)
  (set-map map boss-map)
  (setf (player-posx p) 5
	(player-posy p) 9))
;;100階イベント
(defun moge-event (p)
  (if (equal (car (player-buki p)) "もげぞーの剣")
      (progn
        (scr-format "~%「もげぞーの剣が輝き出し、もげぞうの剣に進化した！」~%")
        (equip-buki (assoc "もげぞうの剣" *buki* :test #'equal) p))
      (scr-format "「なにも起こらなかった。」~%")))
  ;;(scr-format "enterを押してください。~%")
  ;;(read-command-char))
;;移動後のマップ更新
(defun update-map (map p y x)
  (case (aref map (+ (player-posy p) y) (+ (player-posx p) x))
    (30 ;;壁
     (if (and (> (player-hammer p) 0)
	      (> (- *tate* 1) (+ (player-posy p) y) 0)
	      (> (- *yoko* 1) (+ (player-posx p) x) 0))
	 (kabe-break map p y x)
	 (scr-format "「そっちには移動できません！！」~%")))
    (4 ;;薬
     (scr-format "「回復薬を手に入れた！」~%")
     (incf (player-heal p))
     (update-player-pos p x y map))
    (2 ;;くだり階段
     (cond
       ((= (player-map p) 99)
        (set-bossmap map p *map100*))
       ((= (player-map p) 49)
        (set-bossmap map p *map50*))
       (t
        (set-map map (maze p))))
     (incf (player-map p))
     (if (= (mod (player-map p) 2) 0)
	 (incf (player-hammer p)))
     (if (= (mod (player-map p) 10) 0)
	 (incf *monster-level*)))
    (3 ;;宝箱
     (item-get p)
     (update-player-pos p x y map))
    (5 ;;ボス
     (update-player-pos p x y map)
     (setf *battle?* t
	   *boss?* 1))
    (6 ;;イベント
     (update-player-pos p x y map)
     (moge-event p))
    (7 ;;中ボス
     (update-player-pos p x y map)
     (setf *battle?* t
           *boss?* 2))
    (otherwise
     (update-player-pos p x y map)
     (if (= (randval 13) 1) ;;敵との遭遇確率
	 (setf *battle?* t)))))
;;薬を使う
(defun use-heal (p)
  (cond
    ((>= (player-heal p) 1)
     (scr-format "~%「回復薬を使った。」~%")
     (decf (player-heal p))
     (setf (player-hp p)  (player-maxhp p)
	   (player-agi p) (player-maxagi p)
	   (player-str p) (player-maxstr p)))
    (t
      (scr-format "~% 「回復薬を持っていません！」~%"))))



