(load "item.lisp" :external-format :utf-8)

(defparameter *tate* 11) ;;マップサイズ
(defparameter *yoko* 11)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
;;宝箱から出る武器の確率 *buki*に対応
(defparameter *battle?* nil)
(defparameter *monster-num* 6)
(defparameter *monster-level* 1) ;;階数によるモンスターのレベル
(defparameter *boss?* 0)
(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *start-time* 0)
(defparameter *ha2ne2* nil)
(defparameter *copy-buki* (copy-tree *buki-d*))

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
  (msg nil)
  (monster-num 0))

(defstruct donjon
  (map nil) ;; (make-array (list *tate* *yoko*))) ;;マップ
  (tate 11)
  (yoko 11)
  (stop-list nil)) ;;行き止まりリスト

(defun init-data ()
  (setf *battle?* nil
	*monster-num* 6
	*monster-level* 1
	*boss?* 0
	*end* 0
	*lv-exp* 100
	*start-time* (get-internal-real-time)
	*ha2ne2* nil
	*copy-buki* (copy-tree *buki-d*)))

(defun game-over-message (p)
  (scr-format "Game Over.~%")
  (scr-format "あなたは地下~d階で力尽きた。~%" (player-map p))
  (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%")
  (case (read-command-char)
    (1 (main))
    (2 nil)
    (otherwise (game-over-message p))))
;;バトル開始
(defun orc-battle (p)
  ;;(scr-format "~%敵が現れた！！~%")
  (init-monsters p)
  ;;(init-player)
  (game-loop p)
  (gamen-clear)
  (show-monsters2)
  (setf *battle?* nil)
  (scr-format "~%~%")
  (when (player-dead p)
    (game-over-message p))
  (when (monsters-dead)
    (loop while (>= (player-exp p) *lv-exp*)
	  do (scr-format "「レ ベ ル ア ッ プ ！ ！」~%")
	     (incf (player-level p))
	     (incf (player-maxhp p) 3)
	     (incf (player-maxagi p) 1)
	     (incf (player-maxstr p) 1)
	     (setf (player-hp p) (player-maxhp p)
		   (player-agi p) (player-maxagi p)
		   (player-str p) (player-maxstr p)
		   (player-exp p) (- (player-exp p) *lv-exp*))
	     (incf *lv-exp* 10))
    (scr-format "「大 勝 利 ！」~%~%")
    (scr-format "次へ = z")
    (read-command-char)))
;;ボスバトル
(defun boss-battle (p)
  (scr-format "~%もげぞうが現れた！！~%")
  (boss-monsters p 0)
  ;;(init-player)
  (game-loop p)
  (setf *battle?* nil)
  (when (player-dead p)
    (game-over-message p))
  (when (monsters-dead)
    (scr-format "「大勝利！」~%~%")
    (setf *end* 1)))
;;ハツネツバトル
(defun ha2ne2-battle (p)
  (scr-format "~%ハツネツエリアが現れた！！~%")
  (boss-monsters p 1)
  ;;(init-player)
  (game-loop p)
  (setf *battle?* nil)
  (when (player-dead p)
    (game-over-message p))
  (when (monsters-dead)
    (setf *boss?* 0
	  *ha2ne2* t)
    (scr-format "「大勝利！」~%")
    (scr-format "「ハツネツの剣を拾った！装備しますか？」(yes=z or no=x)~%")
    (let ((item (assoc "ハツネツの剣" *buki* :test #'equal)))
      (scr-format "現在の装備品：~a 攻撃力:~d HP:~d 素早さ:~d~%"
		  (first (player-buki p)) (second (player-buki p))
		  (third (player-buki p)) (fourth (player-buki p)))
      (scr-format "発見した装備：~a 攻撃力:~d HP:~d 素早さ:~d~%"
		  (first item) (second item) (third item) (fourth item))
      (case (read-command-char)
        (z (equip-buki item p))
        (otherwise (scr-format "ハツネツの剣を捨てた。~%"))))))

;;バトル時、プレイヤーが死ぬかモンスターが全滅するまでループ
(defun game-loop (p)
  (unless (or (player-dead p) (monsters-dead))
    
    (dotimes (k (1+ (truncate (/ (max 0 (player-agi p)) 15))))
      (unless (monsters-dead)
	(gamen-clear)
	(show-monsters2)
	(show-player p)
        (player-attack p)))
    (cond 
      ((null (monsters-dead))
       (gamen-clear)
       (show-monsters2)
       (show-player p)
       (scr-format "~%-------------敵のターン----------------~%")
       (map 'list
            (lambda (m)
              (or (monster-dead m) (monster-attack m p)))
            *monsters*)
       (scr-format "~%次へ = z~%")
       (read-command-char)
       (game-loop p)))))

;;プレイヤーの生死判定
(defun player-dead (p)
  (<= (player-hp p) 0))
;;プレイヤーのステータス表示(バトル時)
(defun show-player (p)
  (scr-format "~%あなたのステータス:HP ~d, 素早さ ~d, 力 ~d,~%"
          (player-hp p) (player-agi p) (player-str p)) 
  (scr-format "持ち物:回復薬 ~d個~%" (player-heal p)))
;;
(defun atack-p (p x)
  (let ((m (pick-monster p)))
    ;;(scr-fresh-line)
    ;;(scr-format "-------------あなたの攻撃--------------~%")
    (monster-hit2 p m x)))
;;攻撃方法
(defun player-attack (p)
  (scr-fresh-line)
  ;;(show-player p)
  (scr-format "攻撃方法: [z]突く [x]ダブルスウィング [c]なぎ払う [v]待機 [q]回復薬を使う:~%")
  (case (read-command-char)
    (z (atack-p p (+ 2 (randval (ash (player-str p) -1)))))
    (x (let ((x (randval (truncate (/ (player-str p) 6)))))
	 (scr-princ "ダブルスウィングのダメージは ")
	 (scr-princ x)
	 (scr-fresh-line)
	 (atack-p p x)
	 (show-monsters2)
	 (unless (monsters-dead)
	   (atack-p p x))))
    (c
     ;;(scr-fresh-line)
     ;;(scr-format "-------------あなたの攻撃--------------~%")
     (dotimes (x (1+ (randval (truncate (/ (player-str p) 3)))))
       (unless (monsters-dead)
	 (monster-hit2 p (random-monster) 1))))
    (v
     (scr-fresh-line)
     (scr-format "ぼけ〜~%"))
    (q (use-heal p))
    (otherwise
     (scr-format "z,x,c,v,qの中から選んでください！~%")
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

(defun auto-pick-monster (num)
  (let ((m (aref *monsters* num)))
    (if (null (monster-dead m))
	m
	(auto-pick-monster (1+ num)))))
;;モンスター選択
(defun pick-monster (p)
  (scr-fresh-line)
  (scr-princ "攻撃したいモンスター番号を選択 #:")
  (scr-fresh-line)
  (let ((key (read-command-char)))
    (case key
      (z (auto-pick-monster 0))
      (otherwise
       (let ((x (ascii->number key)))
	 (if (not (and (integerp x) (>= x 1) (<= x (player-monster-num p))))
	     (progn (scr-princ "有効なモンスター番号ではありません。")
		    (pick-monster p))
	     (let ((m (aref *monsters* (1- x))))
	       (if (monster-dead m)
		   (progn (scr-princ "そのモンスターはすでに死んでます。")
			  (pick-monster p))
		   m))))))))

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
(defun boss-monsters (p m)
  (let ((hoge 0))
    (setf *monsters*
	  (map 'vector
	       (lambda (x)
		 (if (= hoge 0)
		     (progn (incf hoge)
			    (cond
			      ((= m 0) (make-boss :health 300))
			      ((= m 1) (make-ha2ne2 :health 220))))
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
(defun show-monsters2 ()
  (scr-fresh-line)
  (scr-format "---------------------------------------~%")
  (scr-format "敵:~%")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (scr-format "  ~c. "  (number->a (incf x)))
	   (if (monster-dead m)
	       (if (> (monster-damage m) 0)
		   (scr-format "**死亡**   ~d のダメージを与え倒した！~%" (monster-damage m))
		   (scr-format "**死亡**~%")) 
	       (progn (monster-show m)
		      (if (> (monster-damage m) 0)
			  (scr-format "   ~d のダメージを与えた！~%" (monster-damage m))
			  (scr-fresh-line))))
	   (setf (monster-damage m) 0))
	 *monsters*)))

(defstruct monster
  (health (randval (+ 10 *monster-level*)))
  (damage  0))

(defmethod monster-hit2 (p m x)
  (decf (monster-health m) x)
  (incf (monster-damage m) x)
  ;;倒したら経験値取得
  (if (monster-dead m)
      (case (type-of m)
        (ha2ne2
	 (incf (player-exp p) 99))
	(orc
	 (incf (player-exp p) 2))
	(slime-mold
	 (incf (player-exp p) 3))
	(hydra
	 (incf (player-exp p) 4))
	(brigand
	 (incf (player-exp p) 5))
	(yote1
	 (incf (player-exp p) 100)))))


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
       (let ((dame-str (- (player-str p) x)))
	 (if (= (player-str p) 0)
	     (progn (scr-format "「ネコPパンチ。HPが~d下がった。」~%" x)
		    (decf (player-hp p) x))
	     (if (>= dame-str 0)
		 (progn (scr-format "「ネコPパンチ。力が~d下がった。」~%" x)
			(decf (player-str p) x))
		 (progn (scr-format "「ネコPパンチ。力が~d下がった。」~%" (player-str p))
			(setf (player-str p) 0))))))
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
       (let ((dame-agi (- (player-agi p) x)))
	 (if (= (player-agi p) 0)
	     (progn (scr-format "「もげぞうの攻撃。~dのダメージをくらった。」~%" x)
		    (decf (player-hp p) x))
	     (if (>= dame-agi 0)
		 (progn (scr-format "「もげぞうの不思議な踊り。素早さが~d下がった。」~%" x)
			(decf (player-agi p) x))
		 (progn (scr-format "「もげぞうの不思議な踊り。素早さが~d下がった。」~%" (player-agi p))
			(setf (player-agi p) 0))))))
      (2
       (let ((dame-agi (- (player-agi p) x))
	     (dame-str (- (player-str p) x)))
	 (scr-format "「もげぞうのなんかすごい攻撃！すべてのステータスが~d下がった！」~%" x)
	 (decf (player-hp p) x)
	 (if (>= dame-agi 0)
	     (decf (player-agi p) x)
	     (setf (player-agi p) 0))
	 (if (>= dame-str 0)
	     (decf (player-str p) x)
	     (setf (player-str p) 0)))))))
;;-------------------メタル------------------------------
(defstruct (yote1 (:include monster))
  (atk    (randval (+ 10 *monster-level*))))
;;(push #'make-yote1 *monster-builders*)

(defmethod monster-show ((m yote1))
  (scr-princ "メタルヨテイチ"))

(defmethod monster-attack ((m yote1) (p player))
  (let ((atk (randval (yote1-atk m))))
    (case (random 2)
      (0 (scr-format "「メタルヨテイチは何もしていない。」~%"))
      (1 (scr-format "「メタルヨテイチが突然殴り掛かってきた。~dのダメージを受けた。」~%" atk)
       (decf (player-hp p) atk)))))

(defmethod monster-hit2 ((p player) (m yote1) x)
  (decf (monster-health m))
  (incf (monster-damage m))
  (if (monster-dead m)
      (progn (incf (player-exp p) 100))))

;;-------------------オーク------------------------------
(defstruct (orc (:include monster))
  (club-level (randval (+ 8 *monster-level*)))
  (name "オーク"))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (let ((x (orc-club-level m)))
    (cond
      ((>= 3 x 1) (scr-princ "か弱いオーク"))
      ((>= 6 x 4) (scr-princ "日焼けしたオーク"))
      ((>= 9 x 7) (scr-princ "邪悪なオーク"))
      (t (scr-princ "マッチョオーク")))))

(defmethod monster-attack ((m orc) (p player))
  (let ((x (randval (orc-club-level m))))
    (monster-show m)
    (scr-format "が棍棒で殴ってきて ~d のダメージをくらった。~%" x)
    (decf (player-hp p) x)))



;;-------------------ヒドラ------------------------------
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)


(defmethod monster-show ((m hydra))
  (let ((x (monster-health m)))
    (cond
      ((>= 3 x 1)
       (scr-princ "意地悪なヒドラ"))
      ((>= 6 x 4)
       (scr-princ "腹黒いヒドラ"))
      ((>= 9 x 7)
       (scr-princ "強欲なヒドラ"))
      (t (scr-princ "グレートヒドラ")))))


(defmethod monster-attack ((m hydra) (p player))
  (let ((x (randval (ash (monster-health m) -1))))
    (monster-show m)
    (scr-format "の攻撃 ~dのダメージを食らった。~%" x)
    (monster-show m)
    (scr-format "の首が一本生えてきた！~%")
    (incf (monster-health m))
    (decf (player-hp p) x)))


;;-------------------スライム------------------------------
(defstruct (slime-mold (:include monster)) (sliminess (randval (+ 5 *monster-level*))))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (let ((x (slime-mold-sliminess m)))
    (cond
      ((<= 1 x 3) (scr-format "ベタベタなスライム"))
      ((<= 4 x 6) (scr-format "ベトベトなスライム"))
      ((<= 7 x 9) (scr-format "ベチョベチョなスライム"))
      (t (scr-format "ヌルヌルなスライム")))))

(defmethod monster-attack ((m slime-mold) (p player))
  (let ((x (randval (slime-mold-sliminess m))))
    (cond
      ((> (player-agi p) 0)
       (let ((dame-agi (- (player-agi p) x)))
	 (if (>= dame-agi 0)
	     (progn (monster-show m)
		    (scr-format "は足に絡みついてきてあなたの素早さが ~d 下がった！~%" x)
		    (decf (player-agi p) x))
	     (progn (monster-show m)
		    (scr-format "は足に絡みついてきてあなたの素早さが ~d 下がった！~%"
				(player-agi p))
		    (setf (player-agi p) 0)))))
      (t (monster-show m)
	 (scr-format "が何か液体を吐きかけてきて ~d ダメージくらった！~%" x)
	 (decf (player-hp p) x)))))

;;-------------------ブリガンド------------------------------
(defstruct (brigand (:include monster)) (atk (+ 2 (random *monster-level*))))
(push #'make-brigand *monster-builders*)

(defmethod monster-show ((m brigand))
  (let ((x (brigand-atk m)))
    (cond
      ((<= 1 x 3) (scr-format "毛の薄いブリガンド"))
      ((<= 4 x 6) (scr-format "ひげもじゃなブリガンド"))
      ((<= 7 x 9) (scr-format "胸毛の濃いブリガンド"))
      (t (scr-format "禿げてるブリガンド")))))

(defmethod monster-attack ((m brigand) (p player))
  (let ((x (max (player-hp p) (player-agi p) (player-str p)))
	(damage (brigand-atk m)))
    (monster-show m)
    (cond ((= x (player-hp p))
	   (scr-format "のスリングショットの攻撃で ~d ダメージくらった！~%" damage)
	   (decf (player-hp p) damage))
	  ((= x (player-agi p))
	   (scr-format "は鞭であなたの足を攻撃してきた！素早さが ~d 減った！~%" damage)
	   (decf (player-agi p) damage))
	  ((= x (player-str p))
	   (scr-format "は鞭であなたの腕を攻撃してきた！力が ~d 減った！~%" damage)
	   (decf (player-str p) damage)))))


;;---------------------------------------------------------------------------------------
;;マップ移動
(defun show-msg (p)
  (if (player-msg p)
      (scr-format "~a~%" (player-msg p)))
  (setf (player-msg p) nil))

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
  (gamen-clear)
  (scr-format "地下~d階~%" (player-map p))
  (scr-format "現在のステータス Lv ~d, HP ~d, 素早さ ~d, 力 ~d, exp ~d~%" (player-level p) (player-hp p)
          (player-agi p) (player-str p) (player-exp p))
  (scr-format "現在の武器:~a~%" (first (player-buki p)))
  (scr-format "持ち物:回復薬 ~d個 ハンマー~d個~%" (player-heal p) (player-hammer p))
  (loop for i from 0 below (donjon-tate map) do
    (loop for j from 0 below (donjon-yoko map) do
      (scr-format (map-type (aref (donjon-map map) i j)))
      (if (= j (- (donjon-yoko map) 1))
	  (case i
	    (0 (scr-format " 主:プレイヤーの位置~%"))
	    (2 (scr-format " 宝:宝箱~%"))
	    (1 (scr-format " 下:下り階段~%"))
	    (3 (scr-format " 薬:回復薬~%"))
	    (4 (scr-format " ボ:ボス~%"))
            (5 (scr-format " イ:イベント~%"))
            (6 (scr-format " ハ:中ボス~%"))
	    (otherwise (scr-fresh-line))))))
  (show-msg p)
  (show-map-key))
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
	 (ranking-dialog ss)
	 (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%"))
       (case (read-command-char)
         (1 (main))))
      ((= *end* 0)
       (main-game-loop map p)))))
;;ゲーム開始
(defun main ()
  (init-charms)
  (setf *random-state* (make-random-state t))
  (let* ((p (make-player))
	 (map (make-donjon)))
    (init-data)
    (init-monsters p)
    (maze map p)
    (main-game-loop map p)))

;;壁破壊
(defun kabe-break (map p y x)
  (scr-format "「ハンマーで壁を壊しますか？」[yes=z or no=x]:~%")
  (case (read-command-char)
    (z
      (if (>= (random 10) 3)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 0)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 3))
     (decf (player-hammer p)))))
     ;;(scr-format "「壁を壊しました。」~%"))))

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
  (let ((item item-a));;(assoc item-a *buki* :test #'equal)))
    (scr-format "「~aを見つけた」~%" (first item))
    (scr-format "現在の装備品：~a 攻撃力:~d HP:~d 素早さ:~d~%"
		(first (player-buki p)) (second (player-buki p))
		(third (player-buki p)) (fourth (player-buki p)))
    (scr-format "発見した装備：~a 攻撃力:~d HP:~d 素早さ:~d~%"
		(first item) (second item) (third item) (fourth item))
    (scr-format "「装備しますか？」(yes=z or no=x)~%")
    (case (read-command-char)
      (z
       (scr-format "「~aを装備した。」~%" (first item))
       (equip-buki item p))
      (x
       (scr-format "「~aを見なかったことにした。」~%" (first item)))
      (otherwise
       (equip? p item-a)))))

(defun hummer-get (p)
  (setf (player-msg p) "「ハンマーを見つけた。」")
  (incf (player-hammer p)))

(defun kusuri-get (p)
  (setf (player-msg p) "「回復薬を見つけた。」")
  (incf (player-heal p)))



;;アイテム
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;重み付け抽選
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))

      
;; '(4 3 2 1 1) → '(1 4 3 2 1)→'(1 1 4 3 2) '(1 1 1 4 3)
(defun omomin-zurashi (lst)
  (let ((buki (mapcar #'car lst))
	(omomi (mapcar #'cdr lst)))
    (setf omomi (butlast omomi))
    (push 1 omomi)
    (mapcar #'cons buki omomi)))
;;テスト用------------------------------------
#|
(defun test-pick ()
  (let ((hoge (make-array 54)))
    (dotimes (i 10000)
      (incf (aref hoge (weightpick *omomin*))))
    hoge))
(defun test-hoge ()
  (let ((x 1))
	   (loop for hoge from 0 to 53
		 collect x
		 do (incf x 1))))
|#
;;---------------------------------------------
;;武器ゲット２ 全アイテムからランダム
(defun item-get2 (p)
  (case (random 5)
    ((0 1 2)
     (equip? p (weightpick *copy-buki*)))
    (3 (hummer-get p))
    (4 (kusuri-get p))))

;;プレイヤーの場所更新
(defun update-player-pos (p x y map)
  (setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 1)
  (setf (aref map (player-posy p) (player-posx p)) 0)
  (setf (player-posy p) (+ (player-posy p) y)
	(player-posx p) (+ (player-posx p) x)))
;;マップ設定
(defun set-map (map p moto)
  (loop for i from 0 below (donjon-tate map) do
    (loop for j from 0 below (donjon-yoko map) do
      (if (= (aref moto i j) 1)
	  (setf (player-posx p) j
		(player-posy p) i))
      (setf (aref (donjon-map map) i j) (aref moto i j)))))

;;100階イベント
(defun moge-event (p)
  (if (equal (car (player-buki p)) "もげぞーの剣")
      (progn
        (scr-format "~%「もげぞーの剣が輝き出し、もげぞうの剣に進化した！」~%")
        (equip-buki (assoc "もげぞうの剣" *buki* :test #'equal) p))
      (scr-format "~%「なにも起こらなかった。」~%"))
  (scr-format "~%次へ = z~%")
  (read-command-char))
;;移動後のマップ更新
(defun update-map (map p y x)
  (case (aref (donjon-map map) (+ (player-posy p) y) (+ (player-posx p) x))
    (30 ;;壁
     (if (and (> (player-hammer p) 0)
	      (> (- (donjon-tate map) 1) (+ (player-posy p) y) 0)
	      (> (- (donjon-yoko map) 1) (+ (player-posx p) x) 0))
	 (kabe-break (donjon-map map) p y x)))
	 ;;(scr-format "「そっちには移動できません！！」~%")))
    ;;(4 ;;薬
    ;; (scr-format "「回復薬を手に入れた！」~%")
    ;; (incf (player-heal p))
    ;; (update-player-pos p x y (donjon-map map)))
    (2 ;;くだり階段
     (incf (player-map p))
     (maze map p)
     (if (= (mod (player-map p) 2) 0)
	 (incf (player-hammer p)))
     (if (= (mod (player-map p) 5) 0)
	 (setf *copy-buki* (omomin-zurashi *copy-buki*)))
     (if (= (mod (player-map p) 7) 0)
	 (incf *monster-level*)))
    (3 ;;宝箱
     (item-get2 p)
     (update-player-pos p x y (donjon-map map)))
    (5 ;;ボス
     (update-player-pos p x y (donjon-map map))
     (setf *battle?* t
	   *boss?* 1))
    (6 ;;イベント
     (update-player-pos p x y (donjon-map map))
     (moge-event p))
    (7 ;;中ボス
     (update-player-pos p x y (donjon-map map))
     (setf *battle?* t
           *boss?* 2))
    (otherwise
     (update-player-pos p x y (donjon-map map))
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

;; ランキングは (("一位の名前" 秒数) ("二位の名前" 秒数) ...) の形の属
;; 性リストで、秒数でソートされて保存される。
(defconstant +ranking-file-name+ "ranking.lisp") ; ランキングファイルの名前
(defconstant +ranking-max-length+ 10)            ; ランキングに登録するエントリーの最大数

;; 合計の秒数を (時 分 秒) のリストに変換する。
(defun total-seconds-to-hms (ss)
  (let* ((h (floor ss 3600))
         (m (floor (mod ss 3600) 60))
         (s (mod ss 60)))
    (list h m s)))

;; プレーヤー name の記録 total-seconds を ranking に登録し、新しいラ
;; ンキングデータを返す。ranking に既にプレーヤーの項目がある場合は、
;; 秒数が少なければ項目を更新する。項目の数が +ranking-max-length+ を
;; 超えると、超えた分は削除される。
(defun ranking-update (name total-seconds ranking)
  (let ((ranking1
         (stable-sort
          (if (and (assoc name ranking :test #'string-equal)
                   (< total-seconds (cadr (assoc name ranking :test #'string-equal))))
              (mapcar (lambda (entry)
                        (if (string-equal (car entry) name)
                            (list name total-seconds)
                          entry))
                      ranking)
            ;; 同じタイムは後ろに追加する。早い者勝ち。
            (append ranking (list (list name total-seconds))))
          #'< :key #'cadr)))
    ;; 最大で +ranking-max-length+ の項目を返す。
    (loop for i from 1 to +ranking-max-length+
          for entry in ranking1
          collect entry)))

;; ランキングの内容を表示する。name を指定すると該当の項目の左に矢印が
;; 表示される。
(defun ranking-show (ranking &optional name)
  (loop for place from 1 to 10
        for entry in ranking
        do
        (destructuring-bind (entry-name total-seconds) entry
          (destructuring-bind (h m s) (total-seconds-to-hms total-seconds)
            (let ((arrow (if (string-equal entry-name name) "=>" "  ")))
              (scr-format "~a ~a位 ~2,'0d:~2,'0d:~2,'0d ~a~%"
                          arrow place h m s entry-name))))))

;; ランキングを更新する。ランキングファイルからデータを読み込み、1引数
;; の関数 fun にランキングデータを渡す。fun の返り値をランキングファイ
;; ルに保存する。
;;
;; TODO: 別のプロセスがランキングを同時に変更しないようにロックすべき。
(defun ranking-transaction (fun)
  (flet ((read-ranking ()
                       (with-open-file (file +ranking-file-name+
                                             :external-format :utf8
                                             :if-does-not-exist nil)
                                       (if file
                                           (let ((buf (make-string (file-length file))))
                                             (read-sequence buf file)
                                             (read-from-string buf))
                                         ;; ランキングファイルが存在しなかった場合は空のデータを返す。
                                         '())))
         (write-ranking (ranking)
                        (with-open-file (file +ranking-file-name+
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                                        (format file "~S" ranking))))
    (let ((ranking (read-ranking)))
        (write-ranking (funcall fun ranking)))))

;; メッセージ message を表示し、ユーザーから 1 あるいは 2 を受け取る。
;; 1 を受け取れば t を、さもなくば nil を返す。
(defun yes-no-dialog (message)
  (scr-format "~a(yes=1 or no=2)~%" message)
  (= 1 (read-command-char)))

;; クリア記録 total-seconds をランキングファイルへ登録時のダイアログ。
(defun ranking-dialog (total-seconds)
  (when (yes-no-dialog "ランキングに登録しますか？")
    (scr-format "名前を教えてください:~%")
    (let ((name (read-string)))
      (ranking-transaction
       (lambda (ranking)
         (let ((ranking1 (ranking-update name total-seconds ranking)))
           (if (equal ranking1 ranking)
               (progn
                 (scr-format "ランキングに入りませんでした。~%")
                 (ranking-show ranking)
                 ranking)
             (progn
               (scr-format "見事ランクイン！~%")
               (ranking-show ranking1 name)
               ranking1))))))))
