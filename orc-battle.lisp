(load "item.lisp" :external-format :utf-8)

(defparameter *tate* 11) ;;マップサイズ
(defparameter *yoko* 11)
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)

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
  (item nil) ;;持ち物リスト
  (drop nil) ;;敵からのドロップ品一時保管場所
  (auto-heal nil)
  (monster-num 0)) ;;戦闘時の敵の総数

(defstruct donjon
  (map nil)  ;;マップ
  (tate 11)  ;;縦幅
  (yoko 11)  ;;横幅
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

;;コンティニューメッセージ
(defun continue-message ()
  (scr-format "もう一度挑戦しますか？(yes=1 or no=2)~%")
  (case (read-command-char)
    (1 (main))
    (2 nil)
    (otherwise (continue-message))))
;;ゲームオーバーメッセージ
(defun game-over-message (p)
  (scr-format "Game Over.~%")
  (scr-format "あなたは地下~d階で力尽きた。~%" (player-map p))
  ;;(ranking-dialog 0)
  (continue-message))
 
  
;;戦闘終了後レベルアップ
(defun level-up (p)
  (labels 
      ((hoge (n) ;;ポイント振り分け作業
	 (if (= n 0)
	     (progn (setf (player-hp p) (player-maxhp p)
			  (player-str p) (player-maxstr p)
			  (player-agi p) (player-maxagi p))
		    (scr-format "1:HP ~d 2:力 ~d 3:素早さ ~d~%"
				(player-maxhp p) (player-maxstr p) (player-maxagi p)))
	     (progn
	       (scr-format "ポイントを振り分けてください。残り~dポイント~%" n)
	       (scr-format "(番号を選んでね)~%")
	       (scr-format "1:HP ~d 2:力 ~d 3:素早さ ~d~%"
			   (player-maxhp p) (player-maxstr p) (player-maxagi p))
	       (let ((x (read-command-char)))
		 (scr-fresh-line)
		 (case x
		   (1
		    (incf (player-maxhp p))
		    (hoge (1- n)))
		   (2
		    (incf (player-maxstr p))
		    (hoge (1- n)))
		   (3
		    (incf (player-maxagi p))
		    (hoge (1- n)))
		   (otherwise
		    (hoge n))))))))
    (loop while (>= (player-exp p) *lv-exp*) do
      (let ((point (randval 3)))
	(scr-format "「レベルアップ！ステータスポイントを~d獲得しました。」~%" point)
	(hoge point)
	(decf (player-exp p) *lv-exp*)
	(incf (player-level p))
	(incf *lv-exp* 10)))))
;;戦闘終了後アイテム入手
(defun item-drop? (p)
  (dolist (item (player-drop p))
    (let ((buki (assoc item *event-buki* :test #'equal)))
      (cond
	(buki (equip? p buki))
	((string= item "ハンマー")
	 (scr-format "「ハンマーを拾った！」~%")
	 (incf (player-hammer p)))
	((string= item "回復薬")
	 (scr-format "「回復薬を拾った！」~%")
	 (incf (player-heal p))))
      (setf (player-drop p) nil)))) ;;ドロップ品を消す
;;バトル開始
(defun orc-battle (p)
  (cond ;;モンスターズ作成
    ((= *boss?* 1) ;;ラスボス
     (boss-monsters p 0))
    ((= *boss?* 2) ;;中ボス
     (boss-monsters p 1))
    ((= *boss?* 0) ;;雑魚
     (init-monsters p)))
  (game-loop p) ;;バトルループ
  (gamen-clear)
  (show-monsters2)
  (scr-format "~%~%")
  (cond
    ((player-dead p) ;;プレイヤーが死んだとき
     (game-over-message p)
     (setf *end* 2))
    (t ;;(monsters-dead) 敵を倒したとき
     (item-drop? p) ;;アイテム入手処理
     (level-up p) ;;レベルアップ処理
     (cond
       ((= *boss?* 1) (setf *end* 1)) ;;ラスボスならエンディングへ
       ((= *boss?* 2) (setf *ha2ne2* t))) ;;中ボス倒したフラグ
     (scr-format "「大 勝 利 ！」~%~%")
     (scr-format "次へ = z")
     (read-command-char)
     ;;バトルフラグとボスフラグを初期化
     (setf *battle?* nil
	   *boss?* 0))))
;;オートヒール発動
(defun use-auto-heal (p)
  (use-heal p)
  (scr-format "HPが最大HPの~d%以下だったので回復薬を使いました。~%" (player-auto-heal p))
  (scr-format "次へ = z")
  (read-command-char))
;;バトル時、プレイヤーが死ぬかモンスターが全滅するまでループ
(defun game-loop (p)
  (unless (or (player-dead p) (monsters-dead))
    (dotimes (k (1+ (truncate (/ (max 0 (player-agi p)) 15))))
      (unless (monsters-dead)
	(gamen-clear)
	(show-monsters2)
	(show-player p)
	;;オート回復がONになっていて回復薬を一つ以上持っていてオート回復薬の条件にあっていれば
	(if (and (player-auto-heal p) (> (player-heal p) 0)
		 (>= (* (player-maxhp p) (/ (player-auto-heal p) 100)) (player-hp p)))
	    (use-auto-heal p)
	    (player-attack p))))
    (cond 
      ((null (monsters-dead))
       (gamen-clear)
       (show-monsters2)
       (show-player p)
       (scr-format "~%------------------------敵のターン------------------------~%")
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
  (scr-format "~%あなたのステータス:Lv ~d, HP ~d, 素早さ ~d, 力 ~d, exp ~d~%"
          (player-level p) (player-hp p) (player-agi p) (player-str p) (player-exp p)) 
  (scr-format "持ち物:回復薬 ~d個,ハンマー~d個~%" (player-heal p) (player-hammer p)))
;;
(defun atack-p (p x)
  (let ((m (pick-monster p)))
    (monster-hit2 p m x)))
;;攻撃方法
(defun player-attack (p)
  (scr-fresh-line)
  ;;(show-player p)
  (scr-format "攻撃方法: [z]突く [x]ダブルスウィング [c]なぎ払う [v]待機 [q]回復薬を使う:~%")
  (case (read-command-char)
    (z (atack-p p (+ 2 (randval (ash (player-str p) -1)))))
    (x (let ((x (randval (truncate (/ (player-str p) 6)))))
	 (scr-format "~%ダブルスウィングのダメージは ~d~%" x)
	 (atack-p p x)
	 (unless (monsters-dead)
           (scr-format "~%2体目のモンスターを選んでください~%")
	   (atack-p p x))))
    (c
     (dotimes (x (1+ (randval (truncate (/ (player-str p) 3)))))
       (unless (monsters-dead)
	 (monster-hit2 p (random-monster) 1))))
    (v
     (scr-fresh-line))
    (q (use-heal p))
    (otherwise
     (scr-format "z,x,c,v,qの中から選んでください！~%")
     (player-attack p))))

;;n内の１以上の乱数
(defun randval (n)
  (1+ (random (max 1 n))))

;;ランダムでモンスターを選択
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))
;;a→ 0 b→ 1 c→ 2 ...
(defun ascii->number (x)
  (if (null (numberp x))
      (- (char-code (char (symbol-name x) 0)) 65)))
;;生きている一番上の敵を選択
(defun auto-pick-monster (num)
  (let ((m (aref *monsters* num)))
    (if (null (monster-dead m))
	m
	(auto-pick-monster (1+ num)))))
;;モンスター選択
(defun pick-monster (p)
  (scr-fresh-line)
  (scr-princ "攻撃したいモンスター番号を選択(z=自動) #:")
  (scr-fresh-line)
  (let ((key (read-command-char)))
    (case key
      (z (auto-pick-monster 0))
      (otherwise
       (let ((x (ascii->number key)))
	 (if (not (and (integerp x) (>= x 0) (< x (player-monster-num p))))
	     (progn (scr-princ "有効なモンスター番号ではありません。")
		    (pick-monster p))
	     (let ((m (aref *monsters* x)))
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
		 ;;モンスターの出現率
                 (cond
                   ((<= 0 y 25) (make-orc))
                   ((<= 26 y 50) (make-hydra))
                   ((<= 51 y 75) (make-slime-mold))
                   ((<= 76 y 99) (make-brigand))
                   (t (make-yote1 :health 3)))))
	     (make-array (setf (player-monster-num p)
			       (randval (+ *monster-num* (floor (player-level p) 4))))))))
;;配列の０番目にボス、あとはランダムなモンスター(m=0,もげぞう m=1,ハツネツ)
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
;; 1->a 2->b 3->c ...
(defun number->a (x)
  (code-char (+ x 96)))
;;モンスター表示
(defun show-monsters2 ()
  (scr-fresh-line)
  (scr-format "-----------------------敵が現れた！-------------------------~%")
  (scr-format "敵:~%")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (scr-format "  ~c. "  (number->a (incf x)))
	   (if (monster-dead m)
	       (if (> (monster-damage m) 0)
		   (scr-format "**死亡**   ~d のダメージを与え倒した！~%" (monster-damage m))
		   (scr-format "**死亡**~%"))
	       (progn (scr-format "(HP=~d) " (monster-health m))
		      (monster-show m)
		      (if (> (monster-damage m) 0)
			  (scr-format "   ~d のダメージを与えた！~%" (monster-damage m))
			  (scr-fresh-line))))
	   (setf (monster-damage m) 0))
	 *monsters*)))

(defstruct monster
  (health (randval (+ 10 *monster-level*)))
  (damage  0))

;;-----------------敵からのアイテムドロップ-------------------------
(defun yote1-drop (p)
  (if (= 1 (random 100))
      (push "メタルヨテイチの剣" (player-drop p))))
(defun ha2ne2-drop (p)
  (if (= 0 (random 1)) ;;100%
      (push "ハツネツの剣" (player-drop p))))

(defun orc-drop (p)
  (if (= 1 (random 10))
      (push "ハンマー" (player-drop p))))
(defun slime-drop (p)
  (if (= 1 (random 20))
      (push "回復薬" (player-drop p))))
;;-----------------------------------------------------------------
;;モンスターの受けたダメージ処理
(defmethod monster-hit2 (p m x)
  (decf (monster-health m) x)
  (incf (monster-damage m) x)
  ;;倒したら経験値取得
  (if (monster-dead m)
      (case (type-of m)
        (ha2ne2
	 (ha2ne2-drop p)
	 (incf (player-exp p) 99))
	(orc
	 (orc-drop p)
	 (incf (player-exp p) 2))
	(slime-mold
	 (slime-drop p)
	 (incf (player-exp p) 3))
	(hydra
	 (incf (player-exp p) 4))
	(brigand
	 (incf (player-exp p) 5)))))


(defmethod monster-attack (m p))
;;--------中ボス------------------------------------------------------------------------
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

;;--------ボス------------------------------------------------------------------------
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

;;-------------------メタルヨテイチ--------------------------------------------------
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
      (progn (incf (player-exp p) 100)
	     (yote1-drop p))))

;;-------------------オーク---------------------------------------------------------
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

;;-----------------------マップ------------------------------------------------------------
;;---------------------------------------------------------------------------------------
;;マップ移動
(defun show-msg (p)
  (if (player-msg p)
      (scr-format "~a~%" (player-msg p)))
  (setf (player-msg p) nil))
;;オート回復薬メッセージ
(defun show-auto-heal (p)
  (if (null (player-auto-heal p))
      (scr-format "オート回復薬 = OFF~%")
      (scr-format "オート回復薬 = HPが~d%以下で回復~%" (player-auto-heal p))))

;;文字幅取得
(defun moge-char-width (char)
    (if (<= #x20 (char-code char) #x7e)
        1
	2))
;;string全体の文字幅
(defun string-width (string)
  (apply #'+ (map 'list #'moge-char-width string)))
;;最低n幅もったstring作成
(defun minimum-column (n string)
  (let ((pad (- n (string-width string))))
    (if (> pad 0)
	(concatenate 'string string (make-string pad :initial-element #\ ))
        string)))
;;持ち物表示
(defun show-item (p)
  (gamen-clear)
  (loop for buki in (player-item p)
	for x from 1 do
	  (scr-format "[~c]:~a:力+~2,'0d HP+~2,'0d 素早さ+~2,'0d~%"
		      (number->a x) (minimum-column 18 (first buki)) (second buki)
		      (third buki) (fourth buki)))
  (scr-format "アルファベットを選ぶと装備します~%")
  (scr-format "[z]戻る")
  (let ((x (ascii->number (read-command-char))))
    (cond
      ((and (integerp x) (<= 0 x 24) (< x (length (player-item p))))
       (let ((buki (nth x (player-item p))))
	 (if (not (string= "なし" (first (player-buki p))))
	     (push (player-buki p) (player-item p)))
	 (equip-buki buki p)
	 (setf (player-item p) (remove buki (player-item p) :count 1 :test #'equal))))
      ((and (integerp x) (= x 25)) ;;zキーで戻る
       )
      (t
       (show-item p)))))
    
	
	   
    
;;オート回復薬設定
(defun auto-heal-config (p)
  (gamen-clear)
  ;; now = 現在の設定番号
  (let ((now (if (null (player-auto-heal p))
		 5
		 (floor (player-auto-heal p) 10))))
    (scr-format "---------------オート回復薬の設定---------------~%")
    (scr-format "現在の設定 : ~d番~%" now)
    (scr-format "1 : HPが10%以下で回復~%")
    (scr-format "2 : HPが20%以下で回復~%")
    (scr-format "3 : HPが30%以下で回復~%")
    (scr-format "4 : HPが40%以下で回復~%")
    (scr-format "5 : OFF~%")
    (scr-format "設定したい数字を選んでください~%")
    (case (read-command-char)
      (1 (setf (player-auto-heal p) 10))
      (2 (setf (player-auto-heal p) 20))
      (3 (setf (player-auto-heal p) 30))
      (4 (setf (player-auto-heal p) 40))
      (5 (setf (player-auto-heal p) nil))
      (otherwise (auto-heal-config p)))))

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
  (show-player p)
  (scr-format "現在の武器:~a~%" (first (player-buki p)))
  (show-auto-heal p)
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
;;エンディング
(defun ending ()
  (let* ((ss (floor (- (get-internal-real-time) *start-time*) 1000))
	 (h (floor ss 3600))
	 (m (floor (mod ss 3600) 60))
	 (s (mod ss 60)))
    (if *ha2ne2*
	(scr-format "~%「あなたは見事もげぞうの迷宮を完全攻略した！」~%")
	(progn (scr-format "~%「もげぞうを倒したが、逃したハツネツエリアが新たな迷宮を作り出した・・・」~%")
	       (scr-format "「が、それはまた別のお話。」~%")))
    (scr-format "クリアタイムは~2,'0d:~2,'0d:~2,'0d でした！~%" h m s)
    (ranking-dialog ss)
    (continue-message)))
;;プレイヤーが死ぬか戦闘に入るか*end*=2になるまでループ
(defun main-game-loop (map p)
  (unless (or (= *end* 2) (player-dead p))
    (map-move map p)
    (if *battle?*
	(orc-battle p))
    (cond
      ((= *end* 1) ;;ゲームクリア
       (ending))
      ((= *end* 0) ;;ゲームループ
       (main-game-loop map p)))))
;;ゲーム開始
(defun main ()
  (init-charms)
  (setf *random-state* (make-random-state t))
  (let* ((p (make-player)) 
	 (map (make-donjon)))
    (init-data) ;;データ初期化
    (maze map p) ;;マップ生成
    (main-game-loop map p)))

;;壁破壊
(defun kabe-break (map p y x)
  (scr-format "「ハンマーで壁を壊しますか？」[yes=z or no=anykey]:~%")
  (case (read-command-char)
    (z
      (if (>= (random 10) 3)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 0)
	(setf (aref map (+ (player-posy p) y) (+ (player-posx p) x)) 3))
     (decf (player-hammer p)))))
;;(scr-format "「壁を壊しました。」~%"))))

;;アイテムを捨てる入れ替える
(defun throw-item (p item)
  (gamen-clear)
  (scr-format "-------------持ち物が一杯です。捨てる武器を選んでください-------------~%")
  (loop for buki in (player-item p)
	for x from 1 do
	  (scr-format "[~c]:~a:力+~2,'0d HP+~2,'0d 素早さ+~2,'0d~%"
		      (number->a x) (minimum-column 18 (first buki)) (second buki)
		      (third buki) (fourth buki)))
  (scr-format "[z]:戻る(拾った武器を捨てる)")
  (let ((x (ascii->number (read-command-char))))
    (cond
      ((and (integerp x) (<= 0 x 19) (< x (length (player-item p))))
       (setf (nth x (player-item p)) item))
      ((and (integerp x) (= x 25)) ;;zキーで戻る
       )
      (t
       (throw-item p item)))))
  
;;持ち物にアイテム追加
(defun add-item (p item)
  (if (> 20 (length (player-item p))) ;;持ち物２０個まで
      (push item (player-item p))
      (throw-item p item))) ;;アイテムを入れ替える
      

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
(defun equip? (p item)
  (scr-format "「~aを見つけた」~%" (first item))
  (scr-format "現在の装備品：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	      (first (player-buki p)) (second (player-buki p))
	      (third (player-buki p)) (fourth (player-buki p)))
  (scr-format "発見した装備：~a 攻撃力:~d HP:~d 素早さ:~d~%"
	      (first item) (second item) (third item) (fourth item))
  (scr-format "「装備しますか？」(z:装備 x:捨てる c:袋にしまう)~%")
  (case (read-command-char)
    (z
     (scr-format "「~aを装備した。」~%" (first item))
     (if (not (string= "なし" (first (player-buki p))))
	 (push (player-buki p) (player-item p)))
     (equip-buki item p))
    (x
     (scr-format "「~aを見なかったことにした。」~%" (first item)))
    (c ;;持ち物に追加
     (add-item p item))
    (otherwise
     (equip? p item))))

(defun hummer-get (p)
  (setf (player-msg p) "「ハンマーを見つけた。」")
  (incf (player-hammer p)))

(defun kusuri-get (p)
  (setf (player-msg p) "「回復薬を見つけた。」")
  (incf (player-heal p)))



;;重み付け抽選-----------------------------------------------
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;lst = *copy-buki*
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))
;;------------------------------------------------------------
;; lst = *copy-buki*
;;*copy-buki*の確率の部分をずらす
(defun omomin-zurashi (lst)
  (let ((buki (mapcar #'car lst))
	(omomi (mapcar #'cdr lst)))
    (setf omomi (butlast omomi))
    (push 10 omomi)
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
     (loop for hoge from 0 to 53)
     collect x
     do (incf x 1)))
|#
;;---------------------------------------------
;;武器ゲット２ 全アイテムからランダム
(defun item-get2 (p)
  (case (random 7)
    ((0 1 2 5) ;;武器ゲット
     (equip? p (weightpick *copy-buki*)))
    ((3 6) (hummer-get p)) ;;ハンマーゲット
    (4 (kusuri-get p)))) ;;回復薬ゲット

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
        (equip-buki (assoc "もげぞうの剣" *event-buki* :test #'equal) p))
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
     ;;２階降りるごとにハンマーもらえる
     (if (= (mod (player-map p) 2) 0)
	 (incf (player-hammer p)))
     ;;５階降りるごとに宝箱の確率変わる
     (if (= (mod (player-map p) 5) 0)
	 (setf *copy-buki* (omomin-zurashi *copy-buki*)))
     ;;７階降りるごとに敵のレベル上がる
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
;; 1 を受け取れば t を、2を受け取れば nil を返す。それ以外はループ
(defun yes-no-dialog (message)
  (scr-format "~a(yes=1 or no=2)~%" message)
  (case (read-command-char)
    (1 t)
    (2 nil)
    (otherwise (yes-no-dialog message))))

;; クリア記録 total-seconds をランキングファイルへ登録時のダイアログ。
(defun ranking-dialog (total-seconds)
  (when (yes-no-dialog "ランキングに登録しますか？")
    (gamen-clear)
    (endo-win)
    (fresh-line)
    (format t "~%名前を教えてください：~%")
    ;;(scr-format "名前を入力してください:~%")
    (let ((name (read-line)))
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
               ranking1))))))
    (init-charms)))
