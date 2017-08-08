





(defparameter *buki*
  (make-array 54 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("もげぞうの剣"   30 0 0)
		("もげぞーの剣"   1 0 0)
		("モげぞうの剣"   3 0 0)
		("もゲぞうの剣"   2 0 0)
		("予定地ソード"   3 0 0)
		("ハツネツの剣"   3 0 0)
		("木の枝"         1 0 0)
		("木刀"            2 0 0)
		("ショートソード"   1 0 1)
		("ヌンチャク"      2 0 1)
		("サーベル"       3 0 1)
		("鉄の剣"         3 0 0)
		("銅の剣"         4 0 0)
		("銀の剣"         5 0 0)
		("金の剣"         6 0 0)
		("ファルシオン"   7 0 0)
		("ククリ"           7 0 2)
		("グレートソード"  8 0 0)
		("バスタードソード"  9 0 0)
		("クレイモア"        10 0 0)
		("モーニングスター"  10 2 0)
		("バルディッシュ"    11 0 0)
		("パルチザン"       12 0 0)
		("ゲイボルグ"       20 0 0)
		("チャクラム"        6 0 0)
		("ウォーハンマー"   11 5 0)
		("マインゴーシュ"   8 0 5)
		("ソードブレイカー"  15 0 0)
		("シャムシール"      11 0 3)
		("オートクレール"     22 0 0)
		("ブリューナク"       16 0 0)
		("正宗"               20 0 8)
		("レーヴァテイン"    19 9 0)
		("エクスカリバー"    25 0 0)
		("ブロードソード"      3 0 0)
		("レイピア"           3 0 0)
		("ツーハンドソード"   7 0 0)
		("ダガー"            2 0 0)
		("メイス"            2 0 0)
		("鎌"               2 0 0)
		("スピア"          2 0 0)
		("ジャベリン"       3 0 0)
		("ナイフ"           1 0 0)
		("ミスリルナイフ " 4 0 0)
		("猫の爪"          8 0 0)
		("ウェアバスター"  6 0 0)
		("シミター"         3 0 0)
		("フォールチュン"       6 0 0)
		("バトルアクス"        4 0 0)
		("グレートアクス"       7 0 0)
		("ちからのつえ"       6 0 0)
		("ルーンブレード"       11 0 0)
		("さんごのつるぎ"       12 0 0)
		("ディフェンダー"       15 7 0)
		)))

(defparameter *buki1-10*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("もげぞーの剣"   1 0 0)
		("木の枝"         1 0 0)
		("木刀"            2 0 0)
		("ショートソード"   1 0 1)
		("ヌンチャク"      2 0 1)
		("鉄の剣"         3 0 0)
		("銅の剣"         4 0 0)
		("銀の剣"         5 0 0)
		("金の剣"         6 0 0)
		("グレートアクス"       7 0 0))))

(defparameter *buki11-20*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("木刀"            2 0 0)
		("ショートソード"   1 0 1)
		("ヌンチャク"      2 0 1)
		("鉄の剣"          3 0 0)
		("銅の剣"          4 0 0)
		("銀の剣"          5 0 0)
		("金の剣"          6 0 0)
		("グレートアクス"   7 0 0)
		("グレートソード"   8 0 0)
		("ククリ"           7 0 2))))

(defparameter *buki21-30*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("ヌンチャク"      2 0 1)
		("鉄の剣"          3 0 0)
		("銅の剣"          4 0 0)
		("銀の剣"          5 0 0)
		("金の剣"          6 0 0)
		("グレートアクス"   7 0 0)
		("グレートソード"   8 0 0)
		("ククリ"           7 0 2)
		("バスタードソード"  9 0 0)
		("マインゴーシュ"   8 0 5))))

(defparameter *buki31-40*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("銅の剣"          4 0 0)
		("銀の剣"          5 0 0)
		("金の剣"          6 0 0)
		("グレートアクス"   7 0 0)
		("グレートソード"   8 0 0)
		("ククリ"           7 0 2)
		("バスタードソード"  9 0 0)
		("マインゴーシュ"   8 0 5)
		("モーニングスター"  10 2 0)
		("バルディッシュ"    11 0 0))))

(defparameter *buki41-50*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("金の剣"          6 0 0)
		("グレートアクス"   7 0 0)
		("グレートソード"   8 0 0)
		("ククリ"           7 0 2)
		("バスタードソード"  9 0 0)
		("マインゴーシュ"   8 0 5)
		("モーニングスター"  10 2 0)
		("バルディッシュ"    11 0 0)
		("パルチザン"       12 0 0)
		("ウォーハンマー"   11 5 0))))

(defparameter *buki51-60*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("グレートソード"   8 0 0)
		("ククリ"           7 0 2)
		("バスタードソード"  9 0 0)
		("マインゴーシュ"   8 0 5)
		("モーニングスター"  10 2 0)
		("バルディッシュ"    11 0 0)
		("パルチザン"       12 0 0)
		("ウォーハンマー"   11 5 0)
		("ルーンブレード"       13 5 0)
		("さんごのつるぎ"       14 0 4))))


(defparameter *buki61-70*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("バスタードソード"  9 0 0)
		("マインゴーシュ"   8 0 5)
		("モーニングスター"  10 2 0)
		("バルディッシュ"    11 0 0)
		("パルチザン"       12 0 0)
		("ウォーハンマー"   11 5 0)
		("ルーンブレード"       13 5 0)
		("さんごのつるぎ"       14 0 4)
		("ブリューナク"       16 5 0)
		("ソードブレイカー"  17 5 1))))
(defparameter *buki71-80*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("モーニングスター"  10 2 0)
		("バルディッシュ"    11 0 0)
		("パルチザン"       12 0 0)
		("ウォーハンマー"   11 5 0)
		("ルーンブレード"       13 5 0)
		("さんごのつるぎ"       14 0 4)
		("ブリューナク"       16 5 0)
		("ソードブレイカー"  17 5 1)
		("レーヴァテイン"    18 9 0)
		("オートクレール"     20 8 0))))
(defparameter *buki81-90*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("パルチザン"       12 0 0)
		("ウォーハンマー"   11 5 0)
		("ルーンブレード"       13 5 0)
		("さんごのつるぎ"       14 0 4)
		("ブリューナク"       16 5 0)
	        ("予定地ソード"   20 8 8)
		("レーヴァテイン"    18 9 0)
		("オートクレール"     20 8 0)
		("正宗"               21 0 8)
		("ゲイボルグ"       22 5 5))))

(defparameter *buki91-100*
  (make-array 10 :initial-contents
	      ;; 名前      　攻撃力　HP 素早さ　
	      '(("ルーンブレード"       13 5 0)
		("さんごのつるぎ"       14 0 4)
		("ブリューナク"       16 5 0)
		("ソードブレイカー"  17 5 1)
		("レーヴァテイン"    18 9 0)
		("オートクレール"     20 8 0)
		("正宗"               21 0 8)
		("ゲイボルグ"       22 5 5)
		("エクスカリバー"    25 10 10)
		("もげぞうの剣"   30 20 20))))