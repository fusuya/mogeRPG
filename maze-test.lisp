(defparameter *map100*
  #2A((30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30  0  5  0 30 30 30 30)
      (30 30 30 30  0  0  0 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30  4  0  0  0  0  0  0  0  6 30)
      (30 30 30 30 30  1 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)))

(defparameter *map50*
  #2A((30 30 30 30 30 30 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)
      (30  2 30 30  0  7  0 30 30 30 30)
      (30  0 30 30  0  0  0 30 30 30 30)
      (30  0 30 30 30  0 30 30 30 30 30)
      (30  0  0  0  0  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30 30 30 30 30  0 30 30 30 30 30)
      (30  4  0  0  0  0  0  0  0  3 30)
      (30 30 30 30 30  1 30 30 30 30 30)
      (30 30 30 30 30 30 30 30 30 30 30)))

(defparameter *map0*
  (make-array (list *tate* *yoko*)))
(defparameter *1234* '(1 2 3 4))

(defstruct d-map
  (map (make-array (list *tate* *yoko*))) ;;マップ
  (stop-list nil)) ;;行き止まりリスト

(defun init-map (map) ;;マップを壁で埋める
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (setf (aref map i j) 30))))
  
(defun rand1234 (lst lst1)
  (if (null lst1)
      lst
      (let* ((n (random (length lst1)))
	    (m (nth n lst1)))
	(push m lst)
	(rand1234 lst (remove m lst1)))))
  

(defun recursion (y x map)
  (let ((lst (rand1234 '() '(1 2 3 4)))
	(stop? t))
     (loop for i in lst do
	 (case i
	   (1 ;;上
	    (if (< 0 (- y 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (d-map-map map) (- y 2) x) 30) ;;2マス先が通路か
		   (setf (aref (d-map-map map) (- y 2) x) 0)
		   (setf (aref (d-map-map map) (- y 1) x) 0)
		   (setf stop? nil)
		   (recursion (- y 2) x map)))))
	    ;;(return))
	   (2 ;;下
	    (if (> *tate* (+ y 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (d-map-map map) (+ y 2) x) 30)
		   (setf (aref (d-map-map map) (+ y 2) x) 0)
		   (setf (aref (d-map-map map) (+ y 1) x) 0)
		   (setf stop? nil)
		   (recursion (+ y 2) x map)))))
	    ;;(return))
	   (3 ;;右
	    (if (> *yoko* (+ x 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (d-map-map map) y (+ x 2)) 30)
		   (setf (aref (d-map-map map) y (+ x 2)) 0)
		   (setf (aref (d-map-map map) y (+ x 1)) 0)
		   (setf stop? nil)
		   (recursion y (+ x 2) map)))))
	    ;;(return))
	   (4 ;;左
	    (if (< 0 (- x 2)) ;;2マス先が迷路の外か
		(cond
		  ((= (aref (d-map-map map) y (- x 2)) 30)
		   (setf (aref (d-map-map map) y (- x 2)) 0)
		   (setf (aref (d-map-map map) y (- x 1)) 0)
		   (setf stop? nil)
		   (recursion y (- x 2) map)))))))
    (if stop? ;;行き止まりだったら
	(let ((item (random 4)))
	  ;;(format t "~d ~d ~d~%" y x i)
	  (push (list y x) (d-map-stop-list map)) ;;行き止まりの座標リスト
	  (case item
	    (0 ;;薬
	     (setf (aref (d-map-map map) y x) 4))
	    ((1 2 3) ;;宝
	     (setf (aref (d-map-map map) y x) 3)))))))

(defun test-show-map (map)
  (loop for i from 0 below *tate* do
    (loop for j from 0 below *yoko* do
      (princ (map-type (aref map i j)))
      
      (if (= j (- *yoko* 1))
	  (case i
	    (0 (format t " 主:プレイヤーの位置~%"))
	    (2 (format t " 宝:宝箱~%"))
	    (1 (format t " 下:下り階段~%"))
	    (3 (format t " 薬:回復薬~%"))
	    (otherwise (fresh-line)))))))

(defun maze (p)
  (let* ((mapn (make-d-map))
	 (x (random (floor *yoko* 2)))
	 (startx (+ (* x 2) 1))
	 (y (random (floor *tate* 2)))
	 (starty (+ (* y 2) 1))
	 (kaidan nil))
    (init-map (d-map-map mapn)) ;;マップ初期化
    (setf (aref (d-map-map mapn) starty startx) 0) ;;初期位置を通路にする
    (recursion starty startx mapn)
    (setf (aref (d-map-map mapn) starty startx) 1) ;;主人公の位置
    (setf (player-posy p) starty
	  (player-posx p) startx) ;;初期位置
    (setf kaidan (nth (random (length (d-map-stop-list mapn))) (d-map-stop-list mapn))) ;;階段の場所ランダム
    (setf (aref (d-map-map mapn) (car kaidan) (cadr kaidan)) 2) ;;階段をセット
    (d-map-map mapn)))
    ;;(test-show-map (d-map-map mapn))))
    
