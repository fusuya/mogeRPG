(ql:quickload :cl-charms)
(ql:quickload :exit-hooks)

(defun scr-fresh-line ()
  (let ((x (cl-charms/low-level:getcurx cl-charms/low-level:*stdscr*)))
    (if (/= 0 x)
        (cl-charms/low-level:addstr (format nil "~%")))))

(defun scr-princ (obj)
  (cl-charms/low-level:addstr (format nil "~A" obj)))

(defun scr-format (&rest args)
  (cl-charms/low-level:addstr (apply #'format (append '(nil) args))))

;; () -> sexp
(defun read-command-char ()
  (let ((c (cl-charms/low-level:getch)))
    (cond
      ((= c cl-charms/low-level:KEY_UP)    (setf c (char-code #\w)))
      ((= c cl-charms/low-level:KEY_LEFT)  (setf c (char-code #\a)))
      ((= c cl-charms/low-level:KEY_DOWN)  (setf c (char-code #\s)))
      ((= c cl-charms/low-level:KEY_RIGHT) (setf c (char-code #\d))))
    (if (and (<= 0 c) (<= c 127)) ; ascii range
        (let ((char (code-char c)))
          (if (or
               (and (char<= #\a char) (char<= char #\z))
               (and (char<= #\A char) (char<= char #\Z))
               (and (char<= #\0 char) (char<= char #\9)))
              (read-from-string (format nil "~C" (code-char c)))
            nil))
      nil)))

(defun gets ()
  (let ((buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (labels
        ((add-char ()
                   (let ((code (cl-charms/low-level:getch)))
                     (if (= code 10)
                         (progn
                           (cl-charms/low-level:scrl 1)
                           buf)
                       (progn
                         (vector-push-extend (code-char code) buf)
                         (add-char))))))
      (add-char))))

;; () -> sexp
(defun read-command-line ()
  (read-from-string (gets)))

(defun init-charms ()
  (cl-charms/low-level:initscr)
  (cl-charms/low-level:scrollok cl-charms/low-level:*stdscr* 1)
  (cl-charms/low-level:keypad cl-charms/low-level:*stdscr* 1)
(exit-hooks:add-exit-hook #'cl-charms/low-level:endwin))

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
      (q (use-heal p))
      (z (setf *end* 2))
      (otherwise
       (scr-format "w,a,s,d,q,zの中から選んでください！~%")))

    (map-move map p)))
