;; 定义新的游戏地图
(def-map "/game/5x5.el"                 ; 对外开放的URL
  'tutorial-room-0)                     ; 默认的入口

;;; 游戏大厅
(def-room living-room
  ;; 进入该房间后的提示语
  "1. 教程
2. 入门（3x3）
3. 初级（5x5）
4. 中级（7x7）
5. 高级（9x9）
0. 关于作者
请选择1-5开始新游戏："
  ("0" about-room)                      ; 期望用户输入的信息以及相应的后续房间名
  ("1" tutorial-room-0)
  ("[2-5]" game-room-init)              ; 用户输入信息可用正则表达式匹配
                                        ; 相应的返回也可以为函数，动态返回房间名
  (t living-room))                      ; 如果条件为t，为永真

;;; 作者信息
(def-room about-room
  "作者：redraiment
微博：http://weibo.com/redraiment
有任何建议，欢迎在微博或微信上联系redraiment。
请输入任意数字返回游戏大厅。"
  (t living-room))

;;; 教程
(defvar *wechat-5x5-tutorial-rooms* 0
  "The number of tutorial rooms")

;;; 简化教程的定义
(defun string-last-line (content)
  "多行内容组成的字符串中的最后一行"
  (with-temp-buffer
    (insert content)
    (buffer-substring (line-beginning-position)
                      (point-max))))

(defun def-tutorial-room (prompt)
  "根据提示语自动生成教程房间。

1. 提取最后一行作为问题；
2. 分析问题，获取期望用户输入的内容；
3. 定义教程房间和重复提问房间。"
  (let* ((room-name (concat "tutorial-room-" (number-to-string *wechat-5x5-tutorial-rooms*)))
         (repeat-room (concat room-name "-repeat"))
         (next-room (concat "tutorial-room-" (number-to-string (incf *wechat-5x5-tutorial-rooms*))))
         (question (string-last-line prompt))
         (except (if (string-match "请输入“\\([0-9a-zA-Z]+\\)”" question)
                     (match-string 1 question)))
         (doors (if except
                    `((,except ,(intern next-room))
                      ("q" living-room)
                      ("Q" living-room)
                      (t ,(intern repeat-room)))
                  '((t living-room)))))
    (def-room-raw (intern room-name) prompt doors)
    (def-room-raw (intern repeat-room) question doors)))

(defun def-tutorial (&rest prompts)
  "批量生成教程房间。"
  (dolist (prompt prompts)
    (def-tutorial-room prompt)))

(def-tutorial
  "欢迎体验微信版的开窗游戏！为了让您尽快熟悉游戏的规则，请根据教程提示一步一步完成要求的操作。注：任何时刻，您都能输入“q”来退出本教程。
1. 教程
2. 入门（3x3）
3. 初级（5x5）
4. 中级（7x7）
5. 高级（9x9）
0. 关于作者
请选择1-5开始新游戏：
您现在正在游戏大厅里。
请输入“2”进入入门级房间"
  "  ①②③
1　■　
2■■■
3　■　
进入房间后，您就会看到一个如上图所示的3x3棋盘。其中黑子代表打开的窗户，白子代表关闭的窗户。您可以输入任意一个棋盘上的坐标，来开启或关闭某扇窗户。游戏的目标是开启所有窗户！
请输入“22”来关闭第2行第2列的窗户。"
  "  ①②③
1　　　
2　　　
3　　　
你可能被吓了一跳，怎么所有的窗户都被关闭了？我忘了告诉您：这些格子是被按了机关的，只要一个窗户被开启或关闭，它的上下左右四扇窗户也会连锁反应，被开启或关闭。
请输入“11”，试着开启左上角的窗户。"
  "  ①②③
1■■　
2■　　
3　　　
因为连锁反应，11右边和下面的窗户也被开启了。但因为11在左上角，没有上面和左边，因此这回总共只开启了三扇窗户。
请输入“13”开启右上角的窗户。"
  "  ①②③
1■　■
2■　■
3　　　
第1行第2列上的窗户因为本来就是处于开启状态的，这回因为13的连锁反应，又重新被关闭了。
请输入“31”开启左下角的窗户。"
  "  ①②③
1■　■
2　　■
3■■　
此时，总共有5扇窗户被开启了。
请输入“33”开启右下角的窗户。"
  "  ①②③
1■　■
2　　　
3■　■
现在，只有四个角落的窗户被打开。
请输入“22”完成最后一击！"
  "  ①②③
1■■■
2■■■
3■■■
您已完成教程的所有内容，输入任意内容返回大厅，开始您的开窗之旅！")

;;; 棋盘
(defconst *wechat-5x5-white-chess* 12288
  "　")

(defconst *wechat-5x5-black-chess* 9632
  "■")

(defmacro with-board (&rest body)
  `(with-temp-buffer
     (unwind-protect
         (progn
           (if (session "board")
               (insert (session "board")))
           ,@body)
       (session "board" (buffer-string)))))

(defun board-init (size)
  (session "size" size)
  (session "step" 0)
  (erase-buffer)
  (insert (format (format "  %%.%ds\n" size) "①②③④⑤⑥⑦⑧⑨"))
  (dotimes (row size)
    (insert (format "%d%s\n" (1+ row) (make-string size *wechat-5x5-white-chess*)))))

(defun board-contains-p (y x)
  (let ((size (session "size")))
    (and (<= 1 y) (<= y size)
         (<= 1 x) (<= x size))))

(defun board-toggle (y x)
  (when (board-contains-p y x)
    (goto-line (1+ y))
    (beginning-of-line)
    (forward-char x)
    (insert (if (= *wechat-5x5-white-chess* (following-char))
                *wechat-5x5-black-chess*
              *wechat-5x5-white-chess*))
    (delete-char 1)))

(defun board-put (y x)
  (dolist (dir '((-1 0) (0 -1) (0 0) (0 1) (1 0)))
    (board-toggle (+ y (first dir))
                  (+ x (second dir)))))

(defun game-over-p ()
  (beginning-of-buffer)
  (not (search-forward (string *wechat-5x5-white-chess*) nil t)))

(defun board-show ()
  (with-board
   (concat (buffer-string)
           (if (game-over-p)
              (format "共%d步，输入任意内容返回大厅" (session "step"))
            (format "第%d步" (1+ (session "step")))))))

(defun board-position-parse (cmd)
  (if (= (length cmd) 2)
      (list (string-to-int (substring cmd 0 1))
            (string-to-int (substring cmd 1 2)))
    '(0 0)))

;;; 游戏房间
(defun game-room-init (cmd)
  (let* ((middle (string-to-int cmd))
         (size (1- (* 2 middle))))
    (with-board
     (board-init size)
     (board-put middle middle)))
  'game-room)

(def-room game-room
  #'board-show
  (t (lambda (cmd)
         (with-board
          (if (game-over-p)
              'living-room
            (destructuring-bind (y x) (board-position-parse cmd)
              (when (board-contains-p y x)
                (board-toggle y x)
                (session "step" (1+ (session "step"))))
              'game-room))))))
