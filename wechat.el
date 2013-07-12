;;; wechat.el --- Wechat Public Platform DEV framework in Emacs Lisp.
(require 'elnode)
(require 'bigint)

;; Time utility
(defun current-seconds-string ()
  "Returns the current time measured in the number of seconds
since the Unix Epoch (January 1 1970 00:00:00 GMT)."
  (destructuring-bind (high low _ _) (current-time)
    (bigint-to-string
     (bigint-add
      (bigint-multiply
       (bigint-int-to-bigint high)
       (bigint-int-to-bigint 65536))
      (bigint-int-to-bigint low)))))

;; XML utility
(defun xml-get-text (xml node)
  (let ((children (first (xml-get-children xml node))))
    (if children
        (third children))))

(defun wechat-input-xml ()
  "Parse the raw post data as XML."
  (save-excursion
    (beginning-of-buffer)
    (first (xml-parse-region (search-forward "\r\n\r\n")
                             (point-max)))))

(defun wechat-output-xml (xml content)
  "Utility function to generate response XML."
  (format "<xml>
  <ToUserName><![CDATA[%s]]></ToUserName>
  <FromUserName><![CDATA[%s]]></FromUserName>
  <CreateTime>%s</CreateTime>
  <MsgType><![CDATA[text]]></MsgType>
  <Content><![CDATA[%s]]></Content>
  <FuncFlag>0</FuncFlag>
</xml>" (xml-get-text xml 'FromUserName)
        (xml-get-text xml 'ToUserName)
        (current-seconds-string)
        content))

;; Session
(defvar *wechat-work-directory* "~/.emacs.d/wechat/"
  "The path to save user session info.")

(defun wechat-session-filename (username)
  (concat *wechat-work-directory* username))

(defun wechat-session-load (username)
  (let ((filename (wechat-session-filename username)))
    (if (file-readable-p filename)
        (car (read-from-string
              (with-temp-buffer
                (insert-file-contents filename)
                (buffer-string))))
      (make-hash-table :test 'equal))))

(defun wechat-session-save (username)
  (with-temp-file (wechat-session-filename username)
    (insert (prin1-to-string current-session-table))))

(defmacro wechat-with-session (username &rest body)
  (declare (indent 1))
  `(let ((current-session-table (wechat-session-load ,username)))
     (unwind-protect
         (progn ,@body)
       (wechat-session-save ,username))))

;;;###autoload
(defun session (key &optional value)
  (if value
      (puthash key value current-session-table)
    (gethash key current-session-table)))

;; Services
(defvar *wechat-apps* (make-hash-table :test #'equal)
  "Contains defined wechat app.
The value is the default entry of an app.")

(defvar *wechat-rooms* (make-hash-table)
  "Contains defined wechat rooms.")

;;;###autoload
(defun def-map (url entry)
  "Define a new wechat app."
  (puthash url entry *wechat-apps*))

(defun def-room-raw (name prompt doors)
  "Define a new status."
  (puthash name (list prompt doors) *wechat-rooms*))

;;;###autoload
(defmacro def-room (name prompt &rest doors)
  "Wrap for def-room-raw."
  `(def-room-raw ',name ,prompt ',doors))

(defun wechat-enter-room (room-name key)
  (some (lambda (door)
          (destructuring-bind (condition next-room) door
            (when (or (and (booleanp condition)
                           condition)
                      (and (stringp condition)
                           (string-match (concat "^" condition "$") key))
                      (and (functionp condition)
                           (funcall condition key)))
              (if (functionp next-room)
                  (funcall next-room key)
                next-room))))
        (second (gethash room-name *wechat-rooms*))))

(defun wechat-room-prompt (room-name)
  (let ((prompt (first (gethash room-name *wechat-rooms*))))
    (if (functionp prompt)
        (funcall prompt)
      prompt)))

(defun wechat-run (http)
  "Return a service handler by request path."
  (let* ((xml (wechat-input-xml)))
    (wechat-with-session (xml-get-text xml 'FromUserName)
      (session "room" (if (session "room")
                          (wechat-enter-room (session "room")
                                             (xml-get-text xml 'Content))
                        (gethash (elnode-http-pathinfo http) *wechat-apps*)))
      (wechat-output-xml xml (wechat-room-prompt (session "room"))))))

;; Server
(defvar *wechat-server-port* 26870
  "Listen port of Wechat server.")

(defun wechat-server-handler (http)
  "Wechat Server!
If `echostr' param is exist, return the value;
Otherwise, distribute the raw post data to handler."
  (elnode-http-start http 200 '("Content-Type" . "text/html; charset=UTF-8"))
  (elnode-http-return http
    (if (elnode-http-param http "echostr")
        (elnode-http-param http "echostr")
      (wechat-run http))))

;;;###autoload
(defun wechat-server-start (&optional port)
  "Start HTTP server for Wechat services"
  (when (integerp port)
    (setf *wechat-server-port* port))
  (if (file-exists-p *wechat-work-directory*)
      (make-directory *wechat-work-directory* t))
  (elnode-start #'wechat-server-handler :port *wechat-server-port*))

;;;###autoload
(defun wechat-server-stop (&optional port)
  "Stop the wechat server."
  (elnode-stop (or port *wechat-server-port*)))

(provide 'wechat)
