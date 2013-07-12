;;;
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;; Install Required Package
(package-initialize)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")))
(package-refresh-contents)

(dolist (pkg '(elnode bigint))
  (unless (package-installed-p pkg)
    (package-install pkg)
    (require pkg)))

;;; Setup Web Server
(load-file "wechat.el")
(wechat-server-start)

;;; Load Sample App
(dolist (app (file-expand-wildcards "apps/*.el"))
  (load-file app))

;;; Now, you can visit http://localhost:26870/game/5x5.el.
;;; You can integrate nginx by append below into nginx.conf as well.
;; location ~ \.el$ {
;;     proxy_pass http://127.0.0.1:26870;
;;     proxy_set_header Content-Type text/html;
;; }
