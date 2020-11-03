;;; init.el -*- lexical-binding: t; -*-

;; Defer garbage collection until after initialization
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 4 1024 1024))))

;; In interactive sessions, save a little IO time by skipping the
;; mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Load configuration tangled from Org file
(require 'org)
(let* ((config.org (expand-file-name "config.org" user-emacs-directory))
       (config (file-name-sans-extension config.org))
       (config.el (concat config ".el")))
  ;; c.f. org-babel-load-file
  (unless (org-file-newer-than-p
           config.el
           (file-attribute-modification-time (file-attributes config.org)))
    (org-babel-tangle-file config.org config.el "emacs-lisp"))
  (let ((load-prefer-newer t))
    (load config))
  (byte-recompile-file config.el nil 0))
