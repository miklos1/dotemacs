;;; init.el -*- lexical-binding: t; -*-

;; Defer garbage collection until after initialization
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 4 1024 1024))))

;; Load newer configuration file, and re-compile if necessary
(let* ((config (expand-file-name "config" user-emacs-directory))
       (config.el (concat config ".el")))
  (let ((load-prefer-newer t))
    (load config))
  (byte-recompile-file config.el nil 0))
