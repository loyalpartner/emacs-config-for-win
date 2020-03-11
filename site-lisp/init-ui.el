;;; init-ui.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-ui
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; (use-package dashboard
;;   :straight t
;;   :config
;;   (switch-to-buffer "*dashboard*")
;;   (dashboard-insert-startupify-lists))

(use-package doom-modeline
  :after-call pre-command-hook
  :straight t
  :config (doom-modeline-mode 1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar change-theme-idle (* 30 1))

(use-package doom-themes
  :straight t
  :after-call pre-command-hook
  ;; :init
  ;; (add-hook 'after-init-hook (lambda () (load-theme 'doom-one-light t)))
  :config
  (change-theme-handler))

(defun current-time-get-hour ()
  (nth 2 (decode-time)))

(defun current-time-day-p ()
  (elt (number-sequence 8 20) (current-time-get-hour)))

(defun current-time-night-p ()
  (not (current-time-day-p)))

(setq change-theme-idle 3)
(defun change-theme-handler ()
  "根据时间的不同，显示不同的主题"
  (let* ((current-theme (car custom-enabled-themes)))
    (cond ((and (current-time-night-p)
                (not (equal current-theme 'doom-one)))
           (load-theme 'doom-one t))
          ((and (current-time-day-p)
                (equal current-theme 'doom-one))
           (load-theme 'doom-one-light t))))) 

;; (eval-after-load 'doom-themes)
(with-eval-after-load 'doom-themes
  (run-with-idle-timer change-theme-idle t
                       #'change-theme-handler))

(use-package which-key :straight t
  :commands which-key-mode
  :after-call pre-command-hook
  :config
  (which-key-mode))

(use-package hide-mode-line
   :straight t)

(provide 'init-ui)
;;; init-ui.el ends here
