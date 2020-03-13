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

(setq avy-style 'pre)

(use-package doom-themes
  :straight t
  :after-call pre-command-hook)

(use-package responsive-theme
  :after doom-themes
  :straight (responsive-theme
             :type git
             :flavor melpa
             :host github
             :repo "loyalpartner/responsive-theme")
  :config
  (responsive-theme-enable))

(use-package which-key :straight t
  :commands which-key-mode
  :after-call pre-command-hook
  :config
  (which-key-mode))

(use-package hide-mode-line
  :straight t)

(provide 'init-ui)
;;; init-ui.el ends here
