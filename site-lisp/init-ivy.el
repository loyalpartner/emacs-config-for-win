;;; init-ivy.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: Sun Mar 15 15:03:02 2020
;; Modified: Sun Mar 15 15:03:02 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-ivy
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package ivy
  :straight t
  :after-call pre-command-hook
  :config
  (ivy-mode 1))

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package ivy-rich
  :straight t
  :after ivy
  ;; :commands (counsel-M-x)
  :defer 1
  :config
  (ivy-rich-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-prescient
  :straight t
  :after ivy
  :config
  (ivy-prescient-mode 1))

(provide 'init-ivy)
