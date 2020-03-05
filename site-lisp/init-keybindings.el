;;; init-keybindings.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-keybindings
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defvar leader-key "SPC")

(general-override-mode)

(general-define-key
   :keymaps '(normal visual) "gc" #'evilnc-comment-operator)

(nvmap :prefix leader-key
  :keymaps 'override
  "bb" #'switch-to-buffer
  "bd" #'kill-this-buffer
  "bp" #'previous-buffer
  "bn" #'next-buffer)

(nvmap :prefix leader-key
  :keymaps 'override
  "ff" #'find-file
  "fp" #'find-file-in-project
  "SPC" #'find-file-in-project
  "fs" #'save-buffer)

(nvmap :prefix leader-key
  :keymaps 'override
  "ww" #'other-window)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
