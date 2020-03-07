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
;;;;;;;;;;;;;;;;; 
(defvar leader-key "SPC")

;;;###autoload
(defun find-recent-file ()
  "show recentf list"
  (interactive)
  (recentf-mode)
  (find-file (completing-read
	      "recentf:" recentf-list)))

;;;###autoload
(defun find-scratch (&optional args)
  (interactive "P")
  (let ((f (if args
	     #'switch-to-buffer
	     #'pop-to-buffer)))
    (funcall f "*scratch*")))

(general-define-key
   :keymaps '(normal visual) "gc" #'evilnc-comment-operator)

(nvmap :prefix leader-key
  :keymaps 'override
  "b" '(nil :which-key "buffer")
  "f" '(nil :which-key "file")
  "g" '(nil :which-key "git")
  "w" '(:keymap evil-window-map :which-key "window")
  "h" '(:keymap help-map :which-key "help"))

(nvmap :prefix leader-key
  :keymaps 'override
  "bb" #'switch-to-buffer
  "bx" #'find-scratch
  "bd" #'kill-this-buffer
  "bp" #'previous-buffer
  "bn" #'next-buffer)

(nvmap :prefix leader-key
  :keymaps 'override
  "ff" #'find-file
  "fr" #'find-recent-file
  "fp" #'find-file-in-project
  "SPC" #'find-file-in-project
  "fs" #'save-buffer)

(nvmap :prefix leader-key
  :keymaps 'override
  "qq" #'save-buffers-kill-emacs)

(nvmap :prefix leader-key
  :keymaps 'override
  "gs" #'git-gutter:stage-hunk
  "gr" #'git-gutter:revert-hunk
  "gg" #'magit)

(nvmap :prefix leader-key
  :keymaps 'override
  "hb" nil
  ;; #TODO
  "hbi" #'which-key-show-minor-mode-keymap
  "hbf" #'which-key-show-full-keymap
  "hbm" #'which-key-show-major-mode
  "hbk" #'which-key-show-keymap
  "hbt" #'which-key-show-top-level)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
