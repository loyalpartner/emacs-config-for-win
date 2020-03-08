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

;; git
(nvmap :prefix leader-key
  :keymaps 'override
  "gr" #'git-gutter:revert-hunk
  "gs" #'git-gutter:stage-hunk
  "gS" #'magit-stage-file
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

;;;###autoload
(defun search-cwd (&optional arg)
  (interactive "P")
  (let* ((default-directory (if arg
				(read-directory-name "Search directory: ")
			      default-directory))
	 (directory-name (directory-file-name default-directory))
	 (prompt (format "rg [%s]: " directory-name)))

    (counsel-rg nil default-directory nil prompt)))

;;;###autoload
(defun search-other-cwd ()
  (interactive)
  (search-cwd 'other))

;; search
(nvmap :prefix leader-key
  :keymaps 'override
  "sb" '(counsel-grep-or-swiper :which-key "search current buffer")
  "sd" '(search-cwd :which-key "search current directory")
  "sD" '(search-other-cwd :which-key "search current directory")
  "sf" '(counsel-locate :which-key "locate file")
  "si" '(counsel-imenu :which-key "jump to symbol")
  "sl" '(link-hint-open-link :which-key "jump to link")
  "sL" '(link-hint-copy-link :which-key "copy link")
  "so" '(counsel-ace-link :which-key "#TODO")
  "sp" '(counsel-rg :which-key "search project")
  "ss" '(swiper-isearch :which-key "search buffer")
  "sS" '(swiper-isearch-thing-at-point :which-key "search buffer at point"))

;; window
(general-def evil-window-map
  "d" #'evil-window-delete
  "u" #'winner-undo
  "C-r" #'winner-redo)

;; 
(nvmap :prefix leader-key
  :keymaps 'override
  "qq" #'save-buffers-kill-emacs)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
