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

(nvmap override
  "C-`" #'+popup/toggle
  "M-x" #'counsel-M-x
  "gc" #'evilnc-comment-operator
  "gs SPC" #'evil-avy-goto-char-timer
  "gss" #'evil-avy-goto-char-2)

(nvmap override
  :prefix leader-key
  "b" '(nil :which-key "buffer")
  "f" '(nil :which-key "file")
  "g" '(nil :which-key "git")
  "go" '(nil :which-key "open in browser")
  "h" '(nil :which-key "help")
  "s" '(nil :which-key "search")
  "t" '(nil :which-key "toggle")
  "w" '(:keymap evil-window-map :which-key "window")
  "h" '(:keymap help-map :which-key "help")
  "u" #'universal-argument)

;;;###autoload
(defun switch-to-user-buffer ()
  (interactive)
  (let* ((user-buffer-list (seq-filter #'buffer-file-name (buffer-list)))
         (optional-user-buffer-list (remq (current-buffer) user-buffer-list))
         (choosed-buffer-file-name (completing-read "buffers:"
                                                    (mapcar
                                                     #'buffer-file-name
                                                     optional-user-buffer-list)))
         (choosed-buffer
          (seq-find (lambda (buffer)
                      (string= choosed-buffer-file-name (buffer-file-name buffer)))
                    optional-user-buffer-list)))
    (switch-to-buffer choosed-buffer)))

;;;###autoload
(defun next-user-buffer ()
  "next user buffer"
  (interactive)
  (when (seq-find #'buffer-file-name (buffer-list))
    (while (progn
             (next-buffer)
             (not buffer-file-name)))))


;;;###autoload
(defun previous-user-buffer ()
  "previous user buffer"
  (interactive)
  (when (seq-find #'buffer-file-name (buffer-list))
    (while (progn
             (previous-buffer)
             (not buffer-file-name)))))

;; buffer
(nvmap override
  :prefix leader-key
  "bb" #'switch-to-user-buffer
  "bx" #'find-scratch
  "bd" #'kill-this-buffer
  "bp" #'previous-user-buffer
  "bn" #'next-user-buffer)

;; file
(nvmap override
  :prefix leader-key
  "ff" #'find-file
  "fr" #'find-recent-file
  "fp" #'counsel-projectile
  "SPC" #'counsel-projectile
  "fs" #'save-buffer)

;; git
(nvmap override
  :prefix leader-key
  "gb" '(magit-branch-checkout :which-key "switch branch")
  "gB" '(magit-blame-addition :which-key "switch branch")

  "gg" #'magit-status
  "gy" '(browse-at-remote-kill :which-key "copy link to remote")
  "gY" '(vc-copy-link-to-homepage :which-key "copy link to homepage")

  "gr" #'git-gutter:revert-hunk
  "gs" #'git-gutter:stage-hunk
  "gS" #'magit-stage-file

  "goo" '(browse-at-remote :which-key "browse file or region")
  "goh" '(vc-browse-at-homepage :which-key "browse homepage")
  "gor" '(forge-browse-remote :which-key "brose remote")
  "goc" '(forge-browse-commit :which-key "browse commit")
  "goi" '(forge-browse-issue :which-key "browse an issue")
  "gop" '(forge-browse-pullreq :which-key "browse a pull request")
  "goI" '(forge-browse-issues :which-key "browse issues")
  "goP" '(forge-browse-pullreqs :which-key "browse pull requests"))
 
;; help
(nvmap override
  :prefix leader-key
  "hb" nil
  ;; #TODO
  "hbi" #'which-key-show-minor-mode-keymap
  "hbf" #'which-key-show-full-keymap
  "hbm" #'which-key-show-major-mode
  "hbk" #'which-key-show-keymap
  "hbt" #'which-key-show-top-level)

;; PROJECT:
(nvmap override
  :prefix leader-key
  "pf" '(counsel-projectile-find-file :which-key "switch project")
  "pp" '(counsel-projectile-switch-project :which-key "switch project")
  "pt" '(magit-todos-list :which-key "project task list")
 )

;;;###autoload
(defun search-cwd (&optional arg)
  (interactive "P")
  (let* ((default-directory (if arg
				(read-directory-name "Search directory: ")
			      default-directory))
	 (prompt (format "rg [%s]: " default-directory)))

    (counsel-rg nil default-directory nil prompt)))

;;;###autoload
(defun search-other-cwd ()
  (interactive)
  (search-cwd 'other))

;; search
(nvmap override
  :prefix leader-key
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

;; toggle
(nvmap override
  :prefix leader-key
  "ti" #'lisp-interaction-mode
  "tt" #'text-mode
  "tl" #'linum-mode)


;; window
(general-def evil-window-map
  "d" #'evil-window-delete
  "u" #'winner-undo
  "C-r" #'winner-redo)


;; workspace
(nvmap override
  "M-1" (lambda () (interactive) (persp-switch "1"))
  "M-2" (lambda () (interactive) (persp-switch "2"))
  "M-3" (lambda () (interactive) (persp-switch "3"))
  "M-4" (lambda () (interactive) (persp-switch "4"))
  "M-5" (lambda () (interactive) (persp-switch "5"))
  "M-6" (lambda () (interactive) (persp-switch "6"))
  "M-7" (lambda () (interactive) (persp-switch "7"))
  "M-8" (lambda () (interactive) (persp-switch "8"))
  "M-9" (lambda () (interactive) (persp-switch "9")))

(nvmap :prefix leader-key
  :keymaps 'override
  "qq" #'save-buffers-kill-emacs)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
