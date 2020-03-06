;;; init-vc.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 06, 2020
;; Modified: March 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-vc
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package magit
  :straight t
  :init
  (add-hook 'magit-mode-hook #'hide-mode-line-mode)
  :commands magit)

(use-package forge
  :straight t
  :after magit
  :commands
  forge-create-pullreq forge-create-issue)

(use-package evil-magit
  :straight t
  :after magit)

(use-package git-gutter
  :straight t
  :init
  (add-hook 'find-file-hook #'vc-gutter-init-maybe-h)
  :commands git-gutter:revert-hunk git-gutter:stage-hunk)

(use-package magit-todos
  :straight t
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))

(defun vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.

If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (when (not (file-remote-p (or buffer-file-name default-directory)))
        (if (not buffer-file-name)
            (add-hook 'after-save-hook #'vc-gutter-init-maybe-h nil 'local)
          (when (and (vc-backend buffer-file-name)
                     (progn
                       (require 'git-gutter)
                       (not (memq major-mode git-gutter:disabled-modes))))
            (if (and (display-graphic-p)
                     (require 'git-gutter-fringe nil t))
                (progn
                  (setq-local git-gutter:init-function      #'git-gutter-fr:init)
		  (setq-local git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos)
		  (setq-local git-gutter:clear-function     #'git-gutter-fr:clear)
		  (setq-local git-gutter:window-width -1))
	      (setq-local git-gutter:init-function      'nil)
	      (setq-local git-gutter:view-diff-function #'git-gutter:view-diff-infos)
              (setq-local git-gutter:clear-function     #'git-gutter:clear-diff-infos)
              (setq-local git-gutter:window-width 1))
            (git-gutter-mode +1)
            (remove-hook 'after-save-hook #'vc-gutter-init-maybe-h 'local)))))


(provide 'init-vc)
;;; init-vc.el ends here
