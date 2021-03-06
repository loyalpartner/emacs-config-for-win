;;; init-core.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-core
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defvar find-file-in-project-program
  "c:\\msys64\\usr\\bin\\find")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(size-indication-mode t)
(column-number-mode 1)
(blink-cursor-mode 1)
(global-hl-line-mode)
(fset 'yes-or-no-p 'y-or-n-p)

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Word wrapping
;; (setq-default word-wrap t
;;               truncate-lines t
;;               truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")  ; for :retab


;; use text-mode in scratch when start it.
(setq initial-major-mode 'text-mode)

(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      ns-function-modifier 'hyper)

(setq make-backup-files nil)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "saves" user-emacs-directory))))

;;; https://gist.github.com/jcouyang/d7cf6c8011b3b9c3f9a7
(setq vc-follow-symlinks nil)

(save-place-mode 1)

;; (use-package find-file-in-project
;;   :straight t
;;   :commands (find-file-in-project)
;;   :config
;;   (when IS-WINDOWS
;;     (setq ffip-find-executable find-file-in-project-program)))

(defvar leader-key "SPC")

(use-package general
  :straight t
  :config
  (general-evil-setup t)
  (general-override-mode))

(defmacro omap! (key inner outer)
  (general-define-key key inner :keymaps 'evil-inner-text-objects-map)
  (general-define-key key outer :keymaps 'evil-outer-text-objects-map))

(use-package winner
  :defer 1
  ;; :hook (after-init . winner-mode)
  :config
  (winner-mode +1))

(use-package helpful
  :straight t
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)

  )

(use-package expand-region
  :straight t
  :commands (er/expand-region))

;; (general-def help-map
;;   "f" #'helpful-callable
;;   "v" #'helpful-variable
;;   "k" #'helpful-key)
;; (use-package projectile
;;   :straight t
;;   :after-call after-find-file dired-before-readin-hook minibuffer-setup-hook
;;   :commands (projectile-project-root
;;              projectile-project-name
;;              projectile-project-p
;;              projectile-locate-dominating-file)
;;   :init
;;   (setq projectile-globally-ignored-files '(".DS_Store" "Icon
;; " "TAGS")
;;         projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
;;         projectile-kill-buffers-filter 'kill-only-files
;;         projectile-ignored-projects '("~/" "/tmp"))

;;   (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
;;   (global-set-key [remap find-tag]         #'projectile-find-tag)

;;   :config
;;   (projectile-mode +1))

(use-package undo-tree
  :straight t
  :after-call after-find-file
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `(("." . ,(concat user-emacs-directory "undo-tree-hist/")))))

(use-package smartparens
  :straight t
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :after-call after-find-file 
  ;; :hook (after-hook-init . smartparens)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (smartparens-global-mode +1))

(use-package rainbow-delimiters
  :straight t
  :defer t)

(provide 'init-core)
;;; init-core.el ends here
