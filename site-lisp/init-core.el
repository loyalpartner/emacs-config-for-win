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

;; use text-mode in scratch when start it.
(setq initial-major-mode 'text-mode)

(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      ns-function-modifier 'hyper)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq make-backup-files nil)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "saves" user-emacs-directory))))

;;; https://gist.github.com/jcouyang/d7cf6c8011b3b9c3f9a7
(setq vc-follow-symlinks nil)

;; (use-package find-file-in-project
;;   :straight t
;;   :commands (find-file-in-project)
;;   :config
;;   (when IS-WINDOWS
;;     (setq ffip-find-executable find-file-in-project-program)))

(use-package selectrum
  :straight t
  :init
 (add-hook 'pre-command-hook #'selectrum-mode)
  :config
;					(selectrum-mode 1)
  )

(use-package selectrum-prescient
  :after selectrum
  :straight
  (selectrum-prescient
   :host github :repo "raxod502/prescient.el"
   :files ("selectrum-prescient.el"))
  :defer 1
  :config
  (prescient-persist-mode)
  (selectrum-prescient-mode))

(use-package general
  :straight t
  :config
  (general-evil-setup t)
  (general-override-mode))

(defmacro omap! (key inner outer)
  (general-define-key key inner :keymaps 'evil-inner-text-objects-map)
  (general-define-key key outer :keymaps 'evil-outer-text-objects-map))

(use-package winer
  :defer 1
  ;; :hook (after-init . winner-mode)
  )

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
