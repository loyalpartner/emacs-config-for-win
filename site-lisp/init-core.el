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

(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      ns-function-modifier 'hyper)

(setq make-backup-files nil)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "saves" user-emacs-directory))))

;;; https://gist.github.com/jcouyang/d7cf6c8011b3b9c3f9a7
(setq vc-follow-symlinks nil)

(use-package find-file-in-project
  :straight t
  :commands (find-file-in-project)
  :config
  (when IS-WINDOWS
    (setq ffip-find-executable find-file-in-project-program)))

(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1))

(use-package general
  :straight t
  :config
  (general-evil-setup t)
  (general-override-mode))

(use-package selectrum-prescient
  :straight
  (selectrum-prescient
    :host github :repo "raxod502/prescient.el"
    :files ("selectrum-prescient.el"))
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(provide 'init-core)
;;; init-core.el ends here
