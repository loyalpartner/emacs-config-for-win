;;; init-evil.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: 三月 05, 2020
;; Modified: 三月 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-evil
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(setq evil-want-keybinding nil)

(use-package evil :straight t
  :config
  (evil-mode 1))

(use-package evil-escape :straight t
  :init
  :config
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  (setq evil-escape-delay 0.3
	evil-escape-key-sequence "hh")
  (evil-escape-mode 1))

(use-package evil-collection :straight t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

(provide 'init-evil)
;;; init-evil.el ends here
