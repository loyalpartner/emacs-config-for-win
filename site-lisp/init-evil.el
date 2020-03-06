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
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
	evil-escape-key-sequence "hh"
        evil-escape-delay 0.3
        evil-symbol-word-search t)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  (evil-escape-mode 1))

(use-package evil-collection :straight t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :straight t
  :config
  (global-evil-visualstar-mode 1))

(use-package targets
  :straight
  (targets :host github :repo "noctuid/targets.el")
  :config
  (targets-setup 1)
  (targets-define-to function 'evil-defun nil object
		     :bind f 
		     :keys "f"
		     :linewise t
		     :remote-key "r"))

(use-package evil-snipe
  :straight t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;;;###autoload###
(evil-define-operator evil-eval-region-operator (beg end)
  "Evaluate selection or sends it to the open REPL, if available."
  :move-point nil
  (interactive "<r>")
  (print (eval-region beg end) ))

(nvmap :map emacs-lisp-mode-map
  "gr" #'evil-eval-region-operator)

(provide 'init-evil)
;;; init-evil.el ends here
