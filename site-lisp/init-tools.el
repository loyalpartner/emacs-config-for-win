;;; init-tools.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 06, 2020
;; Modified: March 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-tools
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package auto-save
  :straight
  (auto-save :host github :repo "loyalpartner/auto-save")
  :config
  (setq auto-save-silent t)
  (auto-save-enable)
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p "gpg" buffer-file-name t))
          (lambda ()
            (string-suffix-p "lua" buffer-file-name t)))))


(use-package link-hint
  :straight t
  :commands (link-hint-copy-link
             link-hint-open-link)
  :init
  (nvmap Info-mode-map "o" #'link-hint-open-link)
  (nvmap help-mode-map "o" #'link-hint-open-link)
  (nvmap helpful-mode-map "o" #'link-hint-open-link))

(use-package counsel-projectile
  :straight t
  :commands (counsel-projectile
             counsel-projectile-switch-project)
  :config
  (counsel-projectile-mode))

(use-package winum
  :straight t
  :after-call pre-command-hook find-file-hook
  :config
  (winum-setup))

;;;###autoload
(defun winum-setup ()
  (winum-mode 1)
  (mapc (lambda (n)
          (let* ((key (number-to-string n))
                 (func (intern (format "winum-select-window-%d" n))))
            (nvmap
              :prefix leader-key
              :keymaps 'override
              key `(,func :which-key ,(format "[%s]" n)))))
        (number-sequence 1 9)))

(provide 'init-tools)
;;; init-tools.el ends here
