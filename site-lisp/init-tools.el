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
  (auto-save :host github :repo "manateelazycat/auto-save")
  :config
  (setq auto-save-silent t)
  (auto-save-enable)
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p "gpg"
                             (file-name-extension
                              (buffer-name)) t)))))


(use-package link-hint
  :straight t
  :commands (link-hint-copy-link
	     link-hint-open-link))

(use-package counsel-projectile
  :straight t
  :commands (counsel-projectile))

(provide 'init-tools)
;;; init-tools.el ends here
