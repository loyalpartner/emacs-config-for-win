;;; init-chinese.el ends here

;;; init-chinese.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: Mon Mar  9 17:46:31 2020
;; Modified: Mon Mar  9 17:46:31 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-chinese
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(use-package posframe
  :straight t)

(defun private/pyim-english-prober ()
  (cond ((and (boundp 'insert-translated-name-active-overlay)
              insert-translated-name-active-overlay)
         nil)
        (t '(pyim-probe-dynamic-english
             pyim-probe-isearch-mode
             pyim-probe-program-mode
             pyim-probe-org-structu re-template))))

(use-package pyim
  :straight t
  :demand t
  :config
  (setq default-input-method "pyim"
        pyim-default-scheme 'xiaohe-shuangpin
        pyim-page-tooltip 'posframe
        pyim-page-length 5)
  (setq-default pyim-english-input-switch-functions (private/pyim-english-prober))
  (setq-default pyim-punctuation-half-width-functions
		'(pyim-probe-punctuation-line-beginning
		  pyim-probe-punctuation-after-punctuation))
  :after-call after-find-file pre-command-hook)

(use-package pyim-basedict
  :straight t
  :after pyim
  :config
  (pyim-basedict-enable))

(imap "M-c" #'pyim-convert-string-at-point)

(set-default-font "SauceCodePro NF")

(provide 'init-chinese)
