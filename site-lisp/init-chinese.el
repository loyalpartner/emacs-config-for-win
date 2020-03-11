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

(use-package sdcv
  :straight
  (sdcv :type git :flavor melpa :host github :repo "loyalpartner/sdcv")
  ;; (sdcv :type git :flavor melpa :host github :repo "manateelazycat/sdcv")
  :commands (sdcv-search-pointer+
	     sdcv-search-pointer
	     sdcv-search-input
	     sdcv-search-input+
	     sdcv-translate-result
	     sdcv-pick-word)
  :config
  (setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic")
        sdcv-say-word-p t
        sdcv-tooltip-timeout 20)
  (setq sdcv-dictionary-simple-list
	'("懒虫简明英汉词典" "懒虫简明汉英词典" "朗道英汉字典5.0" "朗道汉英字典5.0" "新华字典")
	sdcv-dictionary-complete-list
	'("懒虫简明英汉词典" "懒虫简明汉英词典" "朗道英汉字典5.0" "朗道汉英字典5.0" "牛津英汉双解美化版" "新华字典")))

;; (advice-add #'sdcv-search-with-dictionary :around
;; 	    (lambda (orig-fn &rest args)
;; 	      (let ((word-arg (car args)))
;; 		(unless word-arg
;; 		  (setq word-arg (sdcv-region-or-word))
;; 		  ;; (deactivate-mark)
;; 		  )
;; 		(setcar args (format "\"%s\"" word-arg))
;; 		(apply orig-fn args))))


(use-package company-english-helper
  :straight
  (company-english-helper :type git :flavor melpa :host github :repo "manateelazycat/company-english-helper")
  :commands toggle-company-english-helper company-english-helper-search)

(imap "M-c" #'pyim-convert-string-at-point)

(nvmap "g." #'sdcv-search-pointer+)

(set-default-font "SauceCodePro NF")
(provide 'init-chinese)
