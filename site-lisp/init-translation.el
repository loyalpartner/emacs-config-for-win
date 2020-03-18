;;; init-translation.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: Mon Mar 16 15:48:46 2020
;; Modified: Mon Mar 16 15:48:46 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-translation
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

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

(use-package company-english-helper
  :straight
  (company-english-helper :type git :flavor melpa :host github :repo "manateelazycat/company-english-helper")
  :commands toggle-company-english-helper company-english-helper-search)

(defalias 'fy 'sdcv-search-pointer+ "翻译单词")

(general-def '(visual normal) Info-mode-map
  "w" #'evil-forward-word-begin
  "W" #'evil-forward-WORD-begin
  "e" #'evil-forward-word-end
  "E" #'evil-forward-WORD-end
  "b" #'evil-backward-word-begin
  "B" #'evil-backward-WORD-begin
  "h" #'evil-backward-char
  "j" #'evil-next-line
  "k" #'evil-previous-line
  "l" #'evil-forward-char
  "gv" #'evil-visual-restore)

(nvmap "g." (lambda (&optional arg)
              (interactive "P")
              (call-interactively (if arg
                                      #'sdcv-search-pointer
                                    #'sdcv-search-pointer+))))

(provide 'init-translation)
