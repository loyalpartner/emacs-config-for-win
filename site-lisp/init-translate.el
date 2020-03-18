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

(use-package google-translate
  :straight (google-translate :host github :repo "loyalpartner/google-translate")
  :commands (google-translate-at-point
             google-translate-translate
             google-translate-request)
  :init
  (setq google-translate-default-source-language "en"
        google-translate-default-target-language "zh-CN"
        google-translate--tkk-url "http://translate.google.cn"
        google-translate-base-url "http://translate.google.cn/translate_a/single"
        google-translate-listen-url "http://translate.google.cn/translate_tts"))

(use-package insert-translated-name
  :straight
  (insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name")
  :commands insert-translated-name-insert)

(defun translate-chinese-word-p (word)
    (if (and word (string-match "\\cc" word)) t nil))

;;;###autoload
(evil-define-operator evil-google-translate-operator (beg end type)
  "中英文互相翻译."
  (interactive "<R>")
  (let* ((text (buffer-substring-no-properties beg end))
         (word (thing-at-point 'word))
         (source (if (translate-chinese-word-p word) "zh-CN" "en"))
         (target (if (translate-chinese-word-p word) "en" "zh-CN")))
    (google-translate-translate source target text)))

;;;###autoload
(evil-define-operator evil-translate-and-replace-operator (beg end type)
  "查询并替换."
  (interactive "<R>")
  (let* ((text (buffer-substring-no-properties beg end))
         (source (if (translate-chinese-word-p text) "zh-CN" "en"))
         (target (if (translate-chinese-word-p text) "en" "zh-CN"))
         (json (google-translate-request source target text))
         (result (google-translate-json-translation json)))
    (when result
      (kill-region beg end)
      (insert result))))

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

(imap "C-c ." #'insert-translated-name-insert)

(nvmap "g." (lambda (&optional arg)
         (interactive "P")
         (call-interactively (if arg
                                 #'sdcv-search-pointer
                               #'sdcv-search-pointer+))))

(nvmap override
  :prefix leader-key
  "yc" #'evil-translate-and-replace-operator
  "yy" #'evil-google-translate-operator)

(provide 'init-translate)
