;;; init-pdf.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: Sun Mar 15 11:10:40 2020
;; Modified: Sun Mar 15 11:10:40 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-pdf
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  :config
  ;; (map! :map pdf-view-mode-map :gn "q" #'kill-current-buffer)
  (define-key pdf-view-mode-map (kbd "q") #'kill-current-buffer)

  (setq-default pdf-view-display-size 'fit-page
                pdf-view-use-scaling t
                pdf-view-use-imagemagick nil))

(provide 'init-pdf)
