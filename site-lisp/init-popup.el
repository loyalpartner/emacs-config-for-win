;;; init-popup.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: Fri Mar 13 20:35:46 2020
;; Modified: Fri Mar 13 20:35:46 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-popup
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package doom-popup
  :straight (doom-popup :host github :repo "loyalpartner/doom-popup")
  :config
  (+popup-mode 1))

(provide 'init-popup)
