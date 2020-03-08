;;; init.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: 三月 05, 2020
;; Modified: 三月 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'prog-mode-hook 'electric-indent-local-mode)

(add-to-list 'load-path
             (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-straight)
(require 'init-ui)
(require 'init-core)
(require 'init-evil)
(require 'init-tools)
(require 'init-vc)
(require 'init-company)
(require 'init-elisp)
(require 'init-keybindings)
