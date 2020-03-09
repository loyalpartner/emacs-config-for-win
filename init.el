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

(add-to-list 'load-path
             (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-straight)
(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  (require 'benchmark-init-modes)
  (require 'benchmark-init)
  (benchmark-init/activate)

  ;; 下面才写你的其它配置

  (require 'init-ui)
  (require 'init-core)
  (require 'init-evil)
  (require 'init-editor)
  (require 'init-tools)
  (require 'init-vc)
  (require 'init-company)
  (require 'init-elisp)
  (require 'init-lispy)
  (require 'init-keybindings))
