;;; init-editor.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-editor
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package yasnippet
  :straight t
  ;:hook (after-init . yas-global-mode)
  :hook ((text-mode prog-mode conf-mode snippet-mode) . yas-global-mode))

(use-package yasnippet-snippets
    :straight t
    :defer 1)

(provide 'init-editor)