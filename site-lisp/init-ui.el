;;; init-ui.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-ui
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package dashboard
  :straight t
  :config
  (switch-to-buffer "*dashboard*")
  (dashboard-insert-startupify-lists))

(use-package doom-modeline
  :straight t
  :config (doom-modeline-mode 1))

(use-package which-key :straight t
  :config
  (which-key-mode 1))

(use-package hide-mode-line
  :straight t)

(provide 'init-ui)
;;; init-ui.el ends here
