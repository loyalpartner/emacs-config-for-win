;;; init-company.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-company
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package company
  :straight t
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :hook (after-init . global-company-mode))

;; Better sorting and filtering
(use-package company-prescient
  :straight t
  :init (company-prescient-mode 1))


(provide 'init-company)
;;; init-company.el ends here
