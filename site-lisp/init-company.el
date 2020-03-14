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
  :after-call pre-command-hook after-find-file
  :commands company-abort
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        ;; company-idle-delay 0
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  :config
                                        ;(add-hook 'company-mode-hook #'evil-normalize-keymaps)

  ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
  ;; by C-x C-n, will switch from `company-yasnippet' to
  ;; `company-dabbrev-code'.
  (advice-add #'company-begin-backend :before (lambda (orig-fun &rest args)
  						                        (company-abort)))

  (general-def company-active-map
    "C-w"     nil             ; don't interfere with `evil-delete-backward-word'
    "C-n"     #'company-select-next
    "C-p"     #'company-select-previous
    "C-j"     #'company-select-next
    "C-k"     #'company-select-previous
    "C-h"     #'company-show-doc-buffer
    "C-u"     #'company-previous-page
    "C-d"     #'company-next-page
    "C-s"     #'company-filter-candidates
    ;; "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
    ;; ((featurep! :completion ivy)  #'counsel-company))
    "C-SPC"   #'company-complete-common
    "TAB"     #'company-complete-common-or-cycle
    "RET" #'company-complete
    [tab]     #'company-complete-common-or-cycle
    [backtab] #'company-select-previous)
  :hook (after-init . global-company-mode))

(imap :prefix "C-x"
  :keymaps 'override
  ;; #TODO Omni-completion
  ;; "C-l"    #'+company/whole-lines
  "C-k"    #'company-english-helper-search
  "C-f"    #'company-files
  "C-]"    #'company-etags
  "s"      #'company-ispell
  "C-s"    #'company-yasnippet
  "C-o"    #'company-capf
  "C-n"    #'company-dabbrev
  "C-p"    #'company-dabbrev-code)

;; Better sorting and filtering
(use-package company-prescient
  :straight t
  :hook (company-mode . company-prescient-mode))

(provide 'init-company)
;;; init-company.el ends here
