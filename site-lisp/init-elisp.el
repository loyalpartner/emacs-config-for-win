;;; init-elisp.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-elisp
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package elisp-mode
  :hook ((emacs-lisp-mode . emacs-lisp-extend-imenu)
	 (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package helpful
  :straight t
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :init
  ;h(global-set-key [remap describe-function] #'helpful-callable)
  ;h(global-set-key [remap describe-command]  #'helpful-command)
  ;h(global-set-key [remap describe-variable] #'helpful-variable)
  ;h(global-set-key [remap describe-key]      #'helpful-key)
					;h(global-set-key [remap describe-symbol]   #'helpful-symbol)
  )


(general-def help-map
  "f" #'helpful-callable
  "v" #'helpful-variable
  "k" #'helpful-key)

(eval-after-load 'lispy
  (nmap :keymaps '(emacs-lisp-mode-map override)
    "gd" #'lispy-goto-symbol))

;;;###autoload
(defun emacs-lisp-extend-imenu ()
  "Improve imenu support in `emacs-lisp-mode', including recognition for Doom's API."
  (setq imenu-generic-expression
        `(("Section" "^[ \t]*;;;;*[ \t]+\\([^\n]+\\)" 1)
          ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
          ("Package" "^\\s-*(\\(?:;;;###package\\|package!\\|use-package!?\\|after!\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
          ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
          ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
          ("Modeline segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
          ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine\\)?-advice\\)\\) +\\([^ )\n]+\\)" 1)
          ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
          ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
          ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))


(provide 'init-elisp)
;;; init-elisp.el ends here
