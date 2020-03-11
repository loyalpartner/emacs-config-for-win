;;; init-evil.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: 三月 05, 2020
;; Modified: 三月 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-evil
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defvar evil-collection-disabled-list
  '(anaconda-mode
    buff-menu
    comint
    company
    custom
    eldoc
    elisp-mode
    ert
    free-keys
    help
    helm
    image
    kotlin-mode
    occur
    package-menu
    ruby-mode
    simple
    slime
    lispy))


(defface evil-tooltip-face
  '((t (:foreground "green" :background "gray12")))
  "Face for sdcv tooltip"
  :group 'sdcv)

(setq evil-want-keybinding nil
      evil-symbol-word-search t)

(use-package evil :straight t
  :demand t
  :after-call pre-command-hook find-file-hook
  :config
  (evil-mode 1))

(use-package evil-escape :straight t
  :after-call pre-command-hook
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
	evil-escape-key-sequence "hh"
        evil-escape-delay 0.3)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  (evil-escape-mode 1))


(use-package evil-collection :straight t
  :defer 1
  :config
  (setq evil-collection-mode-list
	(seq-remove (lambda (mode)
		      (memq mode evil-collection-disabled-list))
		    evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-nerd-commenter
  :straight t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

(use-package evil-surround
  :straight t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :init
  (evil-define-key 'operator evil-surround-mode-map "s" 'evil-surround-edit)
  (evil-define-key 'operator evil-surround-mode-map "S" 'evil-Surround-edit)

  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "gS" 'evil-Surround-region))

(use-package evil-visualstar
  :straight t
  :config
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;; (use-package targets
;;   :straight
;;   (targets :host github :repo "noctuid/targets.el")
;;   ;; :hook (pre-command-hook . targets-setup)
;;   :after-call pre-command-hook 
;;   :config
;;   (targets-setup 1)
;;   (targets-define-to function 'evil-defun nil object
;; 		     :bind f 
;; 		     :keys "f"
;; 		     :linewise t
;; 		     :remote-key "r"))

(use-package evil-snipe
  :straight t
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :after-call pre-command-hook
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package exato
  :straight t
  :commands evil-outer-xml-attr evil-inner-xml-attr)

(use-package evil-args
  :straight t
  :commands (evil-inner-arg evil-outer-arg))

(use-package evil-multiedit
  :straight t
  :after-call pre-command-hook
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-lion
  :straight t
  :commands (evil-lion-left evil-lion-right)
  :init
  (nvmap
    "gl" 'evil-lion-left
    "gL" 'evil-lion-right))

;;;###autoload###
(evil-define-operator evil-eval-region-operator (beg end)
  "Evaluate selection or sends it to the open REPL, if available."
  :move-point nil
  (interactive "<r>")
  (let* ((expr (format "(progn %s)" (buffer-substring-no-properties beg end)))
	 (result (pp-to-string (eval (read expr))))
	 (posframe-buffer-name "*eval-result*"))
    (posframe-show posframe-buffer-name
		   :string (format "⇒ %s" result)
		   :position (line-end-position)
		   :background-color (face-attribute 'evil-tooltip-face :background)
		   :foreground-color (face-attribute 'evil-tooltip-face :foreground))
    (unwind-protect
	(push (read-event) unread-command-events)
      (posframe-delete posframe-buffer-name))))

;;;###autoload
(evil-define-text-object evil-whole-buffer (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (evil-range (point-min) (point-max) type))

;;;###autoload
(evil-define-text-object evil-inner-defun (count &optional _beg _end type)
  "Text object to select the whole buffer."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'defun)
    (evil-range beg end type)))

;;;###autoload
(evil-define-text-object evil-inner-url (count &optional _beg _end type)
  "Text object to select the inner url at point.

This excludes the protocol and querystring."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     (save-excursion
       (goto-char beg)
       (re-search-forward "://" end t))
     (save-excursion
       (goto-char end)
       (- (if-let (pos (re-search-backward "[?#]" beg t))
              pos
            end)
          (if (evil-visual-state-p)
              1
            0)))
     type)))

;;;###autoload
(evil-define-text-object evil-outer-url (count &optional _beg _end type)
  "Text object to select the whole url at point."
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'url)
    (evil-range
     beg (- end (if (evil-visual-state-p) 1 0))
     type)))


(nvmap :map emacs-lisp-mode-map
  "gr" #'evil-eval-region-operator)

(nvmap "gc" #'evilnc-comment-operator)

(omap! "a" evil-inner-arg evil-outer-arg)
(omap! "c" evilnc-inner-comment evilnc-outer-commenter)
(omap! "f" evil-inner-defun evil-inner-defun)
(omap! "g" evil-whole-buffer evil-whole-buffer)
(omap! "u" evil-inner-url evil-outer-url)
(omap! "x" evil-inner-xml-attr evil-outer-xml-attr)

(nvmap
  "]b" #'next-buffer
  "[b" #'previous-buffer
  ;; #TODO
  ;; "]f" #'+evil/next-file
  ;; "[f" #'+evil/previous-file
  "]d" #'git-gutter:next-hunk
  "[d" #'git-gutter:previous-hunk
  "]t" #'hl-todo-next
  "[t" #'hl-todo-previous
  "]e" #'next-error
  "[e" #'previous-error)


(provide 'init-evil)
;;; init-evil.el ends here
