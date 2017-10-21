;;; packages.el --- common-lisp-sly layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <sam@chibi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `common-lisp-sly-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `common-lisp-sly/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `common-lisp-sly/pre-init-PACKAGE' and/or
;;   `common-lisp-sly/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst common-lisp-sly-packages
  '(auto-highlight-symbol
    (common-lisp-snippets :toggle (configuration-layer/package-usedp 'yasnippet))
    ggtags
    sly
    sly-company
    sly-quicklisp
    sly-named-readtables
    paredit))


(defun common-lisp-sly/post-init-auto-highlight-symbol ()
  (with-eval-after-load 'auto-highlight-symbol
    (add-to-list 'ahs-plugin-bod-modes 'lisp-mode)))

(defun common-lisp-sly/init-common-lisp-snippets ())

(defun common-lisp-sly/init-sly-named-readtables ()
  (use-package sly-named-readtables))

(defun common-lisp-sly/post-init-ggtags ()
  (add-hook 'common-lisp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun common-lisp-sly/init-sly ()
  "Initialize sly"
  (use-package sly
    :init
    (progn
      (spacemacs/register-repl 'sly 'sly)
      (setq sly-contribs '(sly-fancy
                           sly-indentation
                           sly-autodoc
                           sly-scratch
                           sly-stickers)
            inferior-lisp-program "sbcl")
      (when (configuration-layer/package-usedp 'sly-quicklisp)
        (push 'sly-contribs sly-quicklisp))
      (when (configuration-layer/package-usedp 'sly-named-readtables)
        (push 'sly-contribs sly-named-readtables))
      (add-hook 'sly-mode-hook #'common-lisp-sly/paredit-over-smart)
      (add-hook 'lisp-mode-hook #'common-lisp-sly/paredit-over-smart)
      (add-hook 'sly-db-mode-hook #'evil-emacs-state)
      (spacemacs/add-to-hooks 'sly-mode '(lisp-mode-hook)))
    :config
    (progn
      (sly-setup)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "'" 'sly
        "\"" 'sly-connect

        ;; compile
        "cc" 'sly-compile-file
        "cC" 'sly-compile-and-load-file
        "cl" 'sly-load-file
        "cf" 'sly-compile-defun
        "cr" 'sly-compile-region
        "cn" 'sly-remove-notes

        ;; eval
        "eb" 'sly-eval-buffer
        "eu" 'sly-undefine-function
        "ee" 'sly-eval-last-expression
        "er" 'sly-eval-region
        "eF" 'sly-eval-file
        "ef" 'sly-eval-defun
        "eo" 'sly-eval-last-expression-display-output
        "ep" 'sly-eval-print-last-expression

        ;; navigation
        "gb" 'sly-pop-find-definition-stack
        "gd" 'sly-edit-definition
        "gn" 'sly-next-note
        "gN" 'sly-previous-note

        ;; help
        "ha" 'sly-apropos
        "hA" 'sly-apropos-all
        "hb" 'sly-who-binds
        "hd" 'sly-disassemble-symbol
        "hD" 'sly-documentation-lookup
        "hh" 'sly-describe-symbol
        "hH" 'sly-hyperspec-lookup
        "hf" 'sly-describe-function
        "hp" 'sly-apropos-package
        "ht" 'sly-toggle-trace-fdefinition
        "hT" 'sly-untrace-all
        "h<" 'sly-who-calls
        "h>" 'sly-calls-who
        "hr" 'sly-who-references
        "hm" 'sly-who-macroexpands
        "hs" 'sly-who-specializes
        "hS" 'sly-who-sets

        ;; macro
        "ma" 'sly-macroexpand-all
        "me" 'macrostep-expand
        "mo" 'sly-macroexpand-1

        ;; repl
        "rc" 'sly-mrepl-clear-repl
        "re" 'sly-eval-last-expression-in-repl
        "rs" 'sly
        "rr" 'sly-restart-inferior-lisp
        "rq" 'sly-quit-lisp

        ;; stickers
        "Sb" 'sly-stickers-toggle-break-on-stickers
        "SB" 'sly-stickers-clear-buffer-stickers
        "SD" 'sly-stickers-clear-defun-stickers
        "Sf" 'sly-stickers-fetch
        "SF" 'sly-stickers-forget
        "Sn" 'sly-stickers-next-sticker
        "Sp" 'sly-stickers-prev-sticker
        "Sr" 'sly-stickers-replay
        "Sr" 'sly-stickers-clear-region-stickers
        "Ss" 'sly-stickers-dwim

        ;; Trace
        "td" 'sly-trace-dialog
        "tf" 'sly-toggle-fancy-trace))
    ;; prefix names for which-key
    (mapc (lambda (x)
            (spacemacs/declare-prefix-for-mode 'lisp-mode (car x) (cdr x)))
      '(("mh" . "help")
         ("me" . "eval")
         ("mS" . "stickers")
         ("mc" . "compile")
         ("mg" . "nav")
         ("mm" . "macro")
         ("mr" . "repl")
         ("mt" . "trace"))))
  (use-package sly-mrepl
    :after sly
    ))

(defun common-lisp-sly/init-sly-quicklisp ()
  (use-package sly-quicklisp
    :ensure t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "rl" 'sly-quickload)))

(defun common-lisp-sly/sly-named-readtables ()
  (use-package sly-named-readtables
    :ensure t))

(defun common-lisp-sly/init-sly-company ()
  "Initialize sly-company"
  (use-package sly-company

    :init
    (add-to-list 'company-backends 'sly-company)
    :config
    (add-hook 'sly-mode-hook 'sly-company-mode)))

(defun common-lisp-sly/paredit-over-smart ()
  (smartparens-strict-mode -1)
  (turn-off-smartparens-mode)
  (paredit-mode 1))

;;; packages.el ends here
