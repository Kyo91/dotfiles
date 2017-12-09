;;; packages.el --- elasticsearch-api layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Samuel Blumenthal <sblumenthal@sblumenthal-mbp>
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
;; added to `elasticsearch-api-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `elasticsearch-api/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `elasticsearch-api/pre-init-PACKAGE' and/or
;;   `elasticsearch-api/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst elastic-packages
  '(es-mode org-babel)
  "The list of Lisp packages required by the elasticsearch-api layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun elastic/post-init-org-babel ()
  (org-babel-do-load-language
   'org-babel-load-languages
   '((elasticsearch . t))))


(defun elastic/init-es-mode ()
  (use-package es-mode
    :ensure t
    :init 
	(add-to-list 'auto-mode-alist '("\\.es" . es-mode))     
    :config
    (progn
      (setq es-always-pretty-print t)
      (spacemacs/set-leader-keys-for-major-mode 'es-mode
        "j" #'es-goto-next-request
        "k" #'es-goto-previous-request
        "c" #'es-execute-request-dwim
        "y" #'es-copy-as
        "u" #'es-set-endpoint-url
        "m" #'es-set-request-method))))


;;; packages.el ends here
