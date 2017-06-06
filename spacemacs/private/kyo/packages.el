;;; packages.el --- kyo layer packages file for Spacemacs.
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
;; added to `kyo-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `kyo/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `kyo/pre-init-PACKAGE' and/or
;;   `kyo/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst kyo-packages
  '(thrift
    smartparens
    evil-smartparens
    ggtags
    counsel-gtags
    magit
    org
    ;; For gradle syntax highlighting & gradle commands
    groovy-mode
    gradle-mode)
  "The list of Lisp packages required by the kyo layer.

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

(defun kyo/init-thrift ()
    (use-package thrift
      :mode ("\\.thrift" . thrift-mode)
      :defer t
      ))

(defun kyo/init-ggtags ()
  (use-package ggtags
    :config
    (add-hook 'prog-mode-hook (ggtags-mode 1))
    (spacemacs/set-leader-keys "o." #'ggtags-find-definition)
    (spacemacs/set-leader-keys "o," #'ggtags-prev-mark)
    (spacemacs/set-leader-keys "or" #'ggtags-find-reference)
    (spacemacs/set-leader-keys "os" #'ggtags-find-symbol)
    (spacemacs/set-leader-keys "ol" #'ggtags-view-tag-history)
    (spacemacs/set-leader-keys "od" #'ggtags-visit-project-root)
    ))

(defun kyo/init-counsel-gtags ()
  (use-package counsel-gtags
    ))

(defun kyo/post-init-smartparens ()
  (smartparens-global-mode)
  )



(defun kyo/post-init-org ()
  (setq org-agenda-files '("/Users/sblumenthal/agenda/")
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        org-default-notes-file "/Users/sblumenthal/agenda/capture.org"
        org-enforce-todo-dependencies 't
        org-enforce-todo-checkbox-dependencies 't)
  (setq org-capture-templates           ; Template for storing capture templates by date.
        '(("t" "General TODO" entry (file+datetree "/Users/sblumenthal/agenda/capture.org")
           "* TODO %^{Description}\n %i\n"))
        ))

(defun kyo/init-evil-smartparens ()
  (use-package evil-smartparens
    :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

;; gradlew is a wrapper for gradle used by my company for some extra functionality on top of gradle
;; if exists for a project, it'll be at the project root.
(defun kyo/run-gradle-command-async (command)
  (interactive "MCommand: ")
  (let* ((gradlew (concat (projectile-project-root) "/gradlew"))
         (gradle (if (file-executable-p gradlew) gradlew "gradle")))
    (async-shell-command (concat gradle " " command))))

(defun kyo/gradle-apply-spotless ()
  (interactive)
  (kyo/run-gradle-command-async "spotlessApply"))

(defun kyo/gradle-check ()
  (interactive)
  (kyo/run-gradle-command-async "check"))

(defun kyo/gradle-build ()
  (interactive)
  (kyo/run-gradle-command-async "build"))

(defun kyo/gradle-test ()
  (interactive)
  (kyo/run-gradle-command-async "test"))


(defun kyo/dired-open (arg file-list)
  "Use OSX 'open' command to open the file at point (for instance a html file in browser)."
  (interactive (list
                current-prefix-arg
                (dired-get-marked-files t current-prefix-arg)))
  (dired-do-shell-command "open *" arg file-list))

(defun kyo/init-groovy-mode ()
  (use-package groovy-mode
    :mode ("\\.gradle" . groovy-mode)))

(defun kyo/init-gradle-mode ()
  (use-package gradle-mode
    :mode ("\\.java" . gradle-mode)
    :config (add-hook 'magit-status-mode-hook '(lambda () (gradle-mode 1)))
    :config (spacemacs/set-leader-keys-for-minor-mode 'gradle-mode
              "ob" 'kyo/gradle-build
              "ot" 'kyo/gradle-test
			  "oa" 'kyo/gradle-apply-spotless
			  "oc" 'kyo/gradle-check
			  "oo" 'kyo/run-gradle-command-async
			  )))

(defun kyo/post-init-magit ()
  )
;;; packages.el ends here
