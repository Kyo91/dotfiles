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

;; Load in private configurations
(load-file "~/.emacs.d/private/kyo/private.el")
(load "~/.emacs.d/private/kyo/init-eshell.el")

(defconst kyo-packages
  '(thrift
    smartparens
    evil-smartparens
    ggtags
    counsel-gtags
    counsel
    magit
    org
    org-projectile
    imenu-anywhere
    ;; For gradle syntax highlighting & gradle commands
    slime
    groovy-mode
    gradle-mode
    perl6-mode
    flycheck-perl6)
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

(defun kyo/post-init-ggtags ()
  (add-hook 'prog-mode-hook (ggtags-mode 1))
  (spacemacs/set-leader-keys "o." #'ggtags-find-definition)
  (spacemacs/set-leader-keys "o," #'ggtags-prev-mark)
  (spacemacs/set-leader-keys "or" #'ggtags-find-reference)
  (spacemacs/set-leader-keys "os" #'ggtags-find-symbol)
  (spacemacs/set-leader-keys "ol" #'ggtags-view-tag-history)
  (spacemacs/set-leader-keys "od" #'ggtags-visit-project-root)
  )

;; (defun kyo/init-slime-company ()
;;     (use-package slime-company))

;; (defun kyo/init-slime-repl-ansi-color ()
;;   (use-package slime-repl-ansi-color))

(defun kyo/init-counsel-gtags ()
  (use-package counsel-gtags
    ))

(defun kyo/post-init-smartparens ()
  (smartparens-global-mode)
  )



(defun kyo/post-init-org ()
  (setq org-agenda-files '("~/agenda" "~/.deft"))
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        org-default-notes-file "/Users/sblumenthal/agenda/capture.org"
        org-enforce-todo-dependencies 't
        org-enforce-todo-checkbox-dependencies 't)
  (setq org-capture-templates           ; Template for storing capture templates by date.
        '(("t" "General TODO" entry (file+datetree "~/agenda/capture.org")
           "* TODO %^{Description}\n %i\n"))
        )
  ;; (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  ;; (advice-add 'evil-quit :before 'org-save-all-org-buffers)
  )

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
    (async-shell-command (concat gradle " " commandslime-company ))))

(defun kyo/gradle-apply-spotless ()
  (interactive)
  (kyo/run-gradle-command-async "spotlessApply"))

(defun kyo/gradle-check ()
  (intera<foo>
ctive)
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
              "ob" 'kyo/gra    // TODO write this method for processing long timestamps into DateTime fields w/ helper method for converting one column at a time.
dle-build
              "ot" 'kyo/gradle-test
              "oa" 'kyo/gradle-apply-spotless
              "oc" 'kyo/gradle-check
              "og" 'kyo/run-gradle-command-async)))

(defun kyo/post-init-org-projectile ()
  (eval-after-load 'org-mode
    (setq org-projectile:projects-file "~/agenda/projects/TODOs.org")
    ))

(defun kyo/post-init-magit ()
  )

(defun kyo/init-imenu-anywhere ()
    (use-package imenu-anywhere
      :config (spacemacs/set-leader-keys "jI" #'ivy-imenu-anywhere)))

(defun kyo/init-perl6-mode ()
  (use-package perl6-mode
    :ensure t
    :defer t
    :init (add-hook 'perl6-mode-hook (lambda ()
                                       (push '(?< . ("<" . ">")) evil-surround-pairs-alist)))
    (sp-local-pair 'perl6-mode "<" ">")
    (add-hook 'perl6-mode-hook (lambda () (evil-smartparens-mode -1)))))

(defun kyo/init-flycheck-perl6 ()
  (use-package flycheck-perl6
    :ensure flycheck))

(defun kyo/post-init-slime ()
  )

(defun kyo/post-init-counsel ()
  (spacemacs/set-leader-keys "ss" 'counsel-grep-or-swiper))
;;; packages.el ends here
