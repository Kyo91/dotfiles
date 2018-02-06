;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
	 csv
     graphviz
     shell-scripts
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup 't
                      auto-completion-tab-key-behavior nil)
     clojure
     common-lisp-sly 
     deft
	 elastic
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "Kyo91")
            ("irc.mozilla.org"
             :port "6697"
             :ssl t
             :nick "Kyo91"))
          erc-enable-sasl-auth t)
     (elfeed :variables
             rmh-elfeed-org-files (list "~/dotfiles/elfeed.org"
                                        )
             elfeed-enable-web-interface t)
     emacs-lisp
     git
     github
     gtags
     (haskell :variables haskell-completion-backend 'intero)
     html
     ivy
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     javascript
     markdown
     music
     ocaml
     (org :variables org-enable-github-support t)
     osx
     pdf-tools
     python
     rust
     (scala :variables scala-enable-eldoc t
            scala-auto-insert-asterisk-in-comments t
            scala-indent:use-javadoc-style t)
     scheme
     sql
     syntax-checking
     themes-megapack
     vimscript
     yaml
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
			shell-default-shell 'eshell
			shell-protect-eshell-prompt t
			)
     ;; version-control
     ;; spell-checking
     kyo ;; My personal layer
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(fish-completion dockerfile-mode docker docker-tramp hyperbole evil-string-inflection erc-status-sidebar dired+ helpful)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(naquadah
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative 't
                                         :disabled-for-modes
                                         dired-mode
                                         doc-view-mode
                                         pdf-view-mode
                                         :size-limit-kb 1000)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode 't
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq explicit-shell-file-name "/bin/bash")
  (setq shell-file-name "bash")
  (setq-default flycheck-scalastylerc "/usr/local/etc/scalastyle_config.xml")
  (setq geiser-active-implementations '(chez))
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program (if (string-equal system-type "darwin") "open -a FireFoxDeveloperEdition" "firefox"))
  (add-hook 'tuareg-mode-hook
   (lambda()
    (when (functionp 'prettify-symbols-mode)
     (prettify-symbols-mode))))
  (add-hook 'eshell-mode-hook #'kyo/initialize-eshell)
  (setq-default elfeed-search-filter "@2-week-ago +unread")
  (add-to-load-path "~/.emacs.d/private/dired+/")
  )

(defun org-configuration ()
  "Configuration for Emacs org-mode code.
This function must be called within a with-eval-after-load block to ensure that settings are for the ELPA
org-mode rather than the one shipped with emacs itself"
  )

(defun kyo/stumpwm-connect ()
  (interactive)
  (sly-connect "127.0.0.1" 5555))

(defun chmodx-buffer ()
  (interactive)
  (shell-command (concat "chmod +x " buffer-file-name)))

(defun kyo/setup-ibuffer ()
  (spacemacs/set-leader-keys "bi" #'ibuffer)
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-auto-mode 1)))
  (setq ibuffer-show-empty-filter-groups nil
		ibuffer-expert t)
  (defalias 'list-buffers 'ibuffer))

(defun kyo/slime-settings ()
  (bind-key (kbd "C-j") 'newline-and-indent)
  (paredit-mode 1))

(defun kyo/slime-qlot-exec (directory)
  "Allows Slime to work with qlot-installed libraries"
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
			   :program-args '("exec" "ros" "-S" "." "run")
			   :directory directory
			   :name 'qlot
			   :env (list (concat "PATH="
								  (mapconcat 'identity exec-path ":"))
						  (concat "QUICKLISP_HOME="
								  (file-name-as-directory directory) "quicklisp/"))))

(defun kyo/slime-qlot-here ()
  (interactive)
  (let* ((directory (file-name-directory buffer-file-name))
		 (project (last
				   (split-string directory "/")
				   1)))
	(kyo/slime-qlot-exec directory)
	(message project)
	(slime-reload-system project)))

(defun kyo/setup-lisp ()
  (setq inferior-lisp-program "sbcl")
  )

(defun eww-more-readable ()
  "Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20) ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload))                                    ;; apply eww-header changes

(defun kyo/eww-setup ()
  (interactive)
  (evilified-state-evilify-map eww-mode-map
    :mode eww
    :eval-after-load eww
    :bindings
    "R" #'eww-more-readable
    "r" #'eww-readable))


(defun kyo/setup-counsel ()
  (setq counsel-grep-base-command
		"rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (spacemacs/set-leader-keys "ss" 'counsel-grep-or-swiper))

(defun kyo/erc-config ()
  (spacemacs/set-leader-keys-for-major-mode 'erc-mode
    (kbd "bb") 'erc-switch-to-buffer)
  (erc-social-graph-enable)
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))
  (setq erc-join-buffer 'bury)
  (setq erc-autojoin-channels-alist
        '(("freenode.net"
           "#lisp" "#lispweb" "#lispgames" "#perl6" "#emacs" "#proglangdesign")
          ("mozilla.org"
           "#rust-beginners"))
        erc-rename-buffers t
        erc-interpret-mirc-color t
        erc-social-graph-dynamic-graph t))

(defun kyo/eww-browse-url-other-window (url &optional newwin)
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (eww-browse-url url newwin)))

(defun company-eshell-autosuggest-candidates (prefix)
  (let* ((history
          (delete-dups
           (mapcar (lambda (str)
                     (string-trim (substring-no-properties str)))
                   (ring-elements eshell-history-ring))))
         (most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        history)))
    (when most-similar
      `(,most-similar))))


(defun company-eshell-autosuggest--prefix ()
  (let ((prefix
         (string-trim-left
          (buffer-substring-no-properties
           (save-excursion
             (eshell-bol)
			 (point))
           (save-excursion (end-of-line) (point))))))
    (if (not (string-empty-p prefix))
        prefix
      'stop)))

(defun company-eshell-autosuggest (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell))
    (prefix (and (eq major-mode 'eshell-mode)
                 (company-eshell-autosuggest--prefix)))
    (candidates (company-eshell-autosuggest-candidates arg))))

(defun setup-eshell-autosuggest ()
  (with-eval-after-load 'company
    (setq-local company-backends '(company-eshell-autosuggest))
    (setq-local company-frontends '(company-preview-frontend))))

(defun merlin-eldoc--type-display (_bounds type &optional quiet)
  (if (not type)
      (unless quiet (message "<no information>"))
    (let ((count 0)
          (pos   0))
      (merlin/display-in-type-buffer type)
      (while (and (<= count 8)
                  (string-match "\n" type pos))
        (setq pos (match-end 0))
        (setq count (1+ count)))
      (with-current-buffer merlin-type-buffer-name
        (font-lock-fontify-region (point-min) (point-max))
        (buffer-string)))))

(defun merlin-eldoc/eldoc-function ()
  (unless (let ((at-open? (rx bol (* space) "open" eow)))
            (save-excursion
              (skip-chars-backward "\n \t")
              (s-matches? at-open? (buffer-substring (line-beginning-position) (line-end-position)))))
    (when (merlin--type-enclosing-query)
      (-when-let (res (noflet ((merlin--type-display
                                (bounds type &optional quiet)
                                (merlin-eldoc--type-display bounds type quiet)))
                        (merlin-type-enclosing-go-up)))
        (s-trim res)))))

(defun merlin-eldoc/setup ()
  (eldoc-mode +1)
  (setq-local eldoc-documentation-function #'merlin-eldoc/eldoc-function))

(defun eww-more-readable ()
  "Makes eww more pleasant to use. Run it after eww buffer is loaded."
  (interactive)
  (setq eww-header-line-format nil)               ;; removes page title
  (setq mode-line-format nil)                     ;; removes mode-line
  (set-window-margins (get-buffer-window) 20 20) ;; increases size of margins
  (redraw-display)                                ;; apply mode-line changes
  (eww-reload))                                    ;; apply eww-header changes

(defun kyo/eww-setup ()
  (interactive)
  (evilified-state-evilify-map eww-mode-map
	:mode eww-mode
	:eval-after-load eww
	:bindings
	"R" #'eww-more-readable
	"r" #'eww-readable)
  )

(defun kyo/elfeed-show-visit-eww ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Sent to browser: %s" link)
      (eww link))))

(defun kyo/es-request ()
  (interactive)
  (goto-char 1)
  (let* ((firstline (thing-at-point 'line t))
		 (command-parts (split-string (s-chomp firstline)))
		 (json-start (progn (forward-line 1) (point)))
		 (body (buffer-substring-no-properties json-start (point-max)))
		 (request (concat "curl -X "
						  (first command-parts)
						  " -s http://localhost:9200"
						  (second command-parts)
						  " -d '"
						  body
						  "'"))
		 (response-buffer (switch-to-buffer-other-window "*ES_Response*")))
	(json-mode)
	(message request)
	(shell-command request response-buffer)
	(json-mode-beautify)
	(other-window -1)))

(defun kyo/eww-new ()
  (interactive)
  (let* ((url (read-from-minibuffer "Enter URL or keywords: "))
         (buffer-name (concat "*eww - " url "*")))
    (switch-to-buffer (generate-new-buffer buffer-name))
    (eww-mode)
    (eww url)))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (require 'dired+)
  (setq clojure-enable-fancify-symbols 't)
  (setq eshell-buffer-shorthand 't)
  ;; (setq browse-url-browser-function 'kyo/eww-browse-url-other-window)
  (setq browse-url-browser-function #'eww-browse-url)
  (setq shr-external-browser "firefox-developer")
  (with-eval-after-load 'org (org-configuration))
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))
        ivy-height 20)
  (add-hook 'java-mode-hook
            (setq indent-tabs-mode t
                  tab-width 4))
  (add-hook 'groovy-mode-hook
			(setq indent-tabs-mode t
				  tab-width 4))
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "oo" 'kyo/dired-open)
  (kyo/setup-ibuffer)
  (kyo/setup-lisp)
  (kyo/setup-counsel)
  (kyo/eww-setup)
  (with-eval-after-load 'erc
    (kyo/erc-config))
  (setq ensime-startup-notification nil)
  (spacemacs/set-leader-keys (kbd "bO") #'switch-to-buffer-other-window)
  (with-eval-after-load 'em-prompt
    (defun eshell-next-prompt (n)
      "Move to end of Nth next prompt in the buffer.
See `eshell-prompt-regexp'."
      (interactive "p")
      (re-search-forward eshell-prompt-regexp nil t n)
      (when eshell-highlight-prompt
        (while (not (get-text-property (line-beginning-position) 'read-only) )
          (re-search-forward eshell-prompt-regexp nil t n)))
      (eshell-skip-prompt))


    (defun eshell-previous-prompt (n)
      "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
      (interactive "p")
      (backward-char)
      (eshell-next-prompt (- n))))
  (global-prettify-symbols-mode +1)
  (add-to-list 'exec-path "/home/sam/.opam/system/bin/")
  (add-hook 'merlin-mode-hook #'merlin-eldoc/setup)
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)
  (spacemacs/set-leader-keys (kbd "hh") #'hyperbole)
  (kyo/eww-setup)

  ;; Stop spacemacs from overriding these
  (spacemacs/set-leader-keys (kbd "hdf") #'helpful-function)
  (spacemacs/set-leader-keys (kbd "hdv") #'helpful-variable)
  (spacemacs/set-leader-keys (kbd "hdk") #'helpful-key)
  (spacemacs/set-leader-keys (kbd "hd.") #'helpful-symbol)
  (spacemacs/set-leader-keys (kbd "ae") #'kyo/eww-new)

  (spacemacs/set-leader-keys (kbd "sr") #'counsel-rg)
  )


(defun kyo/initialize-eshell ()
  (message "Eshell mode hook!")
  (company-mode 1)
  (setup-eshell-autosuggest)
  (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
  (define-key eshell-mode-map (kbd "M-p") 'counsel-esh-history))

;; END OF USER CONFIG ;;

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(merlin-command "/home/sam/.opam/4.05.0/bin/ocamlmerlin")
 '(merlin-completion-with-doc t t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(ocp-indent-path "/home/sam/.opam/4.05.0/bin/ocp-indent")
 '(package-selected-packages
   (quote
    (dired-filter dired-hacks-utils es-mode spark erc-status-sidebar counsel-tramp evil-string-inflection znc pdf-tools dired+ xpm helpful elisp-refs loop list-utils intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode hyperbole nov elfeed-web elfeed-org elfeed-goodies ace-jump-mode elfeed tablist fish-completion dockerfile-mode docker-tramp docker csv-mode erc-terminal-notifier graphviz-dot-mode emms erc-colorize erc-youtube erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks sly-named-readtables sly-quicklisp sly-company sly insert-shebang fish-mode company-shell org-category-capture xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help noflet ensime sbt-mode scala-mode doom-nova-theme doom-themes web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode slime-company slime common-lisp-snippets yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode company-anaconda anaconda-mode pythonic perl6-mode flycheck-perl6 ibuffer-projectile zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme imenu-anywhere deft web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data vimrc-mode dactyl-mode reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl evil-smartparens counsel-gtags mmm-mode markdown-toc markdown-mode magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache ht gh-md toml-mode racer flycheck-rust cargo rust-mode winum gradle-mode thrift groovy-mode ox-gfm yaml-mode sql-indent clojure-snippets clj-refactor inflections edn cider multiple-cursors paredit seq queue peg cider-eval-sexp-fu clojure-mode smeargle orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download magit-gitflow htmlize gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash async aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build spacemacs-theme)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(racer-rust-src-path
   "/home/sam/.rustup/toolchains/nightly-2018-01-02-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
