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
     windows-scripts
     python
     shell
     html
     markdown
     (go :variables go-tab-width 4)
     ruby
     asciidoc
     themes-megapack
     c-c++
     ;; ivy
     pdf-tools
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     ;; markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(helm-org-rifle)
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
   dotspacemacs-check-for-update nil
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
   dotspacemacs-startup-banner 'official
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
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         leuven
                         spacegray
                         sanityinc-tomorrow-eighties
                         planet
                         railscasts
                         spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Liberation Mono"
                               :size 15
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
   dotspacemacs-which-key-delay 0.2
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
   dotspacemacs-maximized-at-startup t
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
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
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
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.spacemacs.d/undo")))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))
  (spacemacs/toggle-camel-case-motion-globally-on)
  (spacemacs/toggle-automatic-symbol-highlight-on)
  (setq c-default-style "linux" c-basic-offset 4)
  (setq zenburn-override-colors-alist '(
                                        ("zenburn-bg-05" . "#131818")
                                        ("zenburn-bg-1" . "#6A714A")
                                        ))
  (load-theme 'zenburn t)
  (with-eval-after-load 'org
    ;; here goes your Org config :)

    ;; (setq org-agenda-files (list "c:/Users/NOBODY/Documents/Seafile/docs/org"))
    ;; (setq org-default-notes-file "c:/Users/NOBODY/Documents/Seafile/docs/org/notes.org")
    (setq home-org-dir "c:/Users/NOBODY/Documents/Seafile/docs/org/")
    (setq work-org-dir "c:/Users/chris.keschnat/docs/docs/org")
    (setq org-agenda-files
          (cond ((file-directory-p home-org-dir) (list home-org-dir))
                ((file-directory-p work-org-dir) (list work-org-dir))
          )
    )
    (setq org-default-notes-file (concat (car org-agenda-files) "inbox.org"))
    (setq org-startup-indented t)
    ;; More headings for refile
    ;; (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-refile-use-outline-path 'file)              ; Show full paths for refiling
    (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setq helm-org-rifle-show-path t)
    ;; (defun +org-search ()
    ;;   (interactive)
    ;;   (org-refile '(4)))
    ;; (spacemacs/set-leader-keys "or" (lambda () (interactive) (org-refile '(4))))
    (spacemacs/set-leader-keys "or" 'helm-org-rifle-agenda-files)
    (setq org-refile-allow-creating-parent-nodes 'confirm)


    ;; org-archive-subtree-hierarchical.el
    ;; modified from https://lists.gnu.org/archive/html/emacs-orgmode/2014-08/msg00109.html

    ;; In orgmode
    ;; * A
    ;; ** AA
    ;; *** AAA
    ;; ** AB
    ;; *** ABA
    ;; Archiving AA will remove the subtree from the original file and create
    ;; it like that in archive target:

    ;; * AA
    ;; ** AAA

    ;; And this give you
    ;; * A
    ;; ** AA
    ;; *** AAA


    (require 'org-archive)

    (defun org-archive-subtree-hierarchical--line-content-as-string ()
      "Returns the content of the current line as a string"
      (save-excursion
        (beginning-of-line)
        (buffer-substring-no-properties
        (line-beginning-position) (line-end-position))))

    (defun org-archive-subtree-hierarchical--org-child-list ()
      "This function returns all children of a heading as a list. "
      (interactive)
      (save-excursion
        ;; this only works with org-version > 8.0, since in previous
        ;; org-mode versions the function (org-outline-level) returns
        ;; gargabe when the point is not on a heading.
        (if (= (org-outline-level) 0)
            (outline-next-visible-heading 1)
          (org-goto-first-child))
        (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
          (while (org-goto-sibling)
            (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
          child-list)))

    (defun org-archive-subtree-hierarchical--org-struct-subtree ()
      "This function returns the tree structure in which a subtree
    belongs as a list."
      (interactive)
      (let ((archive-tree nil))
        (save-excursion
          (while (org-up-heading-safe)
            (let ((heading
                  (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
              (if (eq archive-tree nil)
                  (setq archive-tree (list heading))
                (setq archive-tree (cons heading archive-tree))))))
        archive-tree))

    (defun org-archive-subtree-hierarchical ()
      "This function archives a subtree hierarchical"
      (interactive)
      (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
            (this-buffer (current-buffer))
            (file (abbreviate-file-name
                  (or (buffer-file-name (buffer-base-buffer))
                      (error "No file associated to buffer")))))
        (save-excursion
          (setq location (org-get-local-archive-location)
                afile (org-extract-archive-file location)
                heading (org-extract-archive-heading location)
                infile-p (equal file (abbreviate-file-name (or afile ""))))
          (unless afile
            (error "Invalid `org-archive-location'"))
          (if (> (length afile) 0)
              (setq newfile-p (not (file-exists-p afile))
                    visiting (find-buffer-visiting afile)
                    buffer (or visiting (find-file-noselect afile)))
            (setq buffer (current-buffer)))
          (unless buffer
            (error "Cannot access file \"%s\"" afile))
          (org-cut-subtree)
          (set-buffer buffer)
          (org-mode)
          (goto-char (point-min))
          (while (not (equal org-tree nil))
            (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
              (if (member (car org-tree) child-list)
                  (progn
                    (search-forward (car org-tree) nil t)
                    (setq org-tree (cdr org-tree)))
                (progn
                  (goto-char (point-max))
                  (newline)
                  (org-insert-struct org-tree)
                  (setq org-tree nil)))))
          (newline)
          (org-yank)
          (when (not (eq this-buffer buffer))
            (save-buffer))
          (message "Subtree archived %s"
                  (concat "in file: " (abbreviate-file-name afile))))))

    (defun org-insert-struct (struct)
      "TODO"
      (interactive)
      (when struct
        (insert (car struct))
        (newline)
        (org-insert-struct (cdr struct))))

    (defun org-archive-subtree ()
      (org-archive-subtree-hierarchical)
      )

    (spacemacs/set-leader-keys "oa" 'org-archive-subtree-hierarchical)

    (setq org-todo-keyword-faces
          '(("TODO" . org-warning) ("SOMEDAY" . "yellow")
            ("WAITING" . (:foreground "aquamarine" :weight bold))))

    (setq org-todo-keywords
          '((sequence "TODO" "WAITING" "SOMEDAY" "DONE")))
    (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h)))

    (setq org-hide-emphasis-markers t)

    (let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
    'user
    `(org-level-8 ((t (,@headline ,@variable-tuple))))
    `(org-level-7 ((t (,@headline ,@variable-tuple))))
    `(org-level-6 ((t (,@headline ,@variable-tuple))))
    `(org-level-5 ((t (,@headline ,@variable-tuple))))
    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  )

  (define-key global-map "\ew" 'other-window)
  (define-key global-map "\ef" 'find-file)
  (define-key global-map "\eF" 'find-file-other-window)

  (global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
  (global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

  (defun previous-blank-line ()
    "Moves to the previous line containing nothing but whitespace."
    (interactive)
    (search-backward-regexp "^[ \t]*\n")
    )

  (defun next-blank-line ()
    "Moves to the next line containing nothing but whitespace."
    (interactive)
    (forward-line)
    (search-forward-regexp "^[ \t]*\n")
    (forward-line -1)
    )

  (define-key evil-normal-state-map (kbd "M-k") 'previous-blank-line)
  (define-key evil-normal-state-map (kbd "M-j") 'next-blank-line)


  (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;" "C:\\texlive\\2018\\bin\\win32;" "w:\\handmade\\misc;" (getenv "PATH")))

  (setq compilation-directory-locked nil)
  (setq casey-makescript "build.bat")

  (defun find-project-directory-recursive ()
    "Recursively search for a makefile."
    (interactive)
    (if (file-exists-p casey-makescript) t
      (cd "../")
      (find-project-directory-recursive)))

  (defun lock-compilation-directory ()
    "The compilation process should NOT hunt for a makefile"
    (interactive)
    (setq compilation-directory-locked t)
    (message "Compilation directory is locked."))

  (defun unlock-compilation-directory ()
    "The compilation process SHOULD hunt for a makefile"
    (interactive)
    (setq compilation-directory-locked nil)
    (message "Compilation directory is roaming."))

  (defun find-project-directory ()
    "Find the project directory."
    (interactive)
    (setq find-project-from-directory default-directory)
    (switch-to-buffer-other-window "*compilation*")
    (if compilation-directory-locked (cd last-compilation-directory)
      (cd find-project-from-directory)
      (find-project-directory-recursive)
      (setq last-compilation-directory default-directory)))

  (defun make-without-asking ()
    "Make the current build."
    (interactive)
    (if (find-project-directory) (compile casey-makescript))
    (other-window 1))
  ;; (define-key global-map "\em" 'make-without-asking)
  ;; (define-key evil-normal-state-map (kbd "C-]") 'make-without-asking)
  (spacemacs/set-leader-keys "oo" 'make-without-asking)
  (spacemacs/set-leader-keys "oc" 'delete-frame)

  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) ")


  ;; make org faster
  ;; https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/d8cmm7v/
  (setq gc-cons-threshold (* 511 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq garbage-collection-messages t)
)
(setq custom-file "~/.emacs.d/.cache/.custom-settings")
(load custom-file)
