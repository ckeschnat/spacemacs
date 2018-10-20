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
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
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
          '(("TODO" . org-warning)
            ("NEXT" . org-todo)
            ("SOMEDAY" . (:foreground "moccasin" :weight bold))
            ("WAITING" . (:foreground "lavender blush" :weight bold))
            ("CANCELLED" . org-done)))

    (setq org-todo-keywords
          '((sequence "TODO" "NEXT" "WAITING" "SOMEDAY" "|" "CANCELLED" "DONE")))
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
