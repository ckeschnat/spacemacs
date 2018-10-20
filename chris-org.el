(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(with-eval-after-load 'org
    ;; here goes your Org config :)
    (load "~/.spacemacs.d/org-archive-subtree-hierarchical")

    (setq org-startup-indented t)

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

    (spacemacs/set-leader-keys "oc" 'org-capture)

    (setq org-todo-keyword-faces
          '(("TODO" . org-warning)
            ("NEXT" . org-todo)
            ("SOMEDAY" . (:foreground "moccasin" :weight bold))
            ("WAITING" . (:foreground "lavender blush" :weight bold))
            ("CANCELLED" . org-done)))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)" "DONE(d)")))
    (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h)))
                                        ; Tags with fast selection keys
    (setq org-tag-alist (quote (
                                ("@home" . ?h)
                                ("@work" . ?w)
                                ("WAITING" . ?w)
                                ("PERSONAL" . ?P)
                                ("WORK" . ?W)
                                ("NOTE" . ?n)
                                ("CANCELLED" . ?c)
                                ("FLAGGED" . ??)
                                )))

    (setq org-capture-templates
          (quote (("t" "todo" entry (file "") "* TODO %?\n%U\n%a\n")
                  ("n" "note" entry (file "") "* %? :NOTE:\n%U\n%a\n")
                  )))

    (setq org-use-fast-todo-selection t)
    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  (done ("WAITING"))
                  ("TODO" ("WAITING") ("CANCELLED"))
                  ("NEXT" ("WAITING") ("CANCELLED"))
                  ("DONE" ("WAITING") ("CANCELLED")))))

    ;; (setq org-agenda-custom-commands
    ;;       (quote (("N" "Notes" tags "NOTE"
    ;;                (
    ;;                 (org-agenda-overriding-header "Notes")
    ;;                 (org-tags-match-list-sublevels t)
    ;;                 )
    ;;                )
    ;;              )
    ;;       )
    ;; )


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

    ;; circles instead of dashes in lists
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))















    ;; Do not dim blocked tasks
    (setq org-agenda-dim-blocked-tasks nil)

    ;; Compact the block agenda view
    (setq org-agenda-compact-blocks t)

    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          (quote (("N" "Notes" tags "NOTE"
                  ((org-agenda-overriding-header "Notes")
                    (org-tags-match-list-sublevels t)))
                  ("h" "Habits" tags-todo "STYLE=\"habit\""
                  ((org-agenda-overriding-header "Habits")
                    (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
                  (" " "Agenda"
                  ((agenda "" nil)
                    (tags "REFILE"
                          ((org-agenda-overriding-header "Tasks to Refile")
                          (org-tags-match-list-sublevels nil)))
                    (tags-todo "-CANCELLED/!"
                              ((org-agenda-overriding-header "Stuck Projects")
                                (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                (org-agenda-sorting-strategy
                                '(category-keep))))
                    (tags-todo "-HOLD-CANCELLED/!"
                              ((org-agenda-overriding-header "Projects")
                                (org-agenda-skip-function 'bh/skip-non-projects)
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy
                                '(category-keep))))
                    (tags-todo "-CANCELLED/!NEXT"
                              ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                      (if bh/hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                (org-tags-match-list-sublevels t)
                                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-sorting-strategy
                                '(todo-state-down effort-up category-keep))))
                    (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                              ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                      (if bh/hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-sorting-strategy
                                '(category-keep))))
                    (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                              ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                      (if bh/hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'bh/skip-project-tasks)
                                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-sorting-strategy
                                '(category-keep))))
                    (tags-todo "-CANCELLED+WAITING|HOLD/!"
                              ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                      (if bh/hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'bh/skip-non-tasks)
                                (org-tags-match-list-sublevels nil)
                                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                    (tags "-REFILE/"
                          ((org-agenda-overriding-header "Tasks to Archive")
                          (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                          (org-tags-match-list-sublevels nil))))
                  nil))))




  )
