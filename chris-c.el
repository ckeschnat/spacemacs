(setq c-default-style "linux" c-basic-offset 4)

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
(spacemacs/set-leader-keys-for-major-mode 'c++-mode "m" 'make-without-asking)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "A" 'org-archive-subtree-hierarchical)
