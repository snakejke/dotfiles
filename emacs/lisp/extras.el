;;; extras.el --- small extra functions -*- lexical-binding: t; -*-

(defun my/convert-org-to-docx-with-pandoc ()
  "Use Pandoc to convert .org to .docx.
Comments:
- The `-N' flag numbers the headers lines.
- Use the `--from org' flag to have this function work on files
  that are in Org syntax but do not have a .org extension"
  (interactive)
  (message "exporting .org to .docx")
  (shell-command
   (concat "pandoc -N --from org " (buffer-file-name)
           " -o "
           (file-name-sans-extension (buffer-file-name))
           (format-time-string "-%Y-%m-%d-%H%M%S") ".docx")))

(defun sudo-edit (&optional arg)
  "Edit the current file or directory as root.
With a prefix ARG, prompt for a file to visit.
If in a dired buffer, open the current directory as root.
Otherwise, visit the current file with root privileges."
  (interactive "P")
  (let ((file (or buffer-file-name
                  (and (eq major-mode 'dired-mode) default-directory))))
    (if (or arg (not file))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file (as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" file)))))


(defun +unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))


;;; bold text with mouse 
(defvar org-mouse-bold-mode nil
  "Flag to enable/disable text highlighting in org mode.")

(define-advice mouse-set-region (:after (click) org-highlight ())
  (when (and org-mouse-bold-mode
             (derived-mode-p 'org-mode)
             (use-region-p))
    (let ((origin (buffer-substring (region-beginning) (region-end)))
          (emphasis-char "*"))
      (delete-region (region-beginning) (region-end))
      (insert emphasis-char origin emphasis-char))))

(defun org-mouse-bold-mode-enable ()
  "Enable text highlighting mode in org mode."
  (interactive)
  (setq org-mouse-bold-mode t)
  (message "Text highlighting mode in org mode is enabled."))

(defun org-mouse-bold-mode-disable ()
  "Disable text highlighting mode in org mode."
  (interactive)
  (setq org-mouse-bold-mode nil)
  (message "Text highlighting mode in org mode is disabled."))


(provide 'extras)

;;; extras.el ends here
