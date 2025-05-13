;;; org-find-file-tags.el --- select files based on org-mode tags  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides functionality to find org files based on their tags using ripgrep.
;; Requires ripgrep to be installed on the system.

;;; Code:

(require 'cl-lib)
(require 'project)

(defgroup org-find-file-tags nil
  "Find org files based on tags."
  :group 'org)

(defcustom org-find-files-ripgrep-executable "rg"
  "Path to ripgrep executable."
  :type 'string
  :group 'org-find-file-tags)

(defcustom org-find-files-cache-timeout 300
  "Number of seconds to cache tag results."
  :type 'integer
  :group 'org-find-file-tags)

(defvar org-find-files--tags-cache nil
  "Cache of tags and their timestamp.")

(defvar org-find-files-tag-line-re
  "^\\(?:\\*+ .*?:\\([[:alnum:]@#%:]+\\):\\|#\\+tags: +\\([[:alnum:]@#% ]+\\)\\)$"
  "Regex matching org tags in headlines or file metadata.")

(defun org-find-files--ensure-ripgrep ()
  "Ensure ripgrep is available."
  (unless (executable-find org-find-files-ripgrep-executable)
    (user-error "Ripgrep executable not found. Please install ripgrep")))

(defun org-grep-tags (project-dir)
  "Show grep buffer of org files with tagged headlines in PROJECT-DIR using ripgrep."
  (interactive 
   (list (read-directory-name 
          "Directory: " 
          (if (project-current)
              (project-root (project-current))
            default-directory))))
  (org-find-files--ensure-ripgrep)
  (let ((command `(,org-find-files-ripgrep-executable 
                  "--no-heading" "-e" ,org-find-files-tag-line-re
                  "--" ,project-dir)))
    (compilation-start (mapconcat #'shell-quote-argument command " ")
                      'grep-mode)))

(defun org-find-files--get-cached-tags (dir)
  "Get cached tags for DIR or nil if cache expired."
  (when-let ((cache-entry (assoc dir org-find-files--tags-cache)))
    (when (< (- (float-time) (nth 2 cache-entry))
             org-find-files-cache-timeout)
      (nth 1 cache-entry))))

(defun org-find-files--update-cache (dir tags)
  "Update cache for DIR with TAGS."
  (setq org-find-files--tags-cache
        (cons (list dir tags (float-time))
              (assoc-delete-all dir org-find-files--tags-cache)))
  tags)

(defun org-tags-in-current (&optional dir)
  "Return list of unique tags in directory tree starting at DIR."
  (org-find-files--ensure-ripgrep)
  (let* ((dir (or dir (file-truename 
                       (if (project-current)
                           (project-root (project-current))
                         default-directory))))
         (output (condition-case err
                    (process-lines org-find-files-ripgrep-executable 
                                 "--ignore-case" 
                                 "--no-heading"
                                 "--no-line-number"
                                 "--no-filename"
                                 "-e"
                                 org-find-files-tag-line-re
                                 dir)
                  (error
                   (message "Error running ripgrep: %s" err)
                   nil))))
    (when output
      (seq-uniq
       (cl-loop for line in output
                when (string-match org-find-files-tag-line-re line)
                append (split-string 
                       (or (match-string 1 line)
                           (match-string 2 line))
                       ":" t "[[:space:]]"))
       #'string-equal))))

(defun org-tags-in-current--parse-line (line)
  "Parse tags from LINE matching org-find-files-tag-line-re."
  (save-match-data
    (when (string-match org-find-files-tag-line-re line)
      (let ((tags (or (match-string 1 line) 
                     (match-string 2 line))))
        (when tags
          (split-string tags ":" t "[[:space:]]"))))))

(defun org-find-file-by-tags (file-tuple)
  "Open file from FILE-TUPLE (path . line)."
  (interactive 
   (list
    (let ((files (org-find-files-file-with-tag
                  (completing-read "Tag: " (org-tags-in-current) nil t))))
      (gethash (completing-read "File: " files) files))))
  (find-file (car file-tuple))
  (when (cdr file-tuple)
    (goto-char (point-min))
    (forward-line (1- (cdr file-tuple)))))

(defun org-find-files-file-with-tag (tag &optional dir)
  "Return hashtable of files containing TAG with line numbers in DIR."
  (org-find-files--ensure-ripgrep)
  (let* ((dir (or dir (file-truename 
                       (if (project-current)
                           (project-root (project-current))
                         default-directory))))
         (tag-regex (format "\\(?:#\\+tags:.*\\b%s\\b\\|:[[:alnum:]]+:\\b%s\\b:\\)"
                           (regexp-quote tag)
                           (regexp-quote tag)))
         (output (condition-case err
                    (process-lines org-find-files-ripgrep-executable 
                                 "--ignore-case" 
                                 "--line-number"
                                 "-e"
                                 tag-regex 
                                 dir)
                  (error
                   (message "Error running ripgrep: %s" err)
                   nil))))
    (when output
      (cl-loop with ht = (make-hash-table :test 'equal)
               for line in output
               for parts = (split-string line ":" t)
               when (>= (length parts) 2)
               do (let* ((file (car parts))
                        (linenum (string-to-number (cadr parts)))
                        (content (string-join (cddr parts) ":"))
                        (relpath (file-relative-name file dir))
                        (heading (when (string-match "\\*+ \\(.*?\\) *:" content)
                                 (match-string 1 content)))
                        (key (format "%s%s" relpath 
                                   (if heading (concat " :: " heading) ""))))
                    (puthash key (cons file linenum) ht))
               finally return ht))))

(defun org-find-files-debug ()
  "Run ripgrep with debug output to see what's happening."
  (interactive)
  (let* ((dir (file-truename 
               (if (project-current)
                   (project-root (project-current))
                 default-directory)))
         (command (format "%s --debug -e %s %s"
                         org-find-files-ripgrep-executable
                         (shell-quote-argument org-find-files-tag-line-re)
                         (shell-quote-argument dir))))
    (with-current-buffer (get-buffer-create "*org-find-files-debug*")
      (erase-buffer)
      (insert (shell-command-to-string command))
      (pop-to-buffer (current-buffer)))))

(provide 'org-find-file-tags)
;;; org-find-file-tags.el ends here
