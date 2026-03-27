;;; ob-jshell.el --- org-babel backend for Java via JShell using Nim wrapper -*- lexical-binding: t; -*-

(defcustom ob-jshell-command "jrepl"
  "Путь к Nim обертке jrepl для jshell."
  :type 'string
  :group 'org-babel)

(defvar org-babel-default-header-args:jshell
  '((:results . "output")
    (:exports . "both"))
  "Заголовочные аргументы по умолчанию для jshell блоков.")

(defun org-babel-execute:jshell (body params)
  "Выполнить BODY как JShell код через Nim wrapper."
  (let* ((timeout (cdr (assq :timeout params)))
         (cmd-args (when timeout (list (format "--timeout=%s" timeout)))))
    (with-temp-buffer
      (insert body)
      (let ((exit-code (apply #'call-process-region
                              (point-min) (point-max)
                              ob-jshell-command
                              t t nil
                              cmd-args)))
        (let* ((raw-result (buffer-string))
               (cleaned (string-join
                         (seq-filter
                          (lambda (line)
                            (let ((trimmed (string-trim line)))
                              (and (not (string-empty-p trimmed))
                                   (not (string-equal trimmed "null"))
                                   (not (string-match-p "^jshell>\\s-*$" trimmed))
                                   (not (string-match-p "^\\.\\.\\.*>\\s-*$" trimmed))
                                   (not (string-match-p "^>\\.*\\s-*$" trimmed)))))
                          (split-string raw-result "\n"))
                         "\n")))
          (if (= exit-code 0)
              (string-trim cleaned)
            (error "jshell failed with exit code %d: %s" exit-code raw-result)))))))

(add-to-list 'org-src-lang-modes '("jshell" . java-ts))

(provide 'ob-jshell)
;;; ob-jshell.el ends here
