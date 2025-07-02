;; ;;; java-runner.el --- Simple Java code runner for Emacs -*- lexical-binding: t; -*-

;; ;; Author: Your Name
;; ;; Version: 1.0
;; ;; Keywords: java, tools

;; ;;; Commentary:
;; ;; Simple Java runner that works without LSP dependencies
;; ;; Supports three execution modes: batch, interactive, long-running

;; ;;; Code:

;; (require 'project)

;; ;;; Configuration

;; (defgroup java-runner nil
;;   "Java code execution in Emacs"
;;   :group 'programming
;;   :prefix "java-runner-")

;; (defcustom java-runner-java-program nil
;;   "Path to Java executable. If nil, will be auto-detected."
;;   :type '(choice (const :tag "Auto-detect" nil)
;;                  (string :tag "Path to java"))
;;   :group 'java-runner)

;; (defcustom java-runner-main-args nil
;;   "Default arguments for main method."
;;   :type '(repeat string)
;;   :group 'java-runner)

;; (defcustom java-runner-jvm-args nil
;;   "Default JVM arguments."
;;   :type '(repeat string)
;;   :group 'java-runner)

;; (defcustom java-runner-env nil
;;   "Environment variables for Java execution.
;; List of strings of the form ENVVARNAME=VALUE."
;;   :type '(repeat string)
;;   :group 'java-runner)

;; (defcustom java-runner-debug-jvm-arg
;;   "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=localhost:8000"
;;   "JVM argument to start debugger."
;;   :type 'string
;;   :group 'java-runner)

;; ;;; Core Functions

;; (defun java-runner--find-java-executable ()
;;   "Find Java executable with fallback strategy."
;;   (or java-runner-java-program
;;       (executable-find "java")
;;       (when-let ((java-home (getenv "JAVA_HOME")))
;;         (let ((java-path (expand-file-name "bin/java" java-home)))
;;           (when (file-executable-p java-path)
;;             java-path)))
;;       (user-error "Cannot find java executable! Set java-runner-java-program or JAVA_HOME")))

;; (defun java-runner--find-project-root ()
;;   "Find project root using multiple strategies."
;;   (or (when-let ((project (project-current)))
;;         (project-root project))
;;       (locate-dominating-file default-directory
;;                              (lambda (dir)
;;                                (or (file-exists-p (expand-file-name "pom.xml" dir))
;;                                    (file-exists-p (expand-file-name "build.gradle" dir))
;;                                    (file-exists-p (expand-file-name ".git" dir))
;;                                    (file-exists-p (expand-file-name ".project" dir)))))
;;       default-directory))

;; (defun java-runner--extract-package-name (java-file)
;;   "Extract package name from Java file."
;;   (with-temp-buffer
;;     (insert-file-contents java-file)
;;     (goto-char (point-min))
;;     (when (re-search-forward "^\\s-*package\\s-+\\([a-zA-Z_][a-zA-Z0-9_.]*\\)\\s-*;" nil t)
;;       (match-string 1))))

;; (defun java-runner--extract-class-name (java-file)
;;   "Extract public class name from Java file."
;;   (with-temp-buffer
;;     (insert-file-contents java-file)
;;     (goto-char (point-min))
;;     (when (re-search-forward "^\\s-*public\\s-+class\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
;;       (match-string 1))))

;; (defun java-runner--has-main-method (java-file)
;;   "Check if Java file contains a main method."
;;   (with-temp-buffer
;;     (insert-file-contents java-file)
;;     (goto-char (point-min))
;;     (re-search-forward "public\\s-+static\\s-+void\\s-+main\\s-*(" nil t)))

;; (defun java-runner--build-fqcn (java-file)
;;   "Build fully qualified class name from Java file."
;;   (let ((package-name (java-runner--extract-package-name java-file))
;;         (class-name (java-runner--extract-class-name java-file)))
;;     (unless class-name
;;       (user-error "Cannot find public class in %s" java-file))
;;     (if package-name
;;         (concat package-name "." class-name)
;;       class-name)))

;; (defun java-runner--build-classpath (project-root)
;;   "Build classpath for the project."
;;   (let ((paths (list "."
;;                     (expand-file-name "target/classes" project-root)
;;                     (expand-file-name "build/classes/java/main" project-root)
;;                     (expand-file-name "build/classes" project-root)
;;                     (expand-file-name "out/production" project-root))))
;;     (string-join (cl-remove-if-not #'file-directory-p paths) path-separator)))

;; (defun java-runner--get-current-java-file ()
;;   "Get current Java file or prompt for one."
;;   (cond
;;    ((and (buffer-file-name) (string-match "\\.java$" (buffer-file-name)))
;;     (buffer-file-name))
;;    (t
;;     (let ((java-file (read-file-name "Java file: " nil nil t)))
;;       (unless (string-match "\\.java$" java-file)
;;         (user-error "Selected file is not a Java file"))
;;       java-file))))

;; (defun java-runner--prompt-execution-type ()
;;   "Prompt user for execution type."
;;   (intern (completing-read "Execution type: "
;;                           '("batch" "interactive" "long-running")
;;                           nil t "batch")))

;; (defun java-runner--build-command (fqcn project-root &optional debug)
;;   "Build Java execution command."
;;   (let ((java-exe (java-runner--find-java-executable))
;;         (classpath (java-runner--build-classpath project-root))
;;         (jvm-args (if debug
;;                       (cons java-runner-debug-jvm-arg java-runner-jvm-args)
;;                     java-runner-jvm-args))
;;         (main-args java-runner-main-args))
;;     (mapconcat #'identity
;;                (cl-remove-if #'string-empty-p
;;                            (list java-exe
;;                                  (mapconcat #'identity jvm-args " ")
;;                                  "-cp" (shell-quote-argument classpath)
;;                                  fqcn
;;                                  (mapconcat #'identity main-args " ")))
;;                " ")))

;; ;;; Execution Strategies

;; (defun java-runner--run-batch (fqcn project-root &optional debug)
;;   "Run Java program in batch mode using compilation."
;;   (let ((default-directory project-root)
;;         (compilation-environment java-runner-env)
;;         (command (java-runner--build-command fqcn project-root debug)))
;;     (compile command)))

;; (defun java-runner--run-interactive (fqcn project-root &optional debug)
;;   "Run Java program with interactive I/O."
;;   (let* ((default-directory project-root)
;;          (command (java-runner--build-command fqcn project-root debug))
;;          (buffer-name "*Java Interactive*")
;;          (process-environment (append java-runner-env process-environment)))
;;     (when (get-buffer-process buffer-name)
;;       (when (yes-or-no-p "Java process already running. Kill it? ")
;;         (kill-process (get-buffer-process buffer-name))))
;;     (let ((process (start-process-shell-command "java-interactive" buffer-name command)))
;;       (switch-to-buffer-other-window buffer-name)
;;       (with-current-buffer buffer-name
;;         (setq-local process-connection-type t) ; Use pty for better I/O
;;         (message "Java interactive process started. Use C-c C-c to terminate")))))

;; (defun java-runner--run-long-running (fqcn project-root &optional debug)
;;   "Run long-running Java process."
;;   (let* ((default-directory project-root)
;;          (command (java-runner--build-command fqcn project-root debug))
;;          (buffer-name "*Java Long Running*")
;;          (process-environment (append java-runner-env process-environment)))
;;     (when (get-buffer-process buffer-name)
;;       (when (yes-or-no-p "Java process already running. Kill it? ")
;;         (kill-process (get-buffer-process buffer-name))))
;;     (let ((process (start-process-shell-command "java-long-running" buffer-name command)))
;;       (switch-to-buffer-other-window buffer-name)
;;       (with-current-buffer buffer-name
;;         (message "Java long-running process started. Use M-x java-runner-kill to terminate")))))

;; ;;; Interactive Commands

;; ;;;###autoload
;; (defun java-runner-run (&optional debug)
;;   "Run Java main method. With prefix argument, start in debug mode."
;;   (interactive "P")
;;   (let* ((java-file (java-runner--get-current-java-file))
;;          (project-root (java-runner--find-project-root))
;;          (fqcn (java-runner--build-fqcn java-file))
;;          (exec-type (java-runner--prompt-execution-type)))
    
;;     ;; Verify main method exists
;;     (unless (java-runner--has-main-method java-file)
;;       (user-error "No main method found in %s" java-file))
    
;;     ;; Save file if it's modified
;;     (when (and (buffer-modified-p) (buffer-file-name))
;;       (save-buffer))
    
;;     ;; Execute based on type
;;     (pcase exec-type
;;       ('batch (java-runner--run-batch fqcn project-root debug))
;;       ('interactive (java-runner--run-interactive fqcn project-root debug))
;;       ('long-running (java-runner--run-long-running fqcn project-root debug)))))

;; ;;;###autoload
;; (defun java-runner-run-batch (&optional debug)
;;   "Run Java main method in batch mode."
;;   (interactive "P")
;;   (let* ((java-file (java-runner--get-current-java-file))
;;          (project-root (java-runner--find-project-root))
;;          (fqcn (java-runner--build-fqcn java-file)))
;;     (unless (java-runner--has-main-method java-file)
;;       (user-error "No main method found in %s" java-file))
;;     (when (and (buffer-modified-p) (buffer-file-name))
;;       (save-buffer))
;;     (java-runner--run-batch fqcn project-root debug)))

;; ;;;###autoload
;; (defun java-runner-kill ()
;;   "Kill running Java processes."
;;   (interactive)
;;   (let ((killed 0))
;;     (dolist (buffer-name '("*Java Interactive*" "*Java Long Running*"))
;;       (when-let ((buffer (get-buffer buffer-name))
;;                  (process (get-buffer-process buffer)))
;;         (kill-process process)
;;         (cl-incf killed)))
;;     (if (> killed 0)
;;         (message "Killed %d Java process(es)" killed)
;;       (message "No running Java processes found"))))

;; ;;;###autoload
;; (defun java-runner-set-main-args ()
;;   "Set arguments for main method interactively."
;;   (interactive)
;;   (let ((args (split-string (read-string "Main method arguments: "
;;                                         (mapconcat #'identity java-runner-main-args " "))
;;                            "\\s-+" t)))
;;     (setq java-runner-main-args args)
;;     (message "Main args set to: %s" (mapconcat #'identity args " "))))

;; ;;;###autoload
;; (defun java-runner-set-jvm-args ()
;;   "Set JVM arguments interactively."
;;   (interactive)
;;   (let ((args (split-string (read-string "JVM arguments: "
;;                                         (mapconcat #'identity java-runner-jvm-args " "))
;;                            "\\s-+" t)))
;;     (setq java-runner-jvm-args args)
;;     (message "JVM args set to: %s" (mapconcat #'identity args " "))))

;; ;;; Minor mode for key bindings

;; (defvar java-runner-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-r") #'java-runner-run)
;;     (define-key map (kbd "C-c C-b") #'java-runner-run-batch)
;;     (define-key map (kbd "C-c C-k") #'java-runner-kill)
;;     map)
;;   "Keymap for java-runner-mode.")

;; ;;;###autoload
;; (define-minor-mode java-runner-mode
;;   "Minor mode for Java code execution."
;;   :lighter " JavaRun"
;;   :keymap java-runner-mode-map)

;; ;; ;;;###autoload
;; ;; (add-hook 'java-mode-hook #'java-runner-mode)

;; (provide 'java-runner)

;; ;;; java-runner.el ends here
