;; -*- lexical-binding: t; -*-

;; (setq use-package-verbose 'debug)


;; (setq use-package-compute-statistics t)

;; (add-hook 'elpaca-after-init-hook
;;           (lambda ()
;;             (message "Emacs loaded in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract (current-time) before-init-time)))
;;                      gcs-done)))

  (add-hook 'elpaca--post-queues-hook
            (lambda ()
              (message "Emacs fully loaded in %.2f seconds with %d GCs."
                       (float-time (time-subtract (current-time) before-init-time))
                       gcs-done)))


  ;; (defun my/report-startup-time ()
  ;;   (message "Emacs fully loaded in %.2f seconds with %d GCs."
  ;;            (float-time (time-subtract (current-time) before-init-time))
  ;;            gcs-done)
  ;;   (remove-hook 'elpaca--post-queues-hook #'my/report-startup-time))

  ;; (add-hook 'elpaca--post-queues-hook #'my/report-startup-time)


;; (add-hook 'after-load-functions
;;           (lambda (path)
;;             (when (or (string-match-p "url-parse" path)
;;                       (string-match-p "auth-source" path))
;;               (message "!!! ПОЙМАН !!! Загружается: %s" path)
;;               (debug)))) ; Вызывает честный Backtrace-буфер

;; (defvar my/require-log nil)

;; (defun my/wrap-require ()
;;   (advice-add 'require :around
;;     (lambda (orig feature &rest args)
;;       (let ((start (current-time)))
;;         (prog1 (apply orig feature args)
;;           (push (cons feature (time-subtract (current-time) start))
;;                 my/require-log))))
;;     '((name . my/require-timer))))

;; (my/wrap-require)

;; (profiler-start 'cpu+mem)
;; (add-hook 'elpaca-after-init-hook (lambda () (profiler-stop) (profiler-report)))

;; ELP полезен для поиска горячих функций
;; (require 'elp)
;; (with-eval-after-load file
;;   (elp-instrument-package file))
;; (add-hook 'elpaca-after-init-hook
;;           (lambda () (elp-results) (elp-restore-package (intern file))))

(setq initial-buffer-choice t) ;;*scratch*

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(setq elpaca-queue-limit 30)

;; We need this loaded for SSH protocol
;; (elpaca-queue
;;  (elpaca keychain-environment
;;    (require 'keychain-environment)
;;    (keychain-refresh-environment)))

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(elpaca elpaca-use-package
  (require 'elpaca-use-package)
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(elpaca-wait)

(if debug-on-error
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(let ((default-directory "~/.config/emacs/lisp"))
  (when (file-exists-p default-directory)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(defvar my-etc-directory (expand-file-name "etc/" user-emacs-directory))
(defvar my-var-directory (expand-file-name "var/" user-emacs-directory))

(make-directory my-etc-directory t)
(make-directory my-var-directory t)

(defun my-expand-etc-file (file)
  (let ((full-path (expand-file-name (convert-standard-filename file) my-etc-directory)))
    (make-directory (file-name-directory full-path) t)
    full-path))

(defun my-expand-var-file (file)
  (let ((full-path (expand-file-name (convert-standard-filename file) my-var-directory)))
    (make-directory (file-name-directory full-path) t)
    full-path))

(with-file-modes #o700
  (my-expand-etc-file "eshell/"))

(my-expand-var-file "lsp/")

;; (let ((additional-paths '("/usr/share/emacs/site-lisp/notmuch")))


;; (let ((additional-paths '("~/.local/state/nix/profile/share/emacs/site-lisp")))
;;   (mapc (lambda (path)
;;           (when (file-directory-p path)
;;             (add-to-list 'load-path path)))
;;         additional-paths))

;; (setq literate-file (concat user-emacs-directory "init.org"))
(setq literate-file (concat user-emacs-directory "init.el"))

(defun +terminal ()
  "Set the terimnal coding system."
  (unless (display-graphic-p)
    (set-terminal-coding-system 'utf-8)))

(add-hook 'server-after-make-frame-hook #'+terminal)

;; (load-file "~/Documents/emacs-secrets.el")

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)

(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator emacs)
 :prefix-map '+prefix-map
 :prefix "SPC"
 :global-prefix "S-SPC")

(general-create-definer global-definer
  :wk-full-keys nil
  :keymaps '+prefix-map)

(global-definer
  "SPC" '(project-find-file :wk "Find file in project")
  ;;"/"   'occur
  "!"   'shell-command
  ";"   'pp-eval-expression
  "`"   'evil-switch-to-windows-last-buffer
  "."   'repeat
  "h"   (general-simulate-key "C-h" :which-key "help")
  "z"   '((lambda (local) (interactive "p")
            (unless repeat-mode (repeat-mode))
            (let ((local current-prefix-arg)
                  (current-prefix-arg nil))
              (call-interactively (if local #'text-scale-adjust #'global-text-scale-adjust))))
          :which-key "zoom"))

(general-create-definer global-leader
  :keymaps 'override
  :states '(insert normal hybrid motion visual operator)
  :prefix "SPC m"
  :non-normal-prefix "S-SPC m"
  "" '( :ignore t
        :which-key
        (lambda (arg)
          (cons (cadr (split-string (car arg) " "))
                (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(defvar +leader-key "SPC")
(defvar +leader-alt-key "S-SPC")

(defmacro +general-global-menu! (name prefix-key &rest body)
  (declare (indent 2))
  (let* ((n (concat "+general-global-" name))
         (prefix-map (intern (concat n "-map"))))
    `(progn
       (general-create-definer ,(intern n)
         :wrapping global-definer
         :prefix-map (quote ,prefix-map)
         :prefix ,prefix-key
         :wk-full-keys nil
         "" '(:ignore t))
       (,(intern n) ,@body)
       (with-eval-after-load 'which-key
         (which-key-add-key-based-replacements
           (concat +leader-key " " ,prefix-key) ,name)
         (which-key-add-key-based-replacements
           (concat +leader-alt-key " " ,prefix-key) ,name)))))

(+general-global-menu! "application" "a"
  "p" '(:ignore t "elpaca")
  "pb" 'elpaca-browse
  "pr"  '((lambda () (interactive)
            (let ((current-prefix-arg (not current-prefix-arg))
                  (this-command 'elpaca-rebuild))
              (call-interactively #'elpaca-rebuild)))
          :wk "rebuild")
  "pm" 'elpaca-manager
  "pl" 'elpaca-log
  "pi" 'elpaca-info
  "pI" '((lambda () (interactive) (info "Elpaca"))
         :wk "elpaca-info")
  "ps" 'elpaca-status
  "pt" 'elpaca-try
  "pv" 'elpaca-visit)

(+general-global-menu! "open" "o"
  "-"  'dired-jump
  "p"  'treemacs
  "e"  'project-eshell-popup
  )

(+general-global-menu! "buffer" "b"
  "d"  'kill-current-buffer
  "o" '((lambda () (interactive) (switch-to-buffer nil))
        :wk "other-buffer")
  "p"  'previous-buffer
  "r"  'rename-buffer
  "R"  'revert-buffer
  "i"  'ibuffer
  "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
        :which-key "messages-buffer")
  "n"  'next-buffer
  "N"  'evil-buffer-new
  "s"  '("fdfd" . basic-save-buffer)
  "S"  '(evil-write-all :wk "Save all buffers")
  ;;"s"  'scratch-buffer
  "TAB" '((lambda () (interactive) (switch-to-buffer nil))
          :which-key "other-buffer"))

(+general-global-menu! "bookmark" "B")

(+general-global-menu! "eval" "e"
  "b" 'eval-buffer
  "d" 'eval-defun
  "e" 'eval-expression
  "p" 'pp-eval-last-sexp
  "s" 'eval-last-sexp)

(+general-global-menu! "file" "f"
  ;; "d"   '((lambda (&optional arg)
  ;;           (interactive "P")
  ;;           (let ((buffer (when arg (current-buffer))))
  ;;             (diff-buffer-with-file buffer))) :which-key "diff-with-file")
  "e"   '(:ignore t :which-key "edit")
  "p"  '((lambda () (interactive) (find-file-existing literate-file) (widen))
          :which-key "dotfile")
  "f"   '(find-file :which-key "find-file")
  "l"   '((lambda (&optional arg)
            (interactive "P")
            (call-interactively (if arg #'find-library-other-window #'find-library)))
          :which-key "+find-library")
  ;;"p"   'find-function-at-point
  "P"   'find-function
  "R"   'rename-file-and-buffer
  ;;"s"   'save-buffer
  "v"   'find-variable-at-point
  "V"   'find-variable)

(+general-global-menu! "frame" "F"
  "D" 'delete-other-frames
  "F" 'select-frame-by-name
  "O" 'other-frame-prefix
  "c" '(:ingore t :which-key "color")
  "cb" 'set-background-color
  "cc" 'set-cursor-color
  "cf" 'set-foreground-color
  "f" 'set-frame-font
  "m" 'make-frame-on-monitor
  "n" 'next-window-any-frame
  "o" 'other-frame
  "p" 'previous-window-any-frame
  "r" 'set-frame-name)

(+general-global-menu! "git/version-control" "g")

(+general-global-menu! "local" "l")

(+general-global-menu! "narrow" "n"
  "d" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(+general-global-menu! "project" "p"
   ;;"b" '(:ignore t :which-key "buffer")
  "p" 'project-switch-project
  "o" '+treemacs/toggle
  )

(+general-global-menu! "quit" "q"
  "s" 'save-buffers-kill-emacs ;; Total exit: saves everything and kills the Emacs(Emacsclient) process completely
  "q" 'save-buffers-kill-terminal ;; save all buffers and kill emacs. but emacsclient will keep buffer alive
  "r" 'restart-emacs
  "d" 'delete-frame
  "Q" 'kill-emacs)

(+general-global-menu! "search" "s"
  "b" 'consult-line
  "h" 'consult-outline
  "f" 'consult-outline-directory
  "w" 'my-consult-ripgrep-word
  "p" 'consult-ripgrep)

(+general-global-menu! "text" "x"
  "i" 'insert-char
  "I" (general-simulate-key "C-x 8" :which-key "iso"))

;;(+general-global-menu! "tab" "t")

(+general-global-menu! "toggle" "T"
  "d" '(:ignore t :which-key "debug")
  "de" 'toggle-debug-on-error
  "dq" 'toggle-debug-on-quit
  "s" '(:ignore t :which-key "spelling"))

(+general-global-menu! "window" "w"
  "?" 'split-window-vertically
  "=" 'balance-windows
  "/" 'split-window-horizontally
  "O" 'delete-other-windows
  "X" '((lambda () (interactive) (call-interactively #'other-window) (kill-buffer-and-window))
        :which-key "kill-other-buffer-and-window")
  "d" 'delete-window
  "h" 'windmove-left
  "j" 'windmove-down
  "<right>" 'evil-window-right
  "<left>" 'evil-window-left
  "<up>" 'evil-window-up
  "<down>" 'evil-window-down
  "k" 'windmove-up
  "l" 'windmove-right
  "o" 'other-window
  "t" 'window-toggle-side-windows
  "."  '(:ingore :which-key "resize")
  ".h" '((lambda () (interactive)
           (call-interactively (if (window-prev-sibling) #'enlarge-window-horizontally
                                 #'shrink-window-horizontally)))
         :which-key "divider left")
  ".l" '((lambda () (interactive)
           (call-interactively (if (window-next-sibling) #'enlarge-window-horizontally
                                 #'shrink-window-horizontally)))
         :which-key "divider right")
  ".j" '((lambda () (interactive)
           (call-interactively (if (window-next-sibling) #'enlarge-window #'shrink-window)))
         :which-key "divider up")
  ".k" '((lambda () (interactive)
           (call-interactively (if (window-prev-sibling) #'enlarge-window #'shrink-window)))
         :which-key "divider down")
  "x" 'kill-buffer-and-window))

(use-package evil
  ;; :defer t
  :demand t
  :preface (setq evil-want-keybinding nil) ;; бинды с evil-collection
  :custom
  (evil-mode-line-format nil)
  (evil-symbol-word-search t "search by symbol with * and #.")
  (evil-shift-width 2 "Same behavior for vim's '<' and '>' commands")
  (evil-want-C-i-jump nil) ;; fix табов с org src ??
  (evil-complete-all-buffers nil)
  (evil-want-integration t)
  (evil-search-module 'evil-search "use vim-like search instead of 'isearch")
  (evil-undo-system 'undo-redo)
  (evil-want-minibuffer nil) ;; x2 ESC в минибуфере.
  (evil-move-beyond-eol t) ;; с nil курсор цепляется при скролле изображений
  (evil-move-cursor-back nil) ;; не делать отступ назад при esc
  :config
  (+general-global-window
    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top
    "L" 'evil-window-move-far-right)
  (+general-global-menu! "quit" "q"
    ":" 'evil-command-window-ex
    "/" 'evil-command-window-search-forward
    "?" 'evil-command-window-search-backward)
  ;; https://superuser.com/questions/684540/evil-mode-evil-shift-left-loses-selection
  ;; Overload shifts so that they don't lose the selection
    (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
    (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
    (define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
    (define-key evil-visual-state-map (kbd "<backtab>") 'djoyner/evil-shift-left-visual)

  (defun djoyner/evil-shift-left-visual (beg end)
    (interactive "r")
    (evil-shift-left beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-shift-right-visual (beg end)
    (interactive "r")
    (evil-shift-right beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (evil-mode)
)

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)

  (defmacro my/evil-collection-override (collection-feature mode-map &rest bindings)
  "BINDINGS are (states key function) where states can be a list or single symbol."
  `(with-eval-after-load ',collection-feature
     (add-hook ',(intern (concat (symbol-name mode-map) "-hook"))
       (lambda ()
         ,@(mapcan (lambda (b)
             (let ((states (if (listp (nth 0 b)) (nth 0 b) (list (nth 0 b))))
                   (key    (nth 1 b))
                   (fn     (nth 2 b)))
               (mapcar (lambda (state)
                   `(evil-local-set-key ',state (kbd ,key) ,fn))
                 states)))
           bindings))
       90)))
  :init
  (setq evil-collection-setup-minibuffer nil) ;; связано с (evil-want-minibuffer nil)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-ement-want-auto-retro t))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-anzu
  :after (evil)
  :config
  (global-anzu-mode t)
  )

(use-package evil-nerd-commenter
  :after (evil)
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :init
  (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines)
)

(use-package evil-escape
:config
(evil-escape-mode))

(use-feature emacs
  :demand t
  :custom
  (scroll-conservatively 101 "Scroll just enough to bring text into view")
  (enable-recursive-minibuffers t "Allow minibuffer commands in minibuffer")
  (frame-title-format '(buffer-file-name "%f" ("%b"))
                      "Make frame title current file's name.")
  (frame-resize-pixelwise t) ;; fvwm3

  (find-library-include-other-files nil)
  (indent-tabs-mode nil "Use spaces, not tabs")
  (inhibit-startup-screen t)
  (history-delete-duplicates t "Don't clutter history")
  (pgtk-use-im-context-on-new-connection nil "Prevent GTK from stealing Shift + Space")
  (sentence-end-double-space nil "Double space sentence demarcation breaks sentence navigation in Evil")
  (tab-stop-list (number-sequence 2 120 2))
  (tab-width 2 "Shorter tab widths")
  (column-numbes-mode t);;
  (truncate-lines t) ;; убрать перенос " ↪ "
  (line-numbers-mode t) ;; строки и колоки(882,44)
  (column-number-mode t)
  (create-lockfiles nil) ;; TODO
  (use-short-answers t) ;; yes-no to y-n
  (resize-mini-windows nil)
  (completion-styles '(flex basic partial-completion emacs22))
  ;; c corfu. если не будет работать. положить в init
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  ;;
  (inhibit-compacting-font-caches t)
  ;; PERF: 6% CPU
  (bidi-paragraph-direction 'left-to-right)
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  ;; Emacs 31+
  (mode-line-collapse-minor-modes '(not flymake-mode flycheck-mode))
  (mode-line-collapse-minor-modes-to "  ")
  :init
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8
        terminal-coding-system 'utf-8
        keyboard-coding-system 'utf-8
        selection-coding-system 'utf-8
        buffer-file-coding-system 'utf-8
        prefer-coding-system 'utf-8)
  ;; don't want ESC as a modifier.
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; переделать
               )

(use-feature display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode)
  ;; (text-mode . display-line-numbers-mode)
  ;; :custom
  ;; (display-line-numbers-type 't)        ;; 't' (abs), 'relative' or 'visual' (visual-line-mode)
  ;; (display-line-numbers-width-start t)  ;; pre-compute width from total line count, avoids gutter jumping
  )

(use-feature which-key
  :demand t
  :config
  (which-key-mode)
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33))

;; (load-file "/home/snake/tree/java.el")

;;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.config/emacs/var/env
;; use this command to create the file  `printenv > $HOME/.config/emacs/var/env'
(defconst my-local-dir (concat user-emacs-directory "var/"))

(defconst my-env-file (concat my-local-dir "env"))

(defun my-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
        (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
        (save-excursion
          (insert "\n")
          (insert-file-contents file))
        (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
          (push (match-string 1) envvars)
          (push (buffer-substring
                 (match-beginning 1)
                 (1- (or (save-excursion
                           (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                             (line-beginning-position)))
                         (point-max))))
                environment)))
      (when environment
        (setq process-environment
              (append (nreverse environment) process-environment)
              exec-path
              (if (member "PATH" envvars)
                  (append (split-string (getenv "PATH") path-separator t)
                          (list exec-directory))
                exec-path)
              shell-file-name
              (if (member "SHELL" envvars)
                  (or (getenv "SHELL") shell-file-name)
                shell-file-name))
        envvars))))

(when (and (or (display-graphic-p)
               (daemonp))
           (file-exists-p my-env-file))
  (my-load-envvars-file my-env-file))
;;; Code to replace exec-path-from-shell

(use-feature elec-pair
  :defer t
  :hook (prog-mode . electric-pair-mode)
  :config
  (setq electric-pair-pairs
            (append electric-pair-pairs '(( "[^ ]<" . ">")))))

(use-feature eshell
  :commands (eshell eshell-command)
  :custom
  (eshell-eshell-directory-name (my-expand-var-file "eshell/"))
  (eshell-banner-message "")
  :config
  (setq eshell-modules-list
        (append eshell-modules-list
                '(eshell-smart eshell-elecslash eshell-tramp)))

  (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
        eshell-prompt-function
        (lambda nil
          (concat
	         "[" (user-login-name) "@" (system-name) " "
	         (if (string= (eshell/pwd) (getenv "HOME"))
	             "~" (eshell/basename (eshell/pwd)))
	         "]"
	         (if (= (user-uid) 0) "# " "$ "))))

  ;;TODO
  (defun project-eshell-popup (&optional arg)
          "Start Eshell in a pop-up window in the current project's root directory.
      If a buffer already exists for running Eshell in the project's root,
      switch to it. Otherwise, create a new Eshell buffer.
      With \\[universal-argument] prefix arg, create a new Eshell buffer even
      if one already exists."
      (interactive "P")
      (let* ((default-directory (project-root (project-current t)))
           (eshell-buffer-name (project-prefixed-buffer-name "eshell"))
           (eshell-buffer (get-buffer eshell-buffer-name))
           (display-buffer-alist
            '(("\\*eshell.*\\*"
               (display-buffer-in-side-window)
               (window-height . 0.3)
               (side . bottom)
               (slot . 0)))))
      (if (and eshell-buffer (not arg))
          (pop-to-buffer eshell-buffer)
        (let ((buf (generate-new-buffer eshell-buffer-name)))
          (pop-to-buffer buf)
          (unless (derived-mode-p 'eshell-mode)
            (eshell-mode))
          buf))))
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  ;;
  )

;; (use-package java-runner
;;   ;; :ensure nil
;;   :load-path "~/.config/emacs/java-runner/")

(use-package modus-themes
  :ensure (:wait t)

  :demand t
  ;; :ensure (modus-themes :ref "4.8.1")
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;;
  (setq modus-themes-custom-auto-reload nil
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-italic-constructs t
      ;; modus-themes-prompts '(bold intense) ;; This variable is obsolete since 5.3.0
      ;; modus-themes-completions '((t . (extrabold))) ;; This variable is obsolete since 5.3.0
      modus-themes-headings
      '((0 . (variable-pitch 1))
        (t . (variable-pitch 1))

    ))
  

(setq modus-themes-after-load-theme-hook
      '(lambda ()
         (custom-set-faces
           `(vertico-current ((t :weight extra-bold :background ,(modus-themes-get-color-value 'bg-completion))))
           `(completions-highlight ((t :inherit bold :background ,(modus-themes-get-color-value 'bg-completion))))
           `(corfu-current ((t :inherit bold :background ,(modus-themes-get-color-value 'bg-completion))))
           `(icomplete-selected-match ((t :inherit bold :background ,(modus-themes-get-color-value 'bg-completion))))
           `(ivy-current-match ((t :inherit bold :background ,(modus-themes-get-color-value 'bg-completion))))
           `(company-tooltip-selection ((t :inherit bold :background ,(modus-themes-get-color-value 'bg-completion)))))))
  
    ;;fonts
    (set-face-attribute 'default nil :height 190)
    (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif" :height 1.0 :weight 'medium)
    (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))

    (setq modus-themes-common-palette-overrides
          '(
          (fringe unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          ))

    ;;Dark
    (setq modus-vivendi-palette-overrides
        '((bg-main  "#1e1f22");;idea
          (fg-main "#bcbec4")
          (constant "#E8BA36")
          ;; (constant "#bcbec4")
          (fnname "#57aaf7")
          (keyword "#fa8072") ;; light salmon
          (string "#6AAB73")
          (type "#BCBEC4")
          (variable "#bcbec4")
          ;;rainbow-delimiters
          (rainbow-0 "#E8BA36")
          (rainbow-1 "#54A857")
          (rainbow-2 "#359FF4")
          (rainbow-3 "#6E7ED9")
          (rainbow-4 "#179387")
          (rainbow-5 "#A5BE00")
          (rainbow-6 "#005FA3")
          (rainbow-7 "#DB7100")
          (rainbow-8 "#FFC666")
          (rainbow-9 "#38FF91")
          ))
    ;; TODO Light
    (setq modus-operandi-palette-overrides
        '(
          (bg-main  "#f2f3f4")
          ;;(fg-main "#bcbec4")
          ))
;;TODO
(defun my-modus-themes-invisible-dividers (&rest _)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg :foreground ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'my-modus-themes-invisible-dividers)
(modus-themes-load-theme 'modus-vivendi))

;; Adapted from: rougier/nano-emacs
(defun +what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

;; *New user option 'treesit-enabled-modes'.*
;; [[https://github.com/emacs-mirror/emacs/blob/07adb8b59dee772b56612b90acd19e1a5a456628/etc/NEWS#L769][emacs/etc/NEWS at 07adb8b59dee772b56612b90acd19e1a5a456628 · emacs-mirror/ema...]]

(use-feature treesit
  :custom
  (treesit-font-lock-level 2)
  ;; (treesit-enabled-modes t)
  :config
;;   ;; https://www.reddit.com/r/emacs/comments/1j53j8v/comment/mge63rp/
  (add-to-list 'treesit-language-source-alist
             '(typst "https://github.com/uben0/tree-sitter-typst"))

  (defconst treesit-langs '(("c" . c)
                            ("c++" . cpp)
                            ("typst" . typst)
                            ("python" . python)
                            ("java" . java)
                            ("yaml" . yaml)))

(defun treesit-populate-mode-mapping ()
  "Populate `major-mode-remap-alist' according to `treesit-langs'."
  (interactive)
  (when (and (fboundp #'treesit-available-p) (treesit-available-p))
    (dolist (lang treesit-langs)
      (when-let* (((treesit-ready-p (cdr lang) t))
                 (mode (intern (concat (car lang) "-mode")))
                 (ts-mode (intern (concat (car lang) "-ts-mode"))))
        (add-to-list 'major-mode-remap-alist (cons mode ts-mode))))))

(defun treesit-install-language-grammars ()
  "Install tree-sitter grammars for languages in `treesit-langs'."
  (interactive)
  (dolist (lang treesit-langs)1
    (unless (treesit-ready-p (cdr lang) t)
      (treesit-install-language-grammar (cdr lang) 'interactive)))
  (treesit-populate-mode-mapping))

(treesit-populate-mode-mapping)
  )

(use-feature dired
  :commands dired-jump ;; или просто dired ?
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  ;; (dired-omit-files "\\(?:\\.+[^z-a]*\\)")
  ;; (dired-clean-up-buffers-too nil) х3 зачет nil
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-hide-details-hide-information-lines nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-omit-mode t nil)
  (dired-omit-verbose nil)
  (dired-guess-shell-alist-user
      '(("\\.py\\'" "python3")))
  ;; :hook (dired-mode-hook . dired-hide-details-mode)
  :config
  ;;(setq dired-omit-files (rx (seq bol ".")))
  (let ((args (list "-ahlv" "--group-directories-first")))
    (when (featurep :system 'bsd)
      (if-let* (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " ")))


  (defhydra hydra-dired (:color amaranth :hint nil)
    "
        ^Marking^      ^File Operations^     ^Actions^          ^Navigation^
        ^-------^      ^---------------^     ^-------^          ^----------^
        _m_ark        _c_opy                _d_elete           _n_ext line
        _u_nmark      _r_ename              _e_dit file name   _p_rev line
        _U_nmark all  _s_ymlink             _v_iew file        _j_ump to
        _t_toggle     _M_kdir               _x_ecute marked    _q_uit
        ^       ^      ^               ^    _w_ord (name)      ^          ^
        ^       ^      ^               ^    _W_ord (full path) ^          ^
   "
    ("m" dired-mark)
    ("u" dired-unmark)
    ("U" dired-unmark-all-marks)
    ("t" dired-toggle-marks)
    ("c" dired-do-copy)
    ("r" dired-do-rename)
    ("s" dired-do-symlink)
    ("M" dired-create-directory)
    ("d" dired-flag-file-deletion)
    ("e" dired-toggle-read-only)
    ("v" dired-view-file)
    ("x" dired-do-flagged-delete)
    ("n" dired-next-line)
    ("p" dired-previous-line)
    ("j" dired-goto-file)
    ("w" dired-copy-filename-as-kill)
    ("W" (lambda () 
           (interactive) 
           (let ((current-prefix-arg 0)) 
             (call-interactively #'dired-copy-filename-as-kill))))
    ("q" nil "quit" :color blue)))

;; Цвета темы для dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-rsync
  :disabled t
  :defer t
  :general (dired-mode-map "C-c C-r" #'dired-rsync))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode)
)

(use-feature dired-aux
  :defer t)

(use-feature dired-x
 :after dired
 :hook (dired-mode . dired-omit-mode))

(use-package docker
  :defer t)

(use-package docker-compose-mode
  :defer t
  :mode "docker-compose.*\.yml\\'")

(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-feature simple
  ;; :hook ((prog-mode text-mode conf-mode) . delete-trailing-whitespace-mode)
  :general
  (+general-global-toggle
    "f" 'auto-fill-mode)
  :custom
  (mail-user-agent 'notmuch-user-agent)
  (eval-expression-debug-on-error nil)
  (save-interprogram-paste-before-kill t)
  (fill-column 80 "Wrap at 80 columns."))

(use-package benchmark-init
  :disabled t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ;; ("M-`"   . popper-cycle)
         ("M-`"   . popper-kill-latest-popup)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
          "\\*compilation\\*"
          "\\*Warnings\\*"
          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "\\*erlang\\*"
          "\\*cargo-rustfix\\*"
          "\\*cargo-run\\*"
          "\\*rustic-compilation\\*"

          helpful-mode
          help-mode
          flymake-diagnostics-buffer-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; (use-package popper
;;   :defines popper-echo-dispatch-actions
;;   :commands popper-group-by-directory
;;   ;; :bind (:map popper-mode-map
;;   ;;             ("s-`" . popper-toggle)
;;   ;;             ("s-o"   . popper-cycle)
;;   ;;             ("q" . popper-kill-latest-popup)
;;   ;;             ("M-`" . popper-toggle-type))
;;   :hook (emacs-startup . popper-mode)
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$" "\\*Pp Eval Output\\*$"
;;           "\\*Compile-Log\\*"
;;           "\\*Completions\\*"
;;           "\\*Warnings\\*"
;;           "\\*Flymake diagnostics.*\\*"
;;           "\\*Async Shell Command\\*"
;;           "\\*Apropos\\*"
;;           "\\*Backtrace\\*"
;;           "\\*prodigy\\*"
;;           "\\*Calendar\\*"
;;           "\\*Embark Actions\\*"
;;           "\\*Finder\\*"
;;           "\\*Kill Ring\\*"
;;           "\\*Embark Export:.*\\*"
;;           "\\*Edit Annotation.*\\*"
;;           "\\*Flutter\\*"
;;           bookmark-bmenu-mode
;;           lsp-bridge-ref-mode
;;           comint-mode
;;           compilation-mode
;;           help-mode helpful-mode
;;           tabulated-list-mode
;;           Buffer-menu-mode
;;           occur-mode
;;           gnus-article-mode devdocs-mode
;;           grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
;;           ivy-occur-mode ivy-occur-grep-mode
;;           process-menu-mode list-environment-mode cargo-process-mode
;;           youdao-dictionary-mode osx-dictionary-mode fanyi-mode

;;           "^\\*eshell.*\\*.*$" eshell-mode
;;           "^\\*shell.*\\*.*$"  shell-mode
;;           "^\\*terminal.*\\*.*$" term-mode
;;           "^\\*vterm.*\\*.*$"  vterm-mode

;;           "\\*DAP Templates\\*$" dap-server-log-mode
;;           "\\*ELP Profiling Restuls\\*" profiler-report-mode
;;           "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
;;           "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
;;           "\\*[Wo]*Man.*\\*$"
;;           "\\*ert\\*$" overseer-buffer-mode
;;           "\\*gud-debug\\*$"
;;           "\\*lsp-help\\*$" "\\*lsp session\\*$"
;;           "\\*quickrun\\*$"
;;           "\\*tldr\\*$"
;;           "\\*vc-.*\\*$"
;;           "\\*eldoc\\*"
;;           "^\\*elfeed-entry\\*$"
;;           "^\\*macro expansion\\**"

;;           "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
;;           "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
;;           "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
;;           "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
;;           "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
;;           rustic-cargo-outdated-mode rustic-cargo-test-moed))


;;   (setq popper-echo-dispatch-actions t)
;;   (setq popper-group-function nil)
;;   :config
;;   (popper-echo-mode 1)

;;   (with-no-warnings
;;     (defun my-popper-fit-window-height (win)
;;       "Determine the height of popup window WIN by fitting it to the buffer's content."
;;       (fit-window-to-buffer
;;        win
;;        (floor (frame-height) 3)
;;        (floor (frame-height) 3)))
;;     (setq popper-window-height #'my-popper-fit-window-height)

;;     (defun popper-close-window-hack (&rest _)
;;       "Close popper window via `C-g'."
;;       ;; `C-g' can deactivate region
;;       (when (and (called-interactively-p 'interactive)
;;                  (not (region-active-p))
;;                  popper-open-popup-alist)
;;         (let ((window (caar popper-open-popup-alist)))
;;           (when (window-live-p window)
;;             (delete-window window)))))
;;     (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(use-feature autorevert
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t) ;; dired
  (auto-revert-interval 2 "Instantaneously revert")
  :config
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode))

(use-package anki-editor
  :ensure (anki-editor :host github :repo "orgtre/anki-editor")
  :after (org)
  :commands anki-editor-mode
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :custom (anki-editor-latex-style 'mathjax)
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number))

(use-package ankiorg
  :ensure (ankiorg :host github :repo "orgtre/ankiorg")
  :defer t
  :commands
  ankiorg-pull-notes
  ankiorg-buffer-get-media-files
  ankiorg-pull-tags
  :custom
  (ankiorg-sql-database
   "/home/snake/.local/share/Anki2/snake/collection.anki2")
  (ankiorg-media-directory
   "/home/snake/.local/share/Anki2/snake/collection.media/"))

(use-package smart-backspace
  :bind ("<C-M-backspace>" . smart-backspace))

;; (defun smart-backspace (n &optional killflag)
;;   "This function provides intellij like backspace.
;; Delete the backword-char usually and delete whitespace
;; to previous line indentation if it's start of line.
;; If a prefix argument is giben, delete the following N characters.
;; Optianal second arg KILLFLAG non-nil means to kill (save in killring)
;; instead of delete. Interactively, N is the prefix arg, and KILLFLAG
;; is set if N was explicitly specified."
;;   (interactive "p\nP")
;;   (let* ((current (point))
;;          (beginning (save-excursion
;;                       (beginning-of-line)
;;                       (point))))
;;     (if (string-match "^[ \t]*$" (buffer-substring beginning current))
;;         (progn
;;           (kill-line 0)
;;           (delete-char (- n) killflag)
;;           (indent-according-to-mode))
;;       (delete-char (- n) killflag))))
;; (define-key evil-insert-state-map [?\C-?] 'smart-backspace)

(use-package sqlite3
  :ensure (sqlite3 :host github :repo "pekingduck/emacs-sqlite3-api")
  :defer t)

(use-package leetcode
  :ensure (leetcode :host github :repo "kaiwk/leetcode.el" :files ("leetcode.el"))
  :defer t
  :custom
  (leetcode-save-solutions t)
  (leetcode-directory "/tmp/")
  (leetcode-prefer-language "java"))

;; [[https://github.com/yqrashawn/yqdotfiles/blob/1634092b80933ecd94018074847e2aaf35279d69/.doom.d/visual.el#L45][yqrashawn]] фикс таймера
(use-package elcord
  :defer 3
  :config
  (setq elcord-use-major-mode-as-main-icon t)
  (setq elcord-display-buffer-details nil)
  (setq elcord-idle-message "Thinking 🤔")
  (setq elcord-quiet t)

  (defun my/discord-running-p ()
    "Возвращает t если процесс Discord запущен."
    (eq 0 (call-process "pgrep" nil nil nil "-ix" "Discord")))

  (defun my/elcord-sync ()
    "Включает/выключает elcord в зависимости от того, запущен ли Discord."
    (if (my/discord-running-p)
        (unless elcord-mode (elcord-mode 1))
      (when elcord-mode (elcord-mode -1))))

  (my/elcord-sync)
  (run-with-timer 30 30 #'my/elcord-sync))

(use-feature multisession
  :custom
  (multisession-directory (my-expand-var-file "multisession/")))

(use-feature nsm
  :custom
  (nsm-settings-file (my-expand-var-file "network-security.eld")))

(use-package telega
  :commands (telega)
  :defer t)

(use-feature bookmark
  :custom
  (bookmark-default-file (my-expand-var-file "bookmarks.eld"))
  (bookmark-fontify nil)
  :general
  (+general-global-bookmark
    "j" 'bookmark-jump
    "s" 'bookmark-set
    "r" 'bookmark-rename))

;; Buttercup is a behavior-driven development framework for testing Emacs Lisp code.

;; https://github.com/jorgenschaefer/emacs-buttercup

(use-package buttercup
  :commands (buttercup-run-at-point))

(use-feature compile
  :commands (compile recompile)
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about-save nil);;autosave + compile
  :config

  (defun +compilation-colorize ()
    "Colorize from `compilation-filter-start' to `point'."
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'+compilation-colorize))

(use-feature ediff
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Бекапы и Автосейвы
(use-feature files
  :config
  ;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file." name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists." new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

  (add-to-list 'auto-mode-alist '("/aliases\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.scm?\\'" . racket-mode))
  (add-to-list 'auto-mode-alist '("\\.bb?\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

  :custom
  (auto-save-visited-mode 1)
  (auto-save-list-file-prefix nil) ; not create directory emacs/auto-save-list
  (auto-save-default nil) ;; disable auto save files
  (make-backup-files) ;; TODO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require-final-newline t "Automatically add newline at end of file")
  (backup-by-copying t)
  (backup-directory-alist `((".*" . ,(expand-file-name
                                      (concat user-emacs-directory "backups"))))
                          "Keep backups in their own directory")
  (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosaves/") t)))
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (version-control t)
  (safe-local-variable-values
   '((eval load-file "./init-dev.el")
     (org-clean-refile-inherit-tags))
   "Store safe local variables here instead of in emacs-custom.el")
  :init
  (add-function :after after-focus-change-function (lambda () (save-some-buffers t)))
  ;; (setq auto-save-visited-predicate
  ;;       (lambda ()
  ;;         (not (and (boundp 'major-mode)
  ;;                  (stringp (symbol-name major-mode))
  ;;                  (string-match-p "^notmuch-" (symbol-name major-mode))))))
  )

(use-feature saveplace
  :custom
  (save-place-file (my-expand-var-file "save-place.el"))
  :config
  (save-place-mode 1))

(use-package undo-fu
  :defer t)

(use-package undo-fu-session
  :defer t
  ;; :hook (elpaca-after-init . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory (my-expand-var-file "undo-fu-session/"))
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode t)
  )

(use-package vundo
  :bind (("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-roll-back-on-quit nil))

;; (use-feature savehist
;;   ;; :demand t
;;   ;; :defer t
;;   ;; :init
;;   ;; (setq savehist-file (my-expand-var-file "savehist"))

;;   :config
;;   ;; :init

;;   (savehist-mode t))

(use-feature savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; [[https://gist.github.com/clemera/8f6bdeffaf3495c98a070e50dc65acbc][mode-line+ · GitHub]]
(defface evil-state-face
  '((t (:weight bold)))
  "Bold face for evil states")

(defface evil-normal-face
  '((t (:inherit evil-state-face
        :background "#ff5f5f"
        :foreground "white")))
  "Face for evil normal state")

(defface evil-emacs-face
  '((t (:inherit evil-state-face
        :background "#3366ff"
        :foreground "white")))
  "Face for evil emacs state")

(defface evil-insert-face
  '((t (:inherit evil-state-face
        :background "#3399ff"
        :foreground "white")))
  "Face for evil insert state")

(defface evil-replace-face
  '((t (:inherit evil-state-face
        :background "#33ff99"
        :foreground "black")))
  "Face for evil replace state")

(defface evil-operator-face
  '((t (:inherit evil-state-face
        :background "pink"
        :foreground "black")))
  "Face for evil operator state")

(defface evil-motion-face
  '((t (:inherit evil-state-face
        :background "purple"
        :foreground "white")))
  "Face for evil motion state")

(defface evil-visual-face
  '((t (:inherit (region evil-state-face))))
  "Face for evil visual state")

(setq-default my-evil-modeline-string
              (propertize " NORMAL " 'face 'evil-normal-face))

(defun my-evil-update-modeline ()
  "Обновить строку состояния evil."
  (when (bound-and-true-p evil-state)
    (let* ((state-name (symbol-name evil-state))
           (face-name (intern (format "evil-%s-face" state-name))))
      (setq my-evil-modeline-string
            (propertize (format " %s " (upcase state-name))
                        'face face-name))
      (force-mode-line-update))))

(dolist (hook '(evil-normal-state-entry-hook
                evil-insert-state-entry-hook
                evil-visual-state-entry-hook
                evil-replace-state-entry-hook
                evil-operator-state-entry-hook
                evil-motion-state-entry-hook
                evil-emacs-state-entry-hook))
  (add-hook hook #'my-evil-update-modeline))

(setq-default mode-line-format
              '((:eval my-evil-modeline-string)  ; ← Обёрнут в (:eval ...)
                "%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))
(setq mode-line-modes-delimiters nil)

(my-evil-update-modeline)

(defun benchmark-modeline ()
  "Измерить время обновления modeline."
  (interactive)
  (let ((start-time (current-time))
        (iterations 1000))
    (dotimes (i iterations)
      (format-mode-line mode-line-format))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Modeline updated %d times in %.3f seconds (%.4f per update)"
               iterations elapsed (/ elapsed iterations)))))

(use-package reverse-im
  :defer 5
  ;;:after (general evil)
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))

(use-package logview
  :defer t
  ;:custom
  ;; (logview-views-file (concat minemacs-local-dir "logview-views.el"))
  ;; (logview-cache-filename (concat minemacs-cache-dir "logview-cache.el")))
  )

(use-package vertico
  :demand t
  :custom (vertico-cycle t)
  :config
  (setf (car vertico-multiline) "\n") ;; don't replace newlines
  (vertico-mode)
  (define-key vertico-map (kbd "C-h") #'+minibuffer-up-dir)
)

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
    '((file (styles basic partial-completion))
      (command (styles orderless flex))
      (symbol (styles orderless basic))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-affix-dispatch-alist
    '((?! . orderless-without-literal)
      (?% . char-fold-to-regexp)
      (?` . orderless-initialism)
      (?= . orderless-literal)
      (?^ . orderless-literal-prefix)
      (?~ . orderless-flex)))
  (orderless-style-dispatchers '(orderless-affix-dispatch))
  :config
  (add-hook 'prog-mode-hook
            (defun +prog-set-orderless-styles ()
              (setq-local orderless-matching-styles '(orderless-prefixes))))

  )

(use-package marginalia
  :defer 2
  :config (marginalia-mode))

(use-package consult
  :demand t
  :config

  ;; Альтернатива #foo -w  '\bword\b'
  (defun my-consult-ripgrep-word ()
    "Поиск целого слова через consult-ripgrep."
    (interactive)
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " -w")))
      (consult-ripgrep)))

  ;; Поиск по всем заголовка в директории
  (defun consult--outline-directory-candidates (files)
    (let ((candidates nil))
      (dolist (file files)
        (when (file-readable-p file)
          (condition-case nil
              (let ((file-candidates
                     (with-current-buffer (find-file-noselect file)
                       (consult--outline-candidates))))
                (dolist (cand file-candidates)
                  (push (propertize
                         (concat cand
                                 (propertize (format "  [%s]" (file-name-nondirectory file))
                                             'face 'consult-file))
                         'consult--outline-level (get-text-property 0 'consult--outline-level cand)
                         'consult-location (get-text-property 0 'consult-location cand))
                        candidates)))
            (error nil))))
      (nreverse candidates)))

  (defun consult-outline-directory (&optional dir level)
    "Jump to an outline heading across all Org files in DIR."
    (interactive
     (list nil (and current-prefix-arg
                    (prefix-numeric-value current-prefix-arg))))
    (let* ((dir (or dir default-directory))
           (files (directory-files dir t "\\.org\\'"))
           (candidates (consult--slow-operation
                           "Collecting headings..."
                         (consult--outline-directory-candidates files)))
           (min-level (if candidates
                          (- (cl-loop for cand in candidates minimize
                                      (get-text-property 0 'consult--outline-level cand))
                             ?1)
                        0))
           (narrow-pred (lambda (cand)
                          (<= (get-text-property 0 'consult--outline-level cand)
                              (+ consult--narrow min-level))))
           (narrow-keys (mapcar (lambda (c) (cons c (format "Level %c" c)))
                                (number-sequence ?1 ?9)))
           (narrow-init (and level (max ?1 (min ?9 (+ level ?0))))))
      (unless candidates
        (user-error "No headings found in %s" dir))
      (consult--read
       candidates
       :prompt (format "Go to heading [%s]: " (abbreviate-file-name dir))
       :annotate (consult--line-fontify)
       :category 'consult-location
       :sort nil
       :require-match t
       :lookup #'consult--line-match
       :initial-narrow narrow-init
       :narrow (list :predicate narrow-pred :keys narrow-keys)
       :history '(:input consult--line-history)
       :add-history (thing-at-point 'symbol)
       :state (consult--location-state candidates))))




  (consult-customize
   consult-recent-file
   consult-source-recent-file
   consult-source-buffer
   :preview-key nil)
  (define-key evil-normal-state-map (kbd "gb") 'consult-buffer)
  (define-key evil-visual-state-map (kbd "gb") 'consult-buffer)

  :general
  (+general-global-buffer
    "b" 'consult-buffer)
  :init

  (setq
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref)

  )

(use-package consult-dir
  :bind (("M-g d"   . consult-dir)
         :map minibuffer-local-completion-map
         ("M-s f" . consult-dir-jump-file)
         ("M-g d" . consult-dir))
  )

(use-feature consult-dir-vertico
  :no-require t
  :after (consult-dir vertico)
  :defines (vertico-map)
  :bind (:map vertico-map
              ("C-x C-j" . consult-dir)
              ("M-g d"   . consult-dir)
              ("M-s f"   . consult-dir-jump-file))
  )

(use-package consult-eglot
  :defer t)

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode))

(use-package embark
  :after (vertico)
  :general
  (general-nmap "C-l" 'embark-act))

(use-package embark-consult
  :after (embark consult))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

(use-feature dabbrev
  :custom
  (dabbrev-case-fold-search nil)             ; Регистрозависимый поиск
  (dabbrev-case-replace nil)                 ; Сохранять регистр найденного
  (dabbrev-abbrev-char-regexp "\\sw\\|\\s_") ; Символы = буквы + _
  (dabbrev-backward-only nil)                ; Искать везде
  (dabbrev-check-other-buffers nil))         ; Только текущий буфер
  ;; (setq dabbrev-check-other-buffers t)
  ;; (setq dabbrev-check-all-buffers nil)  ; Но не все, только "дружественные"

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :defer 5 ;; was 5
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1) ;; 0.2 def
  (corfu-auto-prefix 2)
  ;;(corfu-seperator ?-)
  (corfu-seperator ?\s)
  :config
  (global-corfu-mode)
  ;;
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point))))
  )

;; Замена company-quickhelp. corfu-popupinfo это бывший corfu-doc
(use-feature corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popinfo-delay '(0.5 . 1.0)))

(use-package cape
  ;; :disabled t
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-capf-super cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :custom
  (cape-dabbrev-buffer-function 'current-buffer)
  :init

  (defun my/cape--keyword-list ()
    "Get keyword strings for current major-mode, resolving cape-keyword-list aliases."
    (let ((entry (assq major-mode cape-keyword-list)))
      (when entry
        (let ((val (cdr entry)))
          (if (and (consp val) (symbolp (car val)))
              (cdr (assq (car val) cape-keyword-list))
            val)))))

  (defun my/cape-dabbrev-no-keyword ()
    "cape-dabbrev excluding candidates already present in cape-keyword for current mode."
    (when-let ((res (cape-dabbrev)))
      (let* ((kws (my/cape--keyword-list))
             (orig (nth 2 res)))
        (if (null kws) res
          (append (list (car res) (cadr res)
                        (lambda (str pred action)
                          (let ((cands (complete-with-action action orig str pred)))
                            (if (eq action t)
                                (seq-remove (lambda (c) (member c kws)) cands)
                              cands))))
                  (nthcdr 3 res))))))

  ;; prog-mode: file paths (high) + dabbrev (low)
  (add-hook 'prog-mode-hook
    (defun +cape-add-file-h ()
      (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (add-hook 'prog-mode-hook
    (defun +cape-add-super-h ()
      (add-hook 'completion-at-point-functions
                ;; (cape-capf-super #'cape-keyword #'cape-dabbrev) 20 t)))
                (cape-capf-super #'cape-keyword #'my/cape-dabbrev-no-keyword) 20 t)))
  ;; text-mode: dabbrev only
  (add-hook 'text-mode-hook
    (defun +cape-add-dabbrev-text-h ()
      (add-hook 'completion-at-point-functions #'cape-dabbrev 25 t)))
  ;; org-mode: elisp completion in src blocks
  (add-hook 'org-mode-hook
    (defun +cape-add-elisp-block-h ()
      (add-hook 'completion-at-point-functions #'cape-elisp-block -5 t)))


  ;; Make server capfs nonexclusive — не блокируют остальные capf
  
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent) ;; <2026-05-23 Sat> :results corfu bug ?
   )
;;

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons :defer t)

;; (use-package hydra
;;   :defer t)

(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

(use-feature executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package apheleia
  :defer t
  :config
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "-a" "-"))
  (setf (alist-get 'rebar3 apheleia-formatters)
      '("rebar3" "fmt" "-"))
  (setf (alist-get 'nph apheleia-formatters)
       '("nph" "-"))
  (setf (alist-get 'nim-mode apheleia-mode-alist)
        'nph)
  (setf (alist-get 'erlang-mode apheleia-mode-alist)
        'rebar3)
  )

(use-package lsp-mode
  :hook (
         ;; (js-mode . lsp)
         ;; (js-jsx-mode . lsp)
         ;; (typescript-mode . lsp)
         ;; (erlang-mode . lsp)
         ;; (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  ;; . lsp-deferred
  ;;:commands lsp
  :custom
  (lsp-server-install-dir (my-expand-var-file "lsp/server/"))
  (lsp-session-file (my-expand-var-file "lsp/lsp-session.el"))
  (lsp-java-server-install-dir (my-expand-var-file "lsp-java/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (my-expand-var-file "lsp-java/workspace/"))

  (lsp-completion-provider :none)
  (lsp-completion-show-kind nil)
  (lsp-completion-show-detail nil)
  (lsp-semgrep-languages nil)
  (lsp-enable-snippet nil)
  :config
    ;; Не обращаться к ELPA/MELPA при старте LSP
  (setq lsp-diagnostic-provider :flycheck)   ; или :flycheck/:flymake — явно
  (setq lsp-auto-configure nil)          ; не автонастраивать пакеты
  (setq lsp-enable-suggest-server-download nil)  ; не предлагать скачивать серверы

  (defun my-dap-use-compilation-mode-augmentation (orig-fn session-name)
    "Заменяем special-mode на compilation-mode в буферах DAP."
    (with-current-buffer (funcall orig-fn session-name)
      (compilation-mode)
      (current-buffer)))

  (advice-add 'dap--create-output-buffer :around
            #'my-dap-use-compilation-mode-augmentation)

  (add-to-list 'exec-path (concat (getenv "HOME") "/.local/state/nix/profile/release"))
  (setq lsp-semgrep-languages nil)
  (setq lsp-auto-guess-root t)
  ;; (add-to-list 'lsp-enabled-clients 'jdtls)
  ;; (setq lsp-enabled-clients '(jdtls jedi elp))
  ;; (setq lsp-disabled-clients '(pyls pylsp))
  ;; (setq lsp-log-io t)
  (setq lsp-restart 'auto-restart)
  ;;TODO:
  ;; (setq lsp-enable-symbol-highlighting nil) ;; у него тут t
  ;; lsp-warn-no-matched-clients t) ;; и это у него включено
  ;; (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-modeline-code-actions-enable nil)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-semantic-tokens-enable nil)
  ;; (setq lsp-enable-folding nil)
  ;; (setq lsp-enable-imenu nil)
  ;; (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)

  ;; Enable LSP automatically for Erlang files
  ;; (add-hook 'erlang-mode-hook #'lsp)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                    :major-modes '(erlang-mode)
                    :priority 0
                    :server-id 'erlang-language-platform))

  ;;emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  )

(use-package lsp-ui
  :after lsp
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

;; (use-package lsp-jedi
;;   :after lsp-mode
;;   )

;; (use-package lsp-java
;;   :after lsp
;;   ;; :hook ((java-mode java-ts-mode) . (lambda () (require 'lsp-java)))
;;   :config
;;   ;; (setq lsp-java-java-path "/usr/lib/jvm/java-17-openjdk/bin/java")
;;   (setq lsp-java-java-path "/home/snake/.local/devjava/sdkman/candidates/java/current/bin/java")
;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-21"
;;                                                :path "/home/snake/.local/devjava/sdkman/candidates/java/current"
;;                                                :default t)])
;;   )

  (use-package lsp-java
    :after lsp
    ;; :demand t  ; загрузить сразу, выполнить :config
    :config
    (setq lsp-java-java-path "/home/snake/.local/devjava/sdkman/candidates/java/25-graal/bin/java")
    (setq lsp-java-configuration-runtimes
          '[(:name "JavaSE-25"
             :path "/home/snake/.local/devjava/sdkman/candidates/java/25-graal"
             :default t)]))
  ;; :hook (java-ts-mode . lsp-deferred)

(use-package lsp-metals
  ;; :after lsp
  :defer t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  ;; :hook (scala-mode . lsp)
  )

;; (use-package lsp-metals
;;   :defer t
;;   :init
;;   (setq lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"
;;                                   "-J-Dmetals.icons=unicode"))
;;   (setq lsp-metals-enable-semantic-highlighting t))

(use-package lsp-treemacs
  :after (lsp-mode)
  :init
  (lsp-treemacs-sync-mode 1))

;; (use-feature lsp-sqls
;;   :after lsp
;;   :custom
;;   (lsp-sqls-connections
;;         '(((driver . "mysql") (dataSourceName . "local:local@tcp(localhost:3306)/testdb"))
;;           ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5433 user=postgres password=machaon dbname=postgres sslmode=disable")))))

(use-package lsp-haskell
  :after lsp

  :config
  ;; (add-hook 'haskell-mode-hook #'lsp)
  ;; (add-hook 'haskell-literate-mode-hook #'lsp)
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  ;; (setq lsp-haskell-plugin-ghcide-type-lenses-global-on nil)
  ;; (setq lsp-haskell-plugin-class-code-lens-on nil)
  ;; (setq lsp-haskell-plugin-import-lens-code-lens-on nil)
  ;; (setq lsp-haskell-plugin-import-lens-code-actions-on nil)
  ;; (setq lsp-haskell-plugin-ghcide-type-lenses-config-mode nil)
  ;; (setq lsp-haskell-plugin-stan-global-on nil)
  )

(use-package lsp-pyright
  :disabled t
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(
       ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres password=machaon dbname=postgres sslmode=disable"))
      ))

(use-package envrc
    :config (add-hook 'elpaca--post-queues-hook #'envrc-global-mode))

;; (use-package envrc
;;   :hook (elpaca-after-init . envrc-global-mode))

(use-package haskell-mode
  :defer t)

(use-feature java-ts-mode
  :custom
  (java-ts-mode-enable-doxygen t))

(use-feature python
  :defer t
  :custom
  (python-indent-guess-indent-offset-verbose nil)) ;; Remove warning : Can’t guess python-indent-offset
;; (use-package python-mode
;;   :init
;;     (add-hook 'python-ts-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'compile-command)
;;                    (concat "python3 " (buffer-name))))))

(use-package groovy-mode
:mode (("build\\.gradle" . groovy-mode)
       ("Jenkinsfile" . groovy-mode)))

(use-package kotlin-ts-mode
  :ensure (kotlin-ts-mode :host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kts?m?\\'")

(use-package elixir-ts-mode
    :mode (("\\.ex\\'" . elixir-ts-mode)
           ("\\.exs\\'" . elixir-ts-mode)
           ("\\mix.lock\\'" . elixir-ts-mode)))

(use-package erlang
  :defer t
  :config
  (setq erlang-root-dir (concat (getenv "XDG_DATA_HOME") "/mise/installs/erlang/latest/"))
  (setenv "MANPATH" (concat erlang-root-dir "man"))

  (defun erlang-run-main()
  "Компилирует текущий модуль и вызывает main/0 в шелле."
  (interactive)
  ;; Сохраняем буфер и подготавливаем оболочку
  (save-some-buffers)
  (inferior-erlang-prepare-for-input)
  ;; Компилируем текущий файл (тот же механизм, что и C-c C-k)
  (let* ((file-name (erlang-local-buffer-file-name))
         (module-name (file-name-base file-name))
         (erl-shell (get-buffer inferior-erlang-buffer))
         (compile-command (inferior-erlang-compute-compile-command
                           (substring file-name 0 -4)
                           (append (list (cons 'outdir (inferior-erlang-compile-outdir)))
                                   erlang-compile-extra-opts))))
    ;; Отправляем команду компиляции
    (inferior-erlang-send-command compile-command nil)
    ;; Затем вызываем main/0
    (sit-for 0.2) ; маленькая задержка, чтобы успело скомпилироваться
    (comint-send-string erl-shell (format "%s:main().\n" module-name))))
  )
  ;; :config
  ;; (+eglot-register '(erlang-mode) "elp-server"))

  ;; :init
 ;; (lsp-register-client
 ;;   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
 ;;                    :major-modes '(erlang-mode)
 ;;                    :priority 0
 ;;                    :server-id 'erlang-language-platform))

(use-package nim-mode
  :defer t
  :mode ("\\.nim\\'" . nim-mode)
        ("\\.nimble\\'" . nimscript-mode-maybe))

(use-package rustic
  :ensure (rustic :host github :repo "emacs-rustic/rustic")
  :defer t)

(use-package scala-mode
  :defer t
  :interpreter ("scala" . scala-mode)
  :config
  (defvar scala3-project-cache (make-hash-table :test 'equal))

  (defun is-scala3-project ()
    (when-let* ((proj (project-current))
              (root (project-root proj)))
      (or (gethash root scala3-project-cache)
          (puthash root
                   (when (file-exists-p (concat root "build.sbt"))
                     (with-temp-buffer
                       (insert-file-contents (concat root "build.sbt"))
                       (when (re-search-forward
                              "scalaVersion[[:space:]]*:=[[:space:]]*\\([a-zA-Z0-9\"._-]+\\)"
                              nil t)
                         (let ((val (match-string 1)))
                           (goto-char (point-min))
                           (re-search-forward
                            (concat val "[[:space:]]*=[[:space:]]*\"3\\.")
                            nil t)))))
                   scala3-project-cache))))

  (defun disable-scala-indent ()
    (when (is-scala3-project)
      (setq indent-line-function 'indent-relative-maybe)))

  (add-hook 'scala-mode-hook #'disable-scala-indent))

(use-package sbt-mode
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

(use-package justl
 :defer t)

(use-package just-mode
 :defer t)

(use-package racket-mode
  :hook (racket-mode . racket-xp-mode)
  :general
  (general-define-key :states '(normal) :keymaps 'racket-mode-map
                      (kbd "E") 'racket-eval-last-sexp))

(use-package geiser
  :defer t)

(use-package geiser-chez
  :defer t)

(use-package geiser-guile
  :defer t)

(use-package geiser-mit
  :defer t)

(use-package macrostep-geiser
  :after (geiser)
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup))

(use-package web-mode
  :mode
  "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-java-test-runner (my-expand-var-file "lsp-java/eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar"))
  :config
  (dap-mode t))

;; Двигаем текст между строк.
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

;; (use-package eldoc
;;   :preface
;;   (unload-feature 'eldoc t)
;;   (setq custom-delayed-init-variables '())
;;   (defvar global-eldoc-mode nil)
;;   :config
;;   (global-eldoc-mode))

(use-package eldoc-box
  :hook (prog-mode . +eldoc-box-hover-at-point-mode-maybe)
  ;; :hook (eglot-managed-mode . +eldoc-box-hover-at-point-mode-maybe)
  :init
  (defun +eldoc-box-hover-at-point-mode-maybe (&optional arg)
    (when (display-graphic-p)
      (eldoc-box-hover-at-point-mode arg))))

(use-package treesit-fold
  :ensure (treesit-fold :host github :repo "emacs-tree-sitter/treesit-fold")
  :defer t)

;; (use-feature nxml-mode
;;   :mode "\\.xml\\'"
;;   :config
;;   (+eglot-register '(nxml-mode xml-mode) "lemminx"))

(use-package sly
  :hook ((lisp-mode-local-vars . sly-editing-mode))
  :custom
  (sly-net-coding-system 'utf-8-unix)
  (sly-complete-symbol-function 'sly-simple-completions) ;; быстрее ?
  (sly-kill-without-query-p t)
  :config
  (setq sly-mrepl-history-file-name (concat user-emacs-directory "sly-mrepl-history"))


  (dolist (impl '("lisp"   ; Default Lisp implementation on the system
                  "clisp"  ; GNU CLISP
                  "abcl"   ; Armed Bear Common Lisp
                  "ecl"    ; Embeddable Common-Lisp
                  "gcl"    ; GNU Common Lisp
                  "ccl"    ; Clozure Common Lisp
                  "cmucl"  ; CMU Common Lisp
                  "clasp"  ; Common Lisp on LLVM
                  "sbcl")) ; Steel Bank Common Lisp
    (when (executable-find impl)
      (add-to-list
       'sly-lisp-implementations
       `(,(intern impl) (,impl) :coding-system utf-8-unix))))
  (setq inferior-lisp-program (caar (cdar sly-lisp-implementations))
        sly-default-lisp (caar sly-lisp-implementations))
  ;;;
    (defun +common-lisp--cleanup-sly-maybe-h ()
    "Kill processes and leftover buffers when killing the last sly buffer."
    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'sly-mode buf)
                             (get-buffer-window buf))
                     return t)
      (dolist (conn (sly--purge-connections))
        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
      (let (kill-buffer-hook kill-buffer-query-functions)
        (mapc #'kill-buffer
              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (buffer-local-value 'sly-mode buf)
                       collect buf)))))
 ;;;
    (defun doom-temp-buffer-p (buf)
      "Returns non-nil if BUF is temporary."
      (equal (substring (buffer-name buf) 0 1) " "))

 ;;;
    (progn
      (defun +common-lisp-init-sly-h nil
        "Attempt to auto-start sly when opening a lisp buffer."
        (cond ((or (doom-temp-buffer-p (current-buffer)) (sly-connected-p)))
              ((executable-find (car (split-string inferior-lisp-program)))
               (let ((sly-auto-start 'always))
                 (sly-auto-start)
                 (add-hook 'kill-buffer-hook
                           (function +common-lisp--cleanup-sly-maybe-h) nil t)))
              ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                        inferior-lisp-program))))
      (dolist (hook '(sly-mode-hook))
        (dolist (func (list (function +common-lisp-init-sly-h)))
          (add-hook hook func nil nil))))


  :init

  (progn
    (progn
      (with-eval-after-load 'emacs
        (remove-hook 'lisp-mode-hook (function sly-editing-mode))))
    (progn
      (with-eval-after-load 'sly
        (remove-hook 'lisp-mode-hook (function sly-editing-mode)))))


  (add-hook 'lisp-mode-local-vars-hook #'sly-lisp-indent-compatibility-mode 'append)

    (progn
    (dolist (hook '(after-init-hook))
      (dolist
          (func
           (list
            (lambda (&rest _) (progn (with-eval-after-load 'sly (sly-setup))))))
        (add-hook hook func nil nil))))

  )

(use-package sly-asdf
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))

;; (use-package sly-quicklisp
;;   :after (sly)
;;   (require 'sly-quicklisp-autoloads)
;;   )

(use-package sly-stepper
  :disabled t
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-stepper))

;;(use-package sly-macrostep )

(use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-overlay
  :defer t)

(use-package puni
  :hook (((
           lisp-mode
           ) . puni-mode)
         (puni-mode . electric-pair-mode))
          )
  :preface
  (define-advice puni-kill-line (:before (&rest _) back-to-indentation)
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*" nil)
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation))))

(use-package lispy
  :ensure (:host github :repo "enzuru/lispy")
  :defer t)

(use-package ejc-sql
  :disabled t
  :defer t
  :commands ejc-sql-mode ejc-connect
  :config
  ;(+eglot-register '(sql-mode) "sqls")
  (setq ejc-result-table-impl 'ejc-result-mode)
  (defun k/sql-mode-hook () (ejc-sql-mode t))
  (add-hook 'sql-mode-hook 'k/sql-mode-hook)

  (add-hook 'ejc-sql-connected-hook
            (lambda ()
              (ejc-set-column-width-limit 150)
              (ejc-set-fetch-size 120)
              (ejc-set-use-unicode t)))

  (defun sql/indent-tabs-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'sql-mode-hook #'sql/indent-tabs-mode)
  (ejc-create-connection
   "MariaDB-db-connection"
   :dependencies [[org.mariadb.jdbc/mariadb-java-client "2.6.0"]]
   :classname "org.mariadb.jdbc.Driver"
   :connection-uri "jdbc:mariadb://localhost:3306/sqlstepik"
   :user "root"
   :password "root"

   )
)

;; (use-feature sql
;;    :config
;;    (+eglot-register '(sql-mode) "sqls"))

(use-package flycheck
  :commands (flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit "necessary with alternatives to package.el"))

;; =package-lint= integration for flycheck.
(use-package flycheck-package
  :after (flycheck)
  :config (flycheck-package-setup)
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*"  display-buffer-below-selected (window-height . 0.15))))

(use-feature flymake
  :general
  (global-leader
    :major-modes '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps     '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "f" '(:ignore t :which-key "flymake")
    "ff" '((lambda () (interactive) (flymake-mode 'toggle)) :which-key "toggle flymake-mode")
    "fn" 'flymake-goto-next-error
    "fp" 'flymake-goto-prev-error)
  ;; :hook (flymake-mode . (lambda () (or (ignore-errors flymake-show-project-diagnostics)
  ;;                                      (flymake-show-buffer-diagnostics))))
  :config
  (setq flymake-suppress-zero-counters nil)


  (add-to-list 'display-buffer-alist
               '("\\`\\*Flymake diagnostics.*?\\*\\'"
                 display-buffer-in-side-window  (window-parameters (window-height 0.25)) (side . bottom)))

  (defun +flymake-elpaca-bytecomp-load-path ()
    "Augment `elisp-flymake-byte-compile-load-path' to support Elpaca."
    (setq-local elisp-flymake-byte-compile-load-path
                `("./" ,@(mapcar #'file-name-as-directory
                                 (nthcdr 2 (directory-files (expand-file-name "builds" elpaca-directory) 'full))))))
  (add-hook 'flymake-mode-hook #'+flymake-elpaca-bytecomp-load-path))

;; (use-feature flyspell
;;   :commands (flyspell-mode flyspell-prog-mode)
;;   :general
;;   (+general-global-toggle
;;     "ss" 'flyspell-mode
;;     "sp" 'flyspell-prog-mode)
;;   (+general-global-spelling
;;     "n" 'flyspell-goto-next-error
;;     "b" 'flyspell-buffer
;;     "w" 'flyspell-word
;;     "r" 'flyspell-region)
;;   :hook ((org-mode mu4e-compose-mode git-commit-mode) . flyspell-mode))

;; "This package provides functionality for correcting words via custom interfaces."
;; --
;; https://d12frosted.io/posts/2016-05-09-flyspell-correct-intro.html

(use-package flyspell-correct
  :disabled t
  :after (flyspell)
  :general
  (+general-global-spelling
    "B" 'flyspell-correct-wrapper
    "p" 'flyspell-correct-at-point))

(use-package fvwm-mode
  :ensure (fvwm-mode :host github :repo "theBlackDragon/fvwm-mode")
  :defer t
  :commands fvwm-mode )

(use-feature ispell
  :custom
  (ispell-alternate-dictionary (file-truename "~/.config/emacs/dict/english-words.txt")))

(use-package treemacs
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :custom
  (treemacs-sorting 'alphabetic-numeric-asc)
  :config
  (setq treemacs-git-executable (executable-find "git"))
  :init
  ;; from doom
  (defun +treemacs/toggle ()
  "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for old functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (let ((project (treemacs--find-current-user-project)))
         (if (and project (not (file-equal-p project "~")))
             (treemacs-add-and-display-current-project-exclusively)
           (message "No valid project in current buffer; opening last treemacs session")
           (treemacs))))))
  )

(use-package treemacs-evil
  :after (treemacs evil)
  )

(use-package treemacs-nerd-icons
  :defer t
  :config
  (treemacs-load-theme "nerd-icons"))

;; (use-package modern-icons
;;   :ensure(modern-icons :host github :repo "emacs-modern-icons/modern-icons-treemacs.el" :files "*.el"))

(use-package treemacs-magit
  :after (treemacs magit)
  )

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :disabled t
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :disabled t
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;; Надо ?
;; (use-package org-contrib)

(use-package yaml-pro
  :hook
  (yaml-mode . yaml-pro-mode)
  (yaml-ts-mode . yaml-pro-ts-mode)
  :custom
  (yaml-pro-ts-yank-subtrees nil))

(use-feature yaml-ts-mode
  :custom
  (yaml-indent-offset 2)
  (yaml-backspace-function 'backward-delete-char-untabify)

  :config
  (defun my-yaml-ts-mode-setup ()
    "Setup proper indentation and keys for yaml-ts-mode."
    (require 'yaml-mode)
    (setq-local indent-line-function 'yaml-indent-line)

    (local-set-key (kbd "RET") 'newline-and-indent)
    (local-set-key (kbd "DEL") 'yaml-electric-backspace)
    (local-set-key (kbd "|") 'yaml-electric-bar-and-angle)
    (local-set-key (kbd ">") 'yaml-electric-bar-and-angle)
    (local-set-key (kbd "-") 'yaml-electric-dash-and-dot)
    (local-set-key (kbd ".") 'yaml-electric-dash-and-dot))

  :hook (yaml-ts-mode . my-yaml-ts-mode-setup))

(use-package indent-bars
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth nil)
  (indent-bars-display-on-blank-lines nil))

(use-package org-cliplink
  :defer t)

(use-package org-modern
  :after (org)
  :custom
  (org-modern-block-fringe nil)
  (org-modern-progress nil) ;;  ?
  (org-modern-table nil) ;; поломано
  (org-modern-horizontal-rule (make-string 36 ?─)) ;; "───────────"
  (org-modern-priority t)
  (org-modern-replace-stars "◉○✸✿✤✜◆▶")
  ;; (org-modern-checkbox nil)
  (org-modern-checkbox '((88 . "☑") (45 . "❍") (32 . "☐")))
  (org-modern-priority
      '((?A . #("❗" 0 1 (face (:foreground "orange" :weight bold :inverse-video t))))
        (?B . #("⬆"  0 1 (face (:foreground "yellow" :weight bold :inverse-video t))))
        (?C . #("⬇"  0 1 (face (:foreground "green"  :weight bold :inverse-video t))))
        (?D . #("⚑"  0 1 (face (:foreground "red"    :weight bold :height 1.2 ))))))
  :config
  ;; (setf (alist-get ?X org-modern-checkbox) #("□x" 0 2 (composition ((2)))))
  (setq org-modern-star 'replace)
  (setq org-modern-block-name
        '((t . t)
          ;; ("src" "»" "«")
          ("src" "" "_")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪")))
  (global-org-modern-mode)
  (remove-hook 'org-agenda-finalize-hook 'org-modern-agenda))

(use-package auto-tangle-mode
  :ensure (auto-tangle-mode
           :host github
           :repo "progfolio/auto-tangle-mode.el"
           :local-repo "auto-tangle-mode")
  :commands (auto-tangle-mode))

(use-feature ob-tangle
  :after (org)
  :custom
  (org-src-window-setup 'current-window)
  (org-src-preserve-indentation t)
  :general
  (global-leader :keymaps 'org-mode-map
    "b"   '(:ignore t :which-key "babel")
    "bt"  'org-babel-tangle
    "bT"  'org-babel-tangle-file
    "be"  '(:ignore t :which-key "execute")
    "beb" 'org-babel-execute-buffer
    "bb"  'org-babel-tangle-block
    "bes" 'org-babel-execute-subtree)
  :config
  (defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
     (call-interactively 'org-babel-tangle)
     ))
  (dolist (template '(("f" . "src fountain")
                      ("se" . "src emacs-lisp :lexical t")
                      ("ss" . "src shell")
                      ("sj" . "src javascript")))
  (add-to-list 'org-structure-template-alist template))
  (use-feature ob-js
    :commands (org-babel-execute:js))
  (use-feature ob-clojure
    :commands (org-babel-execute:clojure))
  (use-feature ob-perl
    :commands (org-babel-execute:perl))
  (use-feature ob-awk
    :commands (org-babel-execute:awk))
  ;; (use-feature ob-sqlite
  ;;   :commands (org-babel-execute:sqlite))
  (use-feature ob-sqlite
    :commands (org-babel-execute:sqlite)
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
             '((sqlite . t)))))

  (use-feature ob-sql
    :commands (org-babel-execute:sql)
    :config
    (add-to-list 'org-babel-load-languages '(sql . t))
    )
  (use-feature ob-python
    :commands (org-babel-execute:python))
  (use-feature ob-haskell
    :commands (org-babel-execute:haskell))
  (use-feature ob-shell
    :commands (org-babel-execute:bash
               org-babel-execute:shell
               org-babel-execute:sh
               org-babel-expand-body:generic)
    :config
    (add-to-list 'org-babel-load-languages '(shell . t))
    )
  (use-feature ob-java
    :commands (org-babel-execute:java)
    :config
    (nconc org-babel-default-header-args:java
           '((:dir . nil)
             (:results . "output")))
  )
  (use-feature ob-jshell
    :load-path "~/.config/emacs/lisp"
    :commands (org-babel-execute:jshell)
    :config
    (add-to-list 'org-babel-load-languages '(jshell . t)))

    (use-feature ob-scala-cli
    :load-path "~/.config/emacs/lisp/"
    :commands (org-babel-execute:scala-cli)
    :config
    (add-to-list 'org-babel-load-languages '(scala-cli . t)))

  ;; (use-feature ob-scala
  ;;   :load-path "~/.config/emacs/lisp/"
  ;;   :commands (org-babel-execute:scala-cli)
  ;;   :config
  ;;     (setq org-babel-scala-command "scala-cli")
  ;;     (setq org-babel-scala-wrapper-method "%s")
  ;; )
  )

(use-package ob-mermaid
  :after org
  :config
  ;; Add mermaid to org-babel languages once ob-mermaid is available
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((mermaid . t)))))

(use-package org-roam
  :ensure (org-roam :host github :repo "org-roam/org-roam")
  :disabled t
  :general ;;
  (+general-global-application
    "or" '(:ignore t :which-key "org-roam-setup"))
  :init (setq org-roam-v2-ack t))

(use-package denote
  :disabled t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/OrgFiles/"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package volatile-highlights
  :disabled t
  :custom
  ;; Animation: choose one of 'static, 'fade-in, or 'pulse
  (vhl/animation-style 'fade-in)
  ;; Also mark deletion points (zero-width ranges)
  (vhl/highlight-zero-width-ranges t)
  :config
  (volatile-highlights-mode 1)
  ;; Prefer customize-set-variable (or setopt on Emacs 29.1+) so :set hooks run
  (customize-set-variable 'vhl/animation-mid-frames 4)
  (customize-set-variable 'vhl/animation-frame-interval 0.03)
  ;; On Emacs 29.1+ you can instead use:
  ;; (setopt vhl/animation-mid-frames 4
  ;;         vhl/animation-frame-interval 0.03)
  )

(use-package org
  :ensure (:autoloads "org-loaddefs.el")
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         )
  :defer t
  :general
  (general-define-key :states '(normal) :keymaps 'org-mode-map
                      (kbd "<tab>") 'org-cycle
                      (kbd "<backtab>") 'org-shifttab)
  (general-define-key :states '(normal insert) :keymaps 'org-mode-map
                      (kbd "M-l") 'org-metaright
                      (kbd "M-h") 'org-metaleft
                      (kbd "M-k") 'org-metaup
                      (kbd "M-j") 'org-metadown
                      (kbd "M-L") 'org-shiftmetaright
                      (kbd "M-H") 'org-shiftmetaleft
                      (kbd "M-K") 'org-shiftmetaup
                      (kbd "M-J") 'org-shiftmetadown
                      (kbd "C-M-<return>") 'org-insert-subheading)
  (general-define-key :states  '(motion) :keymaps 'org-mode-map
                      (kbd "RET") 'org-open-at-point)
  ;;<tab> is for GUI only. TAB maps to C-i on terminals.
  (+general-global-application
    "o"   '(:ignore t :which-key "org")
    "oc"  'org-capture
    ;; "oC"  '+org-capture-again
    "ol"  'org-store-link
    "om"  'org-tags-view
    ;;"os"  'org-search-view
    "oT"  'org-todo-list
    "ot"  '(:ignore t :which-key "timer")
    "otr" 'org-timer-show-remaining-time)

  (global-leader
    ;;for terminals
    :keymaps '(org-mode-map)
    "TAB" 'org-cycle
    "."  'org-time-stamp
    "!"  'org-time-stamp-inactive
    "<"  'org-date-from-calendar
    ">"  'org-goto-calendar

    "l" '(:ignore t :which-key "links")
    "lc" 'org-cliplink
    "ll" 'org-insert-link

    "T"  '(:ignore t :which-key "toggle")
    "Tc"  'org-toggle-checkbox
    "Tx"  'org-latex-preview
    ;;"RET" 'org-ctrl-c-ret
    "RET" 'org-meta-return
    "#"   'org-update-statistics-cookies
    "'"   'org-edit-special
    "*"   'org-ctrl-c-star
    "-"   'org-ctrl-c-minus
    "A"   'org-attach)
    :config
    (add-to-list 'org-src-lang-modes '("xml" . sgml))
    (add-to-list 'org-src-lang-modes '("jshell" . java-ts))
    (add-to-list 'org-src-lang-modes '("scala-cli" . scala))
    (add-to-list 'org-src-lang-modes '("ebnf" . ebnf))
    (add-to-list 'org-src-lang-modes '("json" . json-ts))
    (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts))
    (add-to-list 'org-src-lang-modes '("dockerfile" . dockerfile-ts))
    (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
    (add-to-list 'org-src-lang-modes '("go" . go-ts))
    (add-to-list 'org-src-lang-modes '("mermaid" . mermaid-ts))
    (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
    (add-to-list 'org-src-lang-modes '("html" . html-ts))
    (add-to-list 'org-src-lang-modes '("css" . css-ts))
    (add-to-list 'org-src-lang-modes '("gherkin" . feature))

    (setq org-tags-column -120) ;; так лучше
    (setq org-link-frame-setup '((file . find-file))) ;; в org-ref это по дефолту
    (setq org-fontify-quote-and-verse-blocks t) ;; шрифт в comment и quote блоках. Почему в custom не работает ?
    (setq org-bookmark-names-plist nil)  ;;org-capture. don't automatically set bookmarks


    (defun +md-to-org-region-python (start end)
      "Convert region from markdown to org, replacing selection"
      (interactive "r")
      (shell-command-on-region
       start end
       (format "python3 %s"
               (expand-file-name "lisp/md_to_org_debug.py" user-emacs-directory))
       t t))

    (defun +org-align-all-tags ()
      "Wrap org-align-tags to be interactive and apply to all"
      (interactive)
      (org-align-tags t))

    (defun +org-sparse-tree (&optional arg type)
      (interactive)
      (funcall #'org-sparse-tree arg type)
      (org-remove-occur-highlights))

    (defun +insert-heading-advice (&rest _args)
      "Enter insert mode after org-insert-heading. Useful so I can tab to control level of inserted heading."
      (when evil-mode (evil-insert 1)))

    (advice-add #'org-insert-heading :after #'+insert-heading-advice)

  (defun +org-update-cookies ()
    (interactive)
    (org-update-statistics-cookies "ALL"))

  (add-to-list 'org-emphasis-alist
             '("*" (bold :foreground "#f1e00a")
               ("_" (underline :foreground "#c1d0a4")
               )))

  ;; DWIM открытие ссылок
  (advice-add 'org-open-at-point :around
            (defun org-open-at-point-dwim (orig &optional arg)
              "Если под курсором URL — открыть его, иначе обычное поведение."
              (if (org-in-regexp org-link-any-re)
                  (org-open-at-point-global arg)
                (funcall orig arg))))
  :custom
  (org-modules '(org-habit))
  ;;
  (org-todo-keywords
   ;; '((sequence  "TODO(t)" "STARTED(s!)" "NEXT(n!)" "BLOCKED(b@/!)" "|" "DONE(d)")
   ;;   (sequence  "IDEA(i)" "|" "CANCELED(c@/!)" "DELEGATED(D@/!)")
   ;;   (sequence  "RESEARCH(r)" "|"))
   '((sequence  "TODO(t)" "DONE(d)" )
     (sequence "DEPRECATED(o)"))
   ;;move to theme?
   ;; org-todo-keyword-faces
   ;; `(("CANCELED" . (:foreground "IndianRed1" :weight bold))
   ;;   ("TODO" . (:foreground "#ffddaa"
   ;;                          :weight bold
   ;;                          :background "#202020"
   ;;                          :box (:line-width 3 :width -2 :style released-button)))
   ;;   ("DEPRECATED" . (:foreground "yellow-faint" :weight bold)))
   )
  ;; (org-ellipsis (nth 5 '("↴" "˅" "…" " ⬙" " ▽" "▿")))
  (org-ellipsis " ▿")
  (org-priority-lowest ?D)
  (org-fontify-done-headline t)
  (org-insert-heading-respect-content t) ;; вставить новый хеадер с уважением к контенту !
  (org-M-RET-may-split-line nil "Don't split current line when creating new heading"))

(use-feature org-id
  :after org
  :custom
  (org-id-locations-file (my-expand-var-file "org/id-locations.el")))

(use-package ox-gfm :defer t)

;; Export org-mode docs as HTML compatible with Twitter Bootstrap.

;; https://github.com/marsmining/ox-twbs

(use-package ox-twbs
  :disabled t
  :after (org)
  :defer t)

(use-feature ox-publish
  :after org
  :config
  ;; (setq my-html-preamble
  ;;     (if (file-exists-p "~/Documents/mysite/template/preamble.html")
  ;;         (with-temp-buffer
  ;;           (insert-file-contents "~/Documents/mysite/template/preamble.html")
  ;;           (buffer-string))
  ;;       ""))
  (setq org-publish-timestamp-directory "~/Documents/mysite/")

  (defun my-html-postamble (_plist)
    "Return HTML postamble with a link to external JS file."
    (concat "<script src=\"static/js/main.js\"></script>\n"))

  (defun my-html-preamble (plist)
    "Load preamble from ~/Documents/mysite/template/preamble.html dynamically"
    (let ((preamble-file "~/Documents/mysite/template/preamble.html"))
      (if (file-exists-p preamble-file)
          (with-temp-buffer
            (insert-file-contents preamble-file)
            (buffer-string))
        "")))

  (setq org-export-use-babel nil)
  (setq org-publish-project-alist
      `(
        ("orgfiles"
         :base-directory "~/OrgFiles/Java_Вопросы/Reworked_Questions/"
         :base-extension "org"
         :publishing-directory "~/Documents/mysite/"
         :publishing-function org-html-publish-to-html
         :html-preamble my-html-preamble
         :html-postamble my-html-postamble
         ;; :html-preamble ,my-html-preamble
         :with-broken-links t
         :with-toc nil
         :section-numbers nil
         :html-head "<link rel=\"stylesheet\" href=\"static/css/main.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" href=\"static/css/spa.css\" type=\"text/css\"/>")

        ("images"
         :base-directory "~/OrgFiles/Java_Вопросы/Reworked_Questions/Attachments/"
         :recursive t
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/Documents/mysite/Attachments/"
         :publishing-function org-publish-attachment)
        ("static"
         :base-directory "~/Documents/mysite/static/"
         :base-extension "js\\|css"
         :recursive t
         :base-extension ""
         :publishing-function org-publish-attachment


        ))))

;; A simple Emacs minor mode for a nice writing environment.

;; https://github.com/rnkn/olivetti

(use-package olivetti
  :commands (olivetti-mode))

;; (use-feature org-agenda
;;   :after   (general evil)
;;   :config
;;   (defun +org-agenda-archives (&optional arg)
;;     "Toggle `org-agenda-archives-mode' so that it includes archive files by default.
;;   Inverts normal logic of ARG."
;;     (interactive "P")
;;     (let ((current-prefix-arg (unless (or org-agenda-archives-mode arg) '(4))))
;;       (call-interactively #'org-agenda-archives-mode)))

;;   (defun +org-agenda-place-point ()
;;     "Place point on first agenda item."
;;     (goto-char (point-min))
;;     (org-agenda-find-same-or-today-or-agenda))

;;   (add-hook 'org-agenda-finalize-hook #'+org-agenda-place-point 90)
;;   :general
;;   (+general-global-application
;;   "o#"   'org-agenda-list-stuck-projects
;;   "o/"   'org-occur-in-agenda-files
;;   "oa"   '((lambda () (interactive) (org-agenda nil "a")) :which-key "agenda")
;;   "oe"   'org-store-agenda-views
;;   "oo"   'org-agenda)
;;   (global-leader :keymaps 'org-mode-map
;;   "a"   'org-agenda)
;; ;;Consider cribbing =evilified-state= from Spacemacs?

;; (with-eval-after-load 'org-agenda
;;   (evil-make-intercept-map org-agenda-mode-map)
;;   (general-define-key
;;    :keymaps 'org-agenda-mode-map
;;    ;;:states '(emacs normal motion)
;;    "A"     '+org-agenda-archives
;;    "C"     'org-agenda-clockreport-mode
;;    "D"     'org-agenda-goto-date
;;    "E"     'epoch-agenda-todo
;;    "H"     'org-habit-toggle-habits
;;    "J"     'org-agenda-next-item
;;    "K"     'org-agenda-previous-item
;;    "R"     'org-agenda-refile
;;    "S"     'org-agenda-schedule
;;    "RET"   'org-agenda-recenter
;;    ;; "a"     '+org-capture-again
;;    "c"     'org-agenda-capture
;;    "j"     'org-agenda-next-line
;;    "k"     'org-agenda-previous-line
;;    "m"     'org-agenda-month-view
;;    "t"     'org-agenda-set-tags
;;    "T"     'org-agenda-todo
;;    "u"     'org-agenda-undo))
;; ;;When saving, I want changes to my org-files reflected in any open org agenda
;; ;;buffers.
;;   :config

;; ;;for org-agenda-icon-alist
;; (evil-set-initial-state 'org-agenda-mode 'normal)
;; (defun +org-agenda-redo-all ()
;;   "Rebuild all agenda buffers"
;;   (interactive)
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       (when (derived-mode-p 'org-agenda-mode)
;;         (org-agenda-maybe-redo)))))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook '+org-agenda-redo-all nil t)
;;             (setq prettify-symbols-unprettify-at-point 'right-edge)
;;             (setq prettify-symbols-alist
;;                   (mapcan (lambda (el) (list el (cons (upcase (car el)) (cdr el))))
;;                           '(("#+begin_src"     . "λ")
;;                             ("#+end_src"       . "λ")
;;                             (":properties:"    . "⚙")
;;                             (":end:"           . "∎")
;;                             ("#+results:"      . "→"))))
;;             (prettify-symbols-mode 1)))

;;   :custom
;; ;; Add a custom view for a simplified work agenda.
;; (org-agenda-custom-commands
;;  '(("w" "Work Schedule" agenda "+work"
;;     ((org-agenda-files '("~/Documents/todo/work.org"))
;;      (org-agenda-span 'week)
;;      (org-mode-hook nil)
;;      (org-agenda-start-on-weekday 2)
;;      (org-agenda-timegrid-use-ampm t)
;;      (org-agenda-time-leading-zero t)
;;      (org-agenda-use-time-grid nil)
;;      (org-agenda-archives-mode t)
;;      (org-agenda-weekend-days '(2 3))
;;      (org-agenda-format-date "%a %m-%d")
;;      (org-agenda-prefix-format '((agenda . " %t")))
;;      (org-agenda-finalize-hook
;;       '((lambda ()
;;           "Format custom agenda command for work schedule."
;;           (save-excursion
;;             (goto-char (point-min))
;;             (while (re-search-forward "TODO Work" nil 'noerror)
;;               (replace-match ""))
;;             (goto-char (point-min))
;;             (forward-line) ;skip header
;;             (while (not (eobp))
;;               (when (get-text-property (point) 'org-agenda-date-header)
;;                 (let (fn)
;;                   (save-excursion
;;                     (forward-line)
;;                     (setq fn
;;                           (cond ((or (eobp)
;;                                      (get-text-property (point) 'org-agenda-date-header))
;;                                  (lambda () (end-of-line) (insert " OFF")))
;;                                 ((get-text-property (point) 'time)
;;                                  (lambda () (forward-line) (join-line))))))
;;                   (funcall fn)))
;;               (forward-line))))))))
;;    ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))))
;; (org-agenda-skip-deadline-prewarning-if-scheduled nil "Show approaching deadlines even when scheduled.")
;; ;; I prefer the agenda to start on the current day view instead of the week. It's
;; ;; generally faster to generate and usually what I want.
;; (org-agenda-span 'day)
;; ;; These settings should speed up agenda generation:
;; (org-agenda-inhibit-startup t)
;; ;; But, I'm not sure about this one. It doesn't seem to speed things up that much
;; ;; for me and I like to see inherited tags on tasks.
;; (org-agenda-use-tag-inheritance nil)
;; ;; I find category icons to be a nice visual shorthand that keeps the agenda less cluttered.
;; (org-agenda-prefix-format '((agenda . " %i %?-12t% s")))
;; (org-agenda-category-icon-alist
;;  (let ((image-dir (expand-file-name "images/org/" user-emacs-directory))
;;        (categories '(("[Aa]ccounting" "accounting.svg")
;;                      ("[Bb]irthday"   "birthday.svg")
;;                      ("[Cc]alendar"   "calendar.svg")
;;                      ("[Cc]hore"      "chore.svg"    :height 25)
;;                      ("[Ee]xercise"   "exercise.svg" :height 24)
;;                      ("[Ff]ood"       "food.svg")
;;                      ("[Hh]abit"      "habit.svg")
;;                      ("[Hh]ealth"     "health.svg")
;;                      ("[Ii]n"         "in.svg")
;;                      ("[Ll]isten"     "listen.svg")
;;                      ("[Oo]ut"        "out.svg")
;;                      ("[Pp]lay"       "play.svg")
;;                      ("[Rr]efile"     "refile.svg")
;;                      ("[Rr]ead"       "read.svg")
;;                      ("[Ww]atch"      "watch.svg")
;;                      ("[Ww]ork"       "work.svg"))))
;;    (mapcar (lambda (category)
;;              (list (nth 0 category)
;;                    (expand-file-name (nth 1 category) image-dir)
;;                    'svg
;;                    nil
;;                    :height (or (plist-get category :height) 20)
;;                    :ascent (or (plist-get category :ascent) 'center)))
;;            categories)))
;; ;; This sorting strategy will place habits in/next to the agenda time-grid.
;; (org-agenda-sorting-strategy
;;  '((agenda time-up priority-down category-keep)
;;    (todo priority-down category-keep)
;;    (tags priority-down category-keep)
;;    (search category-keep)))

;; ;; I want the agenda clock report table to skip files that don't have any time
;; ;; clocked for the current agenda view.
;; (org-agenda-clockreport-parameter-plist
;;  '(:link t :maxlevel 2 :stepskip0 t :fileskip0 t))

;; ;; I don't need to see the word "Scheduled" before scheduled items.
;; (org-agenda-scheduled-leaders '("" "%2dx "))

;; ;; Align tags to column 80 in the agenda view:
;; (org-agenda-tags-column -80)

;; )

;; for more examples [[https://github.com/progfolio/.emacs.d/blob/7bc8b278c00047e3cfaebbafb7674f77ff7f70f2/init.org?plain=1#L2676][progfolio]]
(use-feature org-capture
  :after org
  :config

  (defun +org-capture-here ()
    "Convenience command to insert a template at point"
    (interactive)
    (org-capture 0))


  (setq org-capture-templates
      (doct `(("Appointment"
               :keys "a"
               :id "2cd2f75e-b600-4e9b-95eb-6baefeaa61ac"
               :properties ((Created "%U"))
               )

              ("Interview Task"
               :keys "i"
               :file "~/Documents/interview-tasks.org" ; Specify your target Org file
               ;; :headline "Tasks"            ; Optional: Specify a headline to file under
               :type plain                  ; Entry type is an Org node with a headline
               :function (lambda () (org-back-to-heading t) (org-end-of-meta-data t)) ; После существующего :PROPERTIES:
               :template (
                          ":PROPERTIES:"
                          ":TaskType: %^{TaskType| |algorithm|system design|sql|writecode|review|testovoe_zadanie|}"
                          ":Difficulty: %^{Difficulty| |easy|medium|hard}"
                          ":Topics: %^{Topics| |array|hashmap|tree|graph|DP|transactions|spring|java_core|}"
                          ":Company: %^{Company}"
                          ":Interviewers: %^{Interviewers}"
                          ":Date: %^{Date|%t}"
                          ":END:"
                          "%?")
               ;; :prepend t                   ; Insert at the beginning of the headline
               :immediate-finish t ; Завершить сразу после заполнения
               )
              ("Anki Decks"
               :keys "d"
               :type plain
               :function (lambda () (org-back-to-heading t) (org-end-of-meta-data t))
               :template (
                          ":PROPERTIES:"
                          ":ANKI_DECK: %^{ANKI_DECK| |Java|}"
                          ":ANKI_NOTE_TYPE: %^{ANKI_NOTE_TYPE| |Basic|}"
                          ":ANKI_TAGS: %^{ANKI_TAGS| |SQL|}"
                          ":END:"
                          "%?")
               :immediate-finish t
               )
              )))

  :general
  (:states 'normal
         :keymaps 'org-capture-mode-map
         ",c" 'org-capture-finalize
         ",k" 'org-capture-kill
         ",r" 'org-capture-refile)
;; :custom
;; (org-capture-dir (concat (getenv "HOME") "/Documents/todo/"))
)

(use-package ob-duckdb
  :disabled t
  :after org
  :custom
  ;; Limit output to prevent Emacs freezing on large results
  (org-babel-duckdb-max-rows 200000)

  ;; Show progress for async queries
  (org-babel-duckdb-show-progress t)
  (org-babel-duckdb-progress-display 'minibuffer) ; or 'popup

  ;; Auto-show queue when multiple async queries pending
  (org-babel-duckdb-queue-display 'auto) ; or 'manual
  (org-babel-duckdb-queue-position 'bottom) ; or 'side

  :config
  ;; Optional: MotherDuck token from file
  ;; (setq org-babel-duckdb-motherduck-token
  ;;       (lambda ()
  ;;         (with-temp-buffer
  ;;           (insert-file-contents "~/.config/duckdb/.motherduck_token")
  ;;           (string-trim (buffer-string)))))
  )

(use-package ob-racket
  :ensure (ob-racket :host github :repo "hasu/emacs-ob-racket" :files ("*.el" "*.rkt"))
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	      #'ob-racket-raco-make-runtime-library))

(use-package org-clean-refile
  :disabled t
  :elpaca (org-clean-refile :host github :repo "progfolio/org-clean-refile" :protocol ssh)
  :commands (org-clean-refile)
  :after (org)
  :general
  (global-leader
    :keymaps 'org-mode-map
    "sr" 'org-clean-refile))

(use-feature org-habit
  :after (org)
  :config
  (defun +org-habit-graph-on-own-line (graph)
    "Place org habit consitency graph below the habit."
    (let* ((count 0)
           icon)
      (save-excursion
        (beginning-of-line)
        (while (and (eq (char-after) ? ) (not (eolp)))
          (when (get-text-property (point) 'display) (setq icon t))
          (setq count (1+ count))
          (forward-char)))
      (add-text-properties (+ (line-beginning-position) count) (line-end-position)
                           `(display ,(concat (unless icon "  ")
                                              (string-trim-left (thing-at-point 'line))
                                              (make-string (or org-habit-graph-column 0) ? )
                                              (string-trim-right
                                               (propertize graph 'mouse-face 'inherit)))))))
;; I've submitted a [[https://orgmode.org/list/87h7sx5f5z.fsf@gmail.com/T/#t][patch]] to customize consistency graph placement in the agenda.
;; Rather than constantly rebase my patch on top of the latest Org, I'm adding advice
;; to override the default placement.

  (defun +org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t)
        (buffer-invisibility-spec '(org-link))
        (moment (time-subtract nil (* 3600 org-extend-today-until))))
    (save-excursion
      (goto-char (if line (line-beginning-position) (point-min)))
      (while (not (eobp))
        (let ((habit (get-text-property (point) 'org-habit-p)))
          (when habit
            (let ((graph (org-habit-build-graph
                          habit
                          (time-subtract moment (days-to-time org-habit-preceding-days))
                          moment
                          (time-add moment (days-to-time org-habit-following-days)))))
              (+org-habit-graph-on-own-line graph))))
        (forward-line)))))

        (advice-add #'org-habit-insert-consistency-graphs
  :override #'+org-habit-insert-consistency-graphs)

  :custom
  (org-habit-today-glyph #x1f4c5)
  (org-habit-completed-glyph #x2713)
  (org-habit-preceding-days 29)
  (org-habit-following-days 1)
  (org-habit-graph-column 3)
  (org-habit-show-habits-only-for-today nil)

  (integerp nil))

(use-feature org-indent
  :diminish ""
  :after (org)
  :hook (org-mode . org-indent-mode)
  :config
  (define-advice org-indent-refresh-maybe (:around (fn &rest args) "when-buffer-visible")
    "Only refresh indentation when buffer's window is visible.
Speeds up `org-agenda' remote operations."
    (when (get-buffer-window (current-buffer) t) (apply fn args))))

;; This function allows me to refile within the currently open org files
;; as well as agenda files. Useful for structural editing.
;; Stolen from: [[https://emacs.stackexchange.com/questions/22128/how-to-org-refile-to-a-target-within-the-current-file?rq=1][stackoverflow: how to org-refile to a target within the current file?]]

(defun +org-files-list ()
  "Returns a list of the file names for currently open Org files"
  (delq nil
        (mapcar (lambda (buffer)
                  (when-let* ((file-name (buffer-file-name buffer))
                              (directory (file-name-directory file-name)))
                    (unless (string-suffix-p "archives/" directory)
                      file-name)))
                (org-buffer-list 'files t))))

(setq +org-max-refile-level 20)
(setq org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-refile-targets `((org-agenda-files  :maxlevel . ,+org-max-refile-level)
                           (+org-files-list :maxlevel . ,+org-max-refile-level)))

(setq org-agenda-files '("~/OrgFiles/TODO.org")
      org-directory "~/OrgFiles"
      org-agenda-text-search-extra-files '(agenda-archives)
      org-fold-catch-invisible-edits 'show-and-error
      org-confirm-babel-evaluate nil
      org-enforce-todo-dependencies t
      org-hide-emphasis-markers t
      org-hierarchical-todo-statistics nil
      org-startup-folded 'fold
      org-log-done 'time
      org-log-reschedule t
      org-return-follows-link t
      org-reverse-note-order t
      org-src-tab-acts-natively t
      org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.mp[[:digit:]]\\'" . "/usr/bin/mpv --force-window=yes %s")
        ;;("\\.x?html?\\'" . "/usr/bin/firefox-beta %s")
        ("\\.x?html?\\'" . "/usr/bin/bash -c '$BROWSER  %s'")
        ("\\.pdf\\'" . default)))

;;Set clock report duration format to floating point hours
;;(setq org-duration-format  '(h:mm))
(setq org-duration-format '(("h" . nil) (special . 2)))



(use-package org-make-toc
  :commands (org-make-toc))

(use-package org-mime
  :after (org)
  :commands (org-mime-htmlize
             org-mime-org-buffer-htmlize
             org-mime-org-subtree-htmlize)
  :config
  (setq org-mime-export-options '( :with-latex dvipng
                                   :section-numbers nil
                                   :with-author nil
                                   :with-toc nil)))

(use-package org-pandoc-import
 :ensure (org-pandoc-import :host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors"))
 :defer t
 :after (org))

(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-heading-lvl nil)
  (org-download-image-org-width 600)
  :config
  (setq org-download-annotate-function (lambda (_link) ""))

  (defun my/org-download-get-assets-dir ()
    "Возвращает директорию для вложений, основанную на имени файла."
    (when buffer-file-name
      (let* ((file-dir (file-name-directory buffer-file-name))
             (file-name-base (file-name-base buffer-file-name))
             (assets-dir (expand-file-name ".assets" file-dir)))
        (expand-file-name file-name-base assets-dir))))

  (defun my/org-download--get-parent-heading (level)
    "Находит заголовок-родитель уровня LEVEL."
    (save-excursion
      (let (heading)
        (org-back-to-heading t)
        (while (> (org-outline-level) level)
          (org-up-heading-safe))
        (when (= (org-outline-level) level)
          (setq heading (org-get-heading t t t t)))
        heading)))

  (defun my/org-download--cleanup-string (s)
    "Очищает строку S, делая ее пригодной для имени файла."
    (when s
      (let ((cleaned (replace-regexp-in-string "[^a-zA-Zа-яА-Я0-9]+" "_" s)))
        ;; Убираем множественные подчеркивания
        (setq cleaned (replace-regexp-in-string "_+" "_" cleaned))
        ;; Убираем _ в начале и конце
        (replace-regexp-in-string "^_+\\|_+$" "" cleaned))))

  (defun my/truncate-to-bytes (s max-bytes)
    "Обрезает строку S так, чтобы она не превышала MAX-BYTES в UTF-8.
     Обрезает по границе символов и убирает trailing underscore."
    (let ((result ""))
      (catch 'done
        (dolist (char (string-to-list s))
          (let* ((char-str (char-to-string char))
                 (char-bytes (length (encode-coding-string char-str 'utf-8)))
                 (current-bytes (length (encode-coding-string result 'utf-8)))
                 (new-total (+ current-bytes char-bytes)))
            (if (<= new-total max-bytes)
                (setq result (concat result char-str))
              (throw 'done result)))))
      ;; Убираем trailing underscore
      (string-trim-right result "_")))

  (defun my/org-download-format-filename (original-filename)
    "Генерирует имя файла: Level1_Level2_Timestamp.ext
     Заголовки обрезаются до 55 байт с учетом лимитов Anki."
    (let* ((ext (file-name-extension original-filename t))
           (extension (or ext ".png"))
           (level1 (my/org-download--cleanup-string
                    (my/org-download--get-parent-heading 1)))
           (level2 (my/org-download--cleanup-string
                    (my/org-download--get-parent-heading 2)))
           (timestamp (format-time-string "%Y%m%dT%H%M%S"))
           (title-part (mapconcat #'identity (delq nil (list level1 level2)) "_"))
           ;; 120 (Anki) - 15 (timestamp) - 1 (sep) - 4 (ext) - 41 (hash) - 4 (reserve) = 55
           (safe-title (my/truncate-to-bytes title-part 55)))
      (concat safe-title "_" timestamp extension)))

  (setq org-download-file-format-function #'my/org-download-format-filename)

  (defun my/org-download-set-directory-before-download (&rest _)
    "Устанавливает org-download-image-dir перед вызовом скачивания."
    (setq-local org-download-image-dir (my/org-download-get-assets-dir)))

  (defun my-avoid-org-id-get-create (orig-fun &rest args)
    "Around advice to prevent org-id-get-create in org-download-clipboard."
    (cl-letf (((symbol-function 'org-id-get-create) (lambda () nil)))
      (apply orig-fun args)))

  (advice-add #'org-download-image :before #'my/org-download-set-directory-before-download)
  (advice-add 'org-download-clipboard :around #'my-avoid-org-id-get-create))

(use-package org-appear
  :hook ((org-mode . org-appear-mode)
         (org-roam-mode . org-appear-mode))
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-feature text-mode
  :hook (text-mode . visual-line-mode))

(use-feature mcp-server
  :ensure (mcp-server :repo "~/.config/emacs/lisp/mcp-server")
  ;; :ensure (mcp-server :host github :repo "rhblind/emacs-mcp-server"
  ;;            :files ("*.el" ("tools/" "tools/*.el") "mcp-wrapper.py" "mcp-wrapper.sh"))
  :disabled t
  :config
  (setq mcp-server-security-allowed-dangerous-functions
      '(find-file with-current-buffer insert-file-contents dired with-temp-buffer))
  (setq mcp-server-emacs-tools-enabled 'all) ;;
  ;; (add-hook 'emacs-startup-hook #'mcp-server-start-unix)
  )

(use-feature ox-latex
  :after org-mode
  :custom
  (org-latex-compiler "lualatex"))

(use-package doct
  :defer t
  :commands (doct))

(use-package vterm
  :ensure
  `(vterm
    :build
    (:after elpaca-build-compile
     ,(elpaca-defscript +vterm-compile-module (:type elisp)
        (add-to-list 'load-path ,(elpaca<-build-dir e))
        (setq vterm-always-compile-module t)
        (require 'vterm))))
  :commands (vterm vterm-other-window)
  :general
  (+general-global-open
    "T" 'vterm-other-window
    "t" 'vterm)
  :init
  (add-to-list 'evil-insert-state-modes #'vterm-mode))

(use-feature winner
  :defer 5
  :general
  (+general-global-window
    "u" 'winner-undo
    "r" 'winner-redo)
  :config (winner-mode))

(use-package expand-region
  :commands (er/expand-region)
  :bind (("C-=" . er/expand-region)))

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :hook (elpaca-after-init-hook . yas-global-mode))
;; (use-package yasnippet
;;   ;; :commands (yas-global-mode)
;;   :commands (yas-expand
;;              yas-minor-mode)
;;   :hook ((prog-mode . yas-minor-mode)
;;          (org-mode . yas-minor-mode))
;;   ;; :custom
;;   ;; (yas-snippet-dirs '("~/.config/emacs/elpaca/repos/snippets"))
;;   ;; (yas-global-mode 1)
;;   ;; :config
;;   ;; (yas-reload-all)

;;   )

;; (use-package yasnippet
;;   :hook ((prog-mode . yas-minor-mode)
;;          (org-mode . yas-minor-mode)))

;; (use-package yasnippet-snippets
;;   :after (yasnippet))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :bind (("M-=" . tempel-complete)
         ("M--" . tempel-insert)
         ("M-]" . tempel-next))
  :custom
  (tempel-trigger-prefix "=")
  :hook ((prog-mode text-mode) . +tempel-setup-capf-h)
  :hook (prog-mode . tempel-abbrev-mode)
  ;; :hook ((text-mode) . +tempel-setup-capf-h)

  :config
  (advice-add 'tempel--insert :after
              (lambda (&rest _)
                (when (bound-and-true-p evil-mode)
                  (evil-insert-state))))

  (setq tempel-path "~/.config/emacs/templates/*.eld")
  (defun +tempel-setup-capf-h ()
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t)))

(use-package lsp-snippet-tempel
  :defer t
  :ensure (lsp-snippet-tempel :host github :repo "svaante/lsp-snippet")
  :config
  (when (featurep 'lsp-mode)
    ;; Initialize lsp-snippet -> tempel in lsp-mode
    (lsp-snippet-tempel-lsp-mode-init))
  (when (featurep 'eglot)
    ;; Initialize lsp-snippet -> tempel in eglot
    (lsp-snippet-tempel-eglot-init)))

(use-package doom-snippets
:ensure (doom-snippets :host github :repo "doomemacs/snippets" :files ("*.el" "*"))
;;:load-path "~/.config/vanilla/snippets"
:after yasnippet)

(use-feature tramp
  :defer t
  :custom
  (tramp-persistency-file-name (my-expand-var-file "tramp/persistency.el"))
  ;; (tramp-auto-save-directory (my-expand-var-file "tramp/auto-save/")) TODO
  (tramp-terminal-type "tramp")
  :config
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
    ;; (defvar my-android-host "192.168.0.14"
    ;; "IP address or hostname of Android device.")

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "android") "remote-shell" "sh"))

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "android")
                     "tmpdir" "/data/data/com.termux/files/home/tmp"))

  (connection-local-set-profile-variables
   'tramp-connection-local-termux-profile
   `((tramp-remote-path
      . ,(mapcar
          (lambda (x)
            (if (stringp x) (concat "/data/data/com.termux/files" x) x))
          (copy-tree tramp-remote-path)))))

  (connection-local-set-profiles
   '(:application tramp :machine "android")
   'tramp-connection-local-termux-profile))

(use-feature vc-hooks
  :custom
  (vc-follow-symlinks t "Visit real file when editing a symlink without prompting."))

;; (use-feature dictionary
;;   :general
;;   (global-definer "W" 'dictionary-lookup-definition)
;;   (+general-global-application "D" 'dictionary-search)
;;   (+general-global-text "d" 'dictionary-search))

;; Tern is a stand-alone, editor-independent JavaScript analyzer that can be used to improve the JavaScript integration of existing editors.

;; https://github.com/ternjs/tern

(use-package tern
  :disabled t
  :commands (tern-mode)
  :hook (js2-mode . tern-mode))

(use-feature window
  :bind (("C-x 2" . vsplit-last-buffer)
         ("C-x 3" . hsplit-last-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  :custom
  (switch-to-buffer-obey-display-actions t)
  (switch-to-prev-buffer-skip-regexp
   '("\\*Help\\*" "\\*Calendar\\*" "\\*mu4e-last-update\\*"
     "\\*Messages\\*" "\\*scratch\\*" "\\magit-.*")))

(use-feature erc
  :commands (erc erc-tls)
  :defines erc-autojoin-channels-alist
  :init (setq erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

(use-package package-lint
  :defer t
  :commands (package-lint-current-buffer +package-lint-elpaca)
  :config
;; package-lint assumes package.el is the package manager.
;; I use elpaca.el, so I get spurious warnings about uninstallable packages.
;; This workaround creates a temporary package archive and enables package.el to appease package-lint.

  (defun +package-lint-elpaca ()
  "Help package-lint deal with elpaca."
  (interactive)
  (require 'package)
  (setq package-user-dir "/tmp/elpa")
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (package-refresh-contents))

(+package-lint-elpaca))

(use-feature paren
  :defer 1
  :config (show-paren-mode))

(use-feature project
  :config
  (add-to-list 'project-switch-commands '(+project-magit-status "Magit" "m"))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" "F"))
  :custom
  (project-list-file (my-expand-var-file "project-list.eld"))
  (project-vc-ignores '("result/"
                        "*.png"
                        ".direnv/"
                        ".node_modules/"
                        ".bloop/" ".metals/" "target/" ".DS_Store"))
  (project-vc-extra-root-markers
   '(".projectile.el" ".project.el" ".project" ; Emacs
     ".repo" ; Repo workspaces
     "autogen.sh" ; Autotools
     "build.sbt"
     "rebar.config"
     ".dir-locals.el"
     "*.csproj" "*.vbproj" "*.vcxproj" "*.vdproj" ".code-workspace" ; Visual Studio
     "requirements.txt" ; Python
     "package.json" ; Node.js
     "pom.xml" ; Apache Maven (Java/Kotlin)
     "gradlew" "build.gradle.kts"
     "Cargo.toml")) ; Cargo (Rust)
  :init
  (defun +project-magit-status ()
  (interactive)
  (magit-status (project-root (project-current))))
)

(use-package pdf-tools
  :disabled t
  :ensure (pdf-tools :pre-build ("./server/autobuild") :files (:defaults "server/epdfinfo"))
  :functions (pdf-isearch-batch-mode)
  :commands (pdf-tools-install pdf-view-mode)
 ;; :custom (pdf-view-midnight-colors '("#AFA27C" . "#0F0E16"))
  :config (add-hook 'pdf-view-mode-hook
                    (lambda ()
                      ;; get rid of borders on pdf's edges
                      (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                      ;;for fast i-search in pdf buffers
                      (pdf-isearch-minor-mode)
                      (pdf-isearch-batch-mode)
                      ;;(pdf-view-dark-minor-mode)
                      ;;(pdf-view-midnight-minor-mode)
                      )
                    )
  :mode (("\\.pdf\\'" . pdf-view-mode)))

(use-package pdf-tools
  :disabled t
  :custom (pdf-view-midnight-colors '("#AFA27C" . "#0F0E16"))
  :config (add-hook 'pdf-view-mode-hook
                    (lambda ()
                      ;; get rid of borders on pdf's edges
                      (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                      ;;for fast i-search in pdf buffers
                      (pdf-isearch-minor-mode)
                      (pdf-isearch-batch-mode)
                      (pdf-view-dark-minor-mode)
                      (pdf-view-midnight-minor-mode)))
  :magic ("%PDF" . pdf-view-mode)
  )

(use-package pass
  :defer t
  :commands (pass pass-view-mode)
  :mode ("\\.password-store/.*\\.gpg\\'" . pass-view-mode)
  :preface
  (defun insert-password ()
    (interactive)
    ;;(shell-command "apg -m24 -x24 -a1 -n1" t))
    (shell-command "gpg --gen-random --armor 1 14" t))

  (add-hook 'pass-view-mode-hook #'pass-view--prepare-otp))

(use-feature auth-source-pass
  ;; :defer t
  :preface
  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
          (puthash
           entry
           (with-temp-buffer
             (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
             (buffer-substring-no-properties (point-min) (point-max)))
           auth-source-pass--cache))))

  (defun auth-source-pass-entries ()
    "Return a list of all password store entries."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
       (lambda (file) (file-name-sans-extension (file-relative-name file store-dir)))
       (directory-files-recursively store-dir "\.gpg$"))))
  :config
  (add-to-list 'auth-sources "~/.password-store/.authinfo")
  (auth-source-pass-enable))

(use-package password-store
  :defer t
  :commands (password-store-insert
             password-store-copy
             password-store-get)
  :custom
  (password-store-password-length 14) ;; или 24
  :functions (password-store--run)
  :config
  (defun password-store--run-edit (entry)
    (require 'pass)
    (find-file (concat (expand-file-name entry (password-store-dir)) ".gpg")))

  (defun password-store-insert (entry login password)
    "Insert a new ENTRY containing PASSWORD."
    (interactive (list (read-string "Password entry: ")
                       (read-string "Login: ")
                       (read-passwd "Password: " t)))
    (message "%s" (shell-command-to-string
                   (if (string= "" login)
                       (format "echo %s | %s insert -m -f %s"
                               (shell-quote-argument password)
                               password-store-executable
                               (shell-quote-argument entry))
                     (format "echo -e '%s\nlogin: %s' | %s insert -m -f %s"
                             password login password-store-executable
                             (shell-quote-argument entry)))))))

(use-package password-store-otp
  :ensure (password-store-otp :version (lambda (_) "0.1.5"))
  :defer t
  :config
  (defun password-store-otp-append-from-image (entry)
    "Check clipboard for an image and scan it to get an OTP URI,
append it to ENTRY."
    (interactive (list (read-string "Password entry: ")))
    (let ((qr-image-filename (password-store-otp--get-qr-image-filename entry)))
      (when (not (zerop (call-process "screencapture" nil nil nil
                                      "-T5" qr-image-filename)))
        (error "Couldn't get image from clipboard"))
      (with-temp-buffer
        (condition-case nil
            (call-process "zbarimg" nil t nil "-q" "--raw"
                          qr-image-filename)
          (error
           (error "It seems you don't have `zbar-tools' installed")))
        (password-store-otp-append
         entry
         (buffer-substring (point-min) (point-max))))
      (when (not password-store-otp-screenshots-path)
        (delete-file qr-image-filename)))))

;; modern replacement rainbow-mode
(use-package colorful-mode
  :commands(colorful-mode)
  ;; :diminish
  ;; :ensure t ; Optional
  ;; :custom
  ;; (colorful-use-prefix t)
  ;; (colorful-only-strings 'only-prog)
  ;; (css-fontify-colors nil)
  ;; :config
  ;; (global-colorful-mode t)
  ;; (add-to-list 'global-colorful-modes 'helpful-mode)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-feature re-builder
  :custom
  (reb-re-syntax 'rx)
  :commands (re-builder))

(use-feature recentf
  ;; :hook (elpaca-after-init . recentf-mode)
  :defer 1
  :config (recentf-mode)
  :custom
  ;; (recentf-show-messages nil)
  ;; (recentf-auto-cleanup 300)
  (recentf-save-file (my-expand-var-file "recentf-save.el"))
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files"))

(use-package dirvish
  :disabled t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg)
        dirvish-side-attributes
        '(vc-state file-size nerd-icons collapse))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map          ; Dirvish inherits `dired-mode-map'
   ("?"   . dirvish-dispatch)     ; contains most of sub-menus in dirvish extensions
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package restclient
  :commands (restclient-mode +web/restclient-new-buffer)
  :config
  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient))
  (defun +web/restclient-new-buffer ()
    "Create a restclient buffer."
    (interactive)
    (let* ((restclient-buffer-name "*restclient*")
           (restclient-buffer (get-buffer restclient-buffer-name)))
      (unless restclient-buffer
        (setq restclient-buffer (generate-new-buffer restclient-buffer-name))
        (with-current-buffer restclient-buffer
          (restclient-mode)
          (when (functionp 'cape-company-to-capf)
            (setq-local completion-at-point-functions
                        (push (cape-company-to-capf 'company-restclient) completion-at-point-functions)))
          (insert "# -*- restclient -*-
# https://github.com/pashky/restclient.el
#
# GET https://api.github.com
# User-Agent: Emacs Restclient
# #
# POST https://jira.atlassian.com/rest/api/2/search
# Content-Type: application/json
# {}
# #
# POST https://somehost/api
# Content-Type: application/x-www-form-urlencoded
# param1=value1¶m2=value2\n")))
      (switch-to-buffer restclient-buffer))))

;; (use-feature shr-color
;;   :custom
;;   (shr-color-visible-luminance-min 85 "For clearer email/eww rendering of bg/fg colors")
;;   (shr-use-colors nil "Don't use colors (for HTML email legibility)"))

(use-feature cus-edit
  :defer t
  :custom
  (custom-file null-device "Don't store customizations"))

(use-feature edebug
  :general
  (global-leader
    :major-modes '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps     '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d" '(:ignore t :which-key "debug")
    "dA" 'edebug-all-defs
    "db" '(:ignore t :which-key "breakpoint")
    "dbU"  'edebug-unset-breakpoints
    "dbc"  'edebug-set-conditional-breakpoint
    "dbg"  'edebug-set-global-break-condition
    "dbn"  'edebug-next-breakpoint
    "dbs"  'edebug-set-breakpoint
    "dbt"  'edebug-toggle-disable-breakpoint
    "dbu"  'edebug-unset-breakpoint
    "dw" 'edebug-where))

(use-package elfeed
  :commands (elfeed)
  :config
  (defvar +elfeed-feed-file (expand-file-name "~/OrgFiles/elfeed.org"))

  (setq elfeed-feeds
        (with-current-buffer (find-file-noselect +elfeed-feed-file)
          (save-excursion
            (save-restriction
              (org-fold-show-all)
              (goto-char (point-min))
              (let ((found nil))
                (org-element-map (org-element-parse-buffer) 'link
                  (lambda (node) (when-let ((props (cadr node))
                                            (standards (plist-get props :standard-properties))
                                            (tags (org-get-tags (aref standards 0)))
                                            ((member "elfeed" tags))
                                            ((not (member "ignore" tags))))
                                   (push (cons (plist-get props :raw-link)
                                               (delq 'elfeed (mapcar #'intern tags)))
                                         found)))
                  nil nil t)
                (nreverse found))))))

  (defun +elfeed-reload-feeds ()
  "Перечитать elfeed.org и обновить elfeed-feeds."
  (interactive)
  (setq elfeed-feeds
        (with-current-buffer (find-file-noselect +elfeed-feed-file)
          (save-excursion
            (save-restriction
              (org-fold-show-all)
              (goto-char (point-min))
              (let ((found nil))
                (org-element-map (org-element-parse-buffer) 'link
                  (lambda (node)
                    (when-let ((props (cadr node))
                               (standards (plist-get props :standard-properties))
                               (tags (org-get-tags (aref standards 0)))
                               ((member "elfeed" tags))
                               ((not (member "ignore" tags))))
                      (push (cons (plist-get props :raw-link)
                                  (delq 'elfeed (mapcar #'intern tags)))
                            found)))
                  nil nil t)
                (nreverse found)))))))

  (defun +elfeed-play-in-mpv ()
    "Play selected videos in a shared mpv instance in chronological order."
    (interactive)
    (mapc (lambda (entry)
            (emp-open-url (elfeed-entry-link entry))
            (message "Playing %S in MPV" (elfeed-entry-title entry)))
          (nreverse (elfeed-search-selected)))
    (elfeed-search-untag-all-unread))

  (defun +elfeed-download ()
    "Download selected videos."
    (interactive)
    (let ((default-directory (expand-file-name "~/Videos/youtube")))
      (dolist (entry (nreverse (elfeed-search-selected)))
        (let ((title (elfeed-entry-title entry)))
          (message "Attempting to download %S" (elfeed-entry-title entry))
          (make-process
           :name "elfeed-download"
           :buffer "elfeed-download"
           :command (list "youtube-dl" (elfeed-entry-link entry))
           :sentinel (lambda (process _event)
                       (when (= 0 (process-exit-status process))
                         (message "Successfully downloaded %S" title))))))
      (elfeed-search-untag-all-unread)))
  :general
  (+general-global-application
    "e"    'elfeed)
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-search-mode-map
   "p" '+elfeed-play-in-mpv
   "d" '+elfeed-download)
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "J" 'elfeed-show-next
   "K" 'elfeed-show-prev)
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  )

(use-feature elisp-mode
  :general
  (global-leader
    :major-modes '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps     '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e"  '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-expression
    "ep" 'pp-eval-last-sexp
    "es" 'eval-last-sexp
    "i"  'elisp-index-search))

(use-feature epg-config
  :defer t
  :init (setq epg-pinentry-mode 'loopback))

(use-feature epa-file
  :defer t
  :init (setq epa-file-cache-passphrase-for-symmetric-encryption t))

;; Когда стандартное поведение lisp-функции непонятно или кажется багнутым. Например:
;; - M-x find-function RET switch-to-buffer — хочешь понять, почему он не принимает C-u как pop-to-buffer
;; - M-. на read-from-minibuffer — лезет в minibuf.c, смотришь как устроен ввод
;; - split-window / delete-other-windows — непонятно почему r bebalance работает неожиданно
;; - call-interactively — хочешь понять как Emacs разбирает interactive specs
;; - Любая функция из C иконкой ⓒ в C-h f — signal, car, mapcar, accept-process-output
;; Короче, когда lisp-реализация ссылается на C-примитив ~((defalias 'car #'..., subr)~ или функция написана целиком на C (фреймовые/буферные/процессные/оконные менеджеры). Это не повседневщина, но при отладке или глубоком изучении — спасает.

(use-feature find-func
  :defer t
  :config (setq find-function-C-source-directory
                (expand-file-name "~/.local/src_builds/emacs/src")))

(use-feature display-fill-column-indicator
  :custom
  (display-fill-column-indicator-character
   (plist-get '( triple-pipe  ?┆
                 double-pipe  ?╎
                 double-bar   ?║
                 solid-block  ?█
                 empty-bullet ?◦)
              'triple-pipe))
  :general
  (+general-global-toggle
    "F" '(:ignore t :which-key "fill-column-indicator")
    "FF" 'display-fill-column-indicator-mode
    "FG" 'global-display-fill-column-indicator-mode))

;; Fontify symbols representing faces with that face.

;; https://github.com/Fuco1/fontify-face

(use-package fontify-face
  :commands (fontify-face-mode))

(use-feature help
  :defer 1
  :custom
  (help-window-select t "Always select the help window"))

(use-package helpful
    :init (setq evil-lookup-func #'helpful-at-point)
    :bind
    ([remap describe-function] . helpful-callable)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key))

;; (use-feature holidays
;;   :commands (org-agenda)
;;   :custom
;;   (holiday-bahai-holidays nil)
;;   (holiday-hebrew-holidays nil)
;;   (holiday-islamic-holidays nil)
;;   (holiday-oriental-holidays nil))

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode +1))

;; This package converts the buffer text and the associated decorations to HTML.
;; This is necessary for exporting Org files to HTML.
(use-package htmlize
  :defer t)

(use-feature ielm
  :custom
  (ielm-history-file-name (my-expand-var-file ("ielm-history.eld")))
  :general
  (global-leader
    :major-modes '(inferior-emacs-lisp-mode)
    :keymaps     '(inferior-emacs-lisp-mode-map)
    "b"  '(:ignore t :which-key "buffer")
    "bb" 'ielm-change-working-buffer
    "bd" 'ielm-display-working-buffer
    "bp" 'ielm-print-working-buffer
    "c"  'comint-clear-buffer)
  ;;@TODO: fix this command.
  ;;This should be easier
  (+general-global-application "i"
    '("ielm" . (lambda ()
                 (interactive)
                 (let* ((b (current-buffer))
                        (i (format "*ielm<%s>*" b)))
                   (setq ielm-prompt (concat (buffer-name b) ">"))
                   (ielm i)
                   (ielm-change-working-buffer b)
                   (next-buffer)
                   (switch-to-buffer-other-window i))))))

;; fix emoji without  VARIATION SELECTOR-16
(use-feature fontset
  :config
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
  )

;; modern replacement for command-log-mode and view-lossage
;; SPC-h-l надо

(use-package keycast
  :defer t)

(use-package js2-mode
  :disabled t
  :commands (js2-mode)
  :mode "\\.js\\'"
  :interpreter (("nodejs" . js2-mode) ("node" . js2-mode)))

(use-package magit
  :ensure (magit :host github :repo "magit/magit")
  :defer t
  :after (general)
  :custom
  (magit-repository-directories (list (cons elpaca-repos-directory 1)))
  (magit-diff-refine-hunk 'all)
  :general
  (+general-global-git/version-control
    "g"  '(magit-status :wk "Magit status")
    "G"  'magit-status-here
    "b"  'magit-branch
    "B"  'magit-blame
    "c"  'magit-clone
    "f"  '(:ignore t :which-key "file")
    "ff" 'magit-find-file
    "fh" 'magit-log-buffer-file
    "i"  'magit-init
    "L"  'magit-list-repositories
    "m"  'magit-dispatch
    "S"  'magit-stage-file
    "s"  'magit-status
    "U"  'magit-unstage-file)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun my/magit-insert-user-info ()
  "Insert current git user info in magit status buffer."
  (when-let ((email (magit-get "user.email"))
             (name (magit-get "user.name")))
    (magit-insert-section (user-info)
      (magit-insert-heading "User Info:")
      (insert (format "  Name:  %s\n" name))
      (insert (format "  Email: %s\n" email))
      (insert ?\n))))

  (add-hook 'magit-status-sections-hook #'my/magit-insert-user-info t)
  (transient-bind-q-to-quit))

(use-package llama
  :ensure (llama :host github :repo "tarsius/llama" )
  :defer t)

(use-package transient
  :defer t
  :ensure(transient :host github :repo "magit/transient")
  :custom
  (transient-history-file (my-expand-var-file "transient/history.el")))

(use-package forge
  :ensure (forge :host github :repo "magit/forge")
  :after magit
  :custom
  (forge-database-file (my-expand-var-file "forge/database.sqlite"))
  :init
  (setq forge-add-default-bindings nil
              forge-display-in-status-buffer nil
              forge-add-pullreq-refspec nil)
  (setq forge-owned-accounts '(("snakejke")))
  )

(use-package with-editor
  :ensure (with-editor :host github :repo  "magit/with-editor")
  :defer t)

(use-package ghub
  :ensure (ghub :host github :repo  "magit/ghub")
  :defer t)

(use-package macrostep
  :general
  (global-leader
    :major-modes '(emacs-lisp-mode lisp-interaction-mode t)
    :keymaps     '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "m"  '(:ignore t :which-key "macrostep")
    "me" 'macrostep-expand
    "mc" 'macrostep-collapse
    "mj" 'macrostep-next-macro
    "mk" 'macrostep-prev-macro))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "/usr/bin/pandoc")
  :init
  (defun my-markdown-mode-hook()
      (visual-line-mode 1))
    (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
  )

(use-feature minibuffer
  :custom (read-file-name-completion-ignore-case t)
  :config
  (defun +minibuffer-up-dir ()
    "Trim rightmost directory component of `minibuffer-contents'."
    (interactive)
    (unless (minibufferp) (user-error "Minibuffer not selected"))
    (let* ((f (directory-file-name (minibuffer-contents)))
           (s (file-name-directory f)))
      (delete-minibuffer-contents)
      (when s (insert s))))
  (define-key minibuffer-local-filename-completion-map
              (kbd "C-h") #'+minibuffer-up-dir)
  (minibuffer-depth-indicate-mode))

(use-package nov
  :ensure (nov :depth nil)
  :custom
  (nov-text-width 80)
  :mode
  ("\\.epub\\'" . nov-mode)
  :commands
  (nov-mode))

(use-feature notmuch
  :load-path ("/usr/share/emacs/site-lisp/notmuch")
  :commands (notmuch)
  :defer t
  :init
  (setq notmuch-search-oldest-first nil
        message-send-mail-function 'message-send-mail-with-sendmail
        notmuch-always-prompt-for-sender t
        message-kill-buffer-on-exit t
        notmuch-hello-logo nil
        notmuch-hello-url nil
        notmuch-show-relative-dates t
        notmuch-show-empty-saved-searches t
        message-directory "~/Mail"
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        notmuch-show-all-tags-list t
        mail-specify-envelope-from t
        ;; '(nil . t) means any charset is allowed without re-encoding.
        ;; The cdr must be the symbol t (not a list containing t) because
        ;; mm-body-encoding checks (eq t (cdr message-posting-charset));
        ;; '(nil t) gives cdr=(t) which fails that test and causes utf-8
        ;; bodies to be base64-encoded instead of kept as 8bit.
        message-posting-charset '(nil . t))
  (setq notmuch-saved-searches
        `(( :name "📥 Входящие"
            :query "tag:inbox and tag:unread"
            :key "i")
          ( :name "🛫 Отправленные"
            :query "tag:sent"
            :key "t")
          ( :name "📝 Черновики"
            :query "tag:draft"
            :key "d")
          ( :name "📦 Все письма"
            :query "*"
            :key "a")))
  :config
  (my/evil-collection-override evil-collection-notmuch notmuch-search-mode
  ((normal visual) "RET" #'my/notmuch-search-maybe-resume-draft))

  ;; (with-eval-after-load 'evil-collection-notmuch
  ;; (add-hook 'notmuch-search-mode-hook
  ;;   (lambda ()
  ;;     (evil-local-set-key 'normal (kbd "RET") #'my/notmuch-search-maybe-resume-draft)
  ;;     (evil-local-set-key 'visual (kbd "RET") #'my/notmuch-search-maybe-resume-draft))
  ;;   90))
;; (message "notmuch :config runs at: %s" (current-time-string))
;; (with-eval-after-load 'evil-collection-notmuch
;;   (message "evil-collection-notmuch loaded at: %s" (current-time-string))
;;   (evil-define-key* '(normal visual) notmuch-search-mode-map
;;     (kbd "RET") #'my/notmuch-search-maybe-resume-draft)
;;   (message "RET binding in auxiliary keymap after evil-define-key*: %s"
;;     (lookup-key (evil-get-auxiliary-keymap notmuch-search-mode-map 'normal t)
;;                 (kbd "RET"))))

  
   ;; (with-eval-after-load 'evil-collection-notmuch
   ;;  (evil-define-key* '(normal visual) notmuch-search-mode-map
   ;;    (kbd "RET") #'my/notmuch-search-maybe-resume-draft))
  ;; (evil-define-key* '(normal visual) notmuch-search-mode-map
  ;; (kbd "RET") #'my/notmuch-search-maybe-resume-draft)

  ;; (evil-define-key '(normal visual) notmuch-search-mode-map
  ;;   (kbd "RET") #'my/notmuch-search-maybe-resume-draft
  ;;   (kbd "t")   #'my/notmuch-toggle-trash)
  
  (defun my/notmuch-message-recovery-file-p (file)
    "Return non-nil when FILE is a Message auto-save draft file."
    (and (stringp file)
         (string-match-p
          "\\`\\(?:\\*message\\*\\|message\\)-[0-9]\\{8\\}-[0-9]\\{6\\}\\'"
          (file-name-nondirectory file))))

  (defun my/notmuch-delete-message-recovery-file (file)
    "Delete FILE when it is a Message auto-save draft file."
    (when (and (my/notmuch-message-recovery-file-p file)
               (file-exists-p file))
      (delete-file file)))

  (defun my/notmuch-message-save-buffer ()
    "Save a Notmuch compose buffer through `notmuch-draft-save'."
    (when (derived-mode-p 'notmuch-message-mode)
      (notmuch-draft-save)
      t))

  (defun my/notmuch-draft-save-around (orig-fun &rest args)
    "Run ORIG-FUN and remove the Message recovery file after a good save."
    (setq-local message-encoded-mail-cache nil)
    (let ((old-file buffer-file-name))
      (cl-letf (((symbol-function 'mail-encode-encoded-word-buffer) #'ignore))
        ;; Skip RFC-2047 header encoding: headers are stored as raw UTF-8.
        ;; notmuch-draft-resume re-decodes them via my/notmuch-draft-resume-fix-headers.
        (prog1 (apply orig-fun args)
          (my/notmuch-delete-message-recovery-file old-file)))))

  (defun my/notmuch-draft-resume-fix-headers ()
    "Decode raw UTF-8 bytes in headers left by no-conversion read."
    (save-restriction
      (message-narrow-to-headers)
      (decode-coding-region (point-min) (point-max) 'utf-8)))

  (advice-add 'notmuch-draft-resume :after
              (lambda (&rest _) (my/notmuch-draft-resume-fix-headers)))

  (advice-add 'notmuch-draft-save :around #'my/notmuch-draft-save-around)

  (defun my/notmuch-message-install-save-handler ()
    "Make generic Emacs save commands store Notmuch drafts correctly."
    (add-hook 'write-contents-functions #'my/notmuch-message-save-buffer nil t))

  (add-hook 'notmuch-message-mode-hook #'my/notmuch-message-install-save-handler)

  (defun my/notmuch-search-resume-draft ()
    "Resume editing the draft at point in notmuch-search."
    (interactive)
    (let ((thread-id (notmuch-search-find-thread-id)))
      (if (null thread-id)
          (message "No thread at point")
        (let ((msg-id (car (notmuch--process-lines
                            notmuch-command "search"
                            "--output=messages" "--exclude=false"
                            thread-id))))
          (if msg-id
              (notmuch-draft-resume msg-id)
            (message "No message found in thread"))))))

  (defun my/notmuch-search-maybe-resume-draft ()
    "Open draft for editing if current search is the drafts folder, else show normally."
    (interactive)
    (if (and notmuch-search-query-string
             (string-match-p "tag:draft" notmuch-search-query-string))
        (my/notmuch-search-resume-draft)
      (notmuch-search-show-thread)))

  ;; (define-key notmuch-search-mode-map (kbd "RET")
  ;;             #'my/notmuch-search-maybe-resume-draft)
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key '(normal visual) notmuch-search-mode-map (kbd "RET")
  ;;     #'my/notmuch-search-maybe-resume-draft))
  
  (defun my/notmuch-toggle-trash ()
    (interactive)
    (evil-collection-notmuch-toggle-tag "trash" "search" #'ignore)))

(use-package wgrep
  :disabled t
  ;;:defer t
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(use-feature whitespace
  :custom
  ;; (whitespace-display-mappings '((tab-mark ?\t [?› ?\t])))
      (whitespace-display-mappings '(
      (space-mark   ?\     [?\u00B7]     [?.])
      (space-mark   ?\xA0  [?\u00A4]     [?_])
      (newline-mark ?\n    [182 ?\n])
      (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-line-column nil)
  ;; (whitespace-style '(empty face lines-tail tab-mark tabs trailing))
  )

(use-feature xref
;    :bind
;   ;; Mimic VSCode
;;   ("s-<mouse-1>" . xref-find-definitions-at-mouse)
  :custom
  (xref-search-program 'ripgrep)
  )

(use-feature novice
  :custom
  (disabled-command-function nil "Enable all commands"))

;; (eval-when-compile
;;   (require 'extras))
(use-feature extras
  :load-path "~/.config/emacs/lisp/"
  :commands (sudo-edit
             my/convert-org-to-docx-with-pandoc
             +unfill-region
             org-mouse-bold-mode-enable
             org-mouse-bold-mode-disable))

;; (add-hook 'elpaca-after-init-hook
;;   (lambda ()
;;     (advice-remove 'require 'my/require-timer)
;;     (let ((sorted (sort my/require-log
;;                         (lambda (a b)
;;                           (> (float-time (cdr a))
;;                              (float-time (cdr b)))))))
;;       (with-current-buffer (get-buffer-create "*require-log*")
;;         (erase-buffer)
;;         (insert (format "%-40s %s\n" "FEATURE" "TIME(s)"))
;;         (insert (make-string 50 ?-) "\n")
;;         (dolist (entry sorted)
;;           (insert (format "%-40s %.4f\n"
;;                           (car entry)
;;                           (float-time (cdr entry)))))
;;         (display-buffer (current-buffer))))))

(provide 'init)
;;; init.el ends here
