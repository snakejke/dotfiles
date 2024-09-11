;; -*- lexical-binding: t; -*-

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

(setq initial-buffer-choice t) ;;*scratch*

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(setq elpaca-queue-limit 30)

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

(let ((additional-paths '("/usr/share/emacs/site-lisp/notmuch"))) 
  (mapc (lambda (path)
          (when (file-directory-p path) 
            (add-to-list 'load-path path)))
        additional-paths))

(setq literate-file (concat user-emacs-directory "init.org"))

(defun +terminal ()
  "Set the terimnal coding system."
  (unless (display-graphic-p)
    (set-terminal-coding-system 'utf-8)))

(add-hook 'server-after-make-frame-hook #'+terminal)

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

(defmacro +general-global-menu! (name prefix-key &rest body)
  "Create a definer named +general-global-NAME wrapping global-definer.
  Create prefix map: +general-global-NAME-map. Prefix bindings in BODY with PREFIX-KEY."
  (declare (indent 2))
  (let* ((n (concat "+general-global-" name))
         (prefix-map (intern (concat n "-map"))))
    `(progn
       (general-create-definer ,(intern n)
         :wrapping global-definer
         :prefix-map (quote ,prefix-map)
         :prefix ,prefix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern n) ,@body))))

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
  "e"  'eshell
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

(+general-global-menu! "link" "l")

(+general-global-menu! "narrow" "n"
  "d" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(+general-global-menu! "project" "p"
   ;;"b" '(:ignore t :which-key "buffer")
  ;; "!"
  ;; "&"
  ;; "."
  ;; ">"
  ;; "a"
  ;; "b"
  ;; "c"
  ;; "C"
  ;; "d"
  ;; "D"
  ;; "e"
  ;; "F"
  ;; "g"
  ;; "i"
  ;; "k"
  ;; "o"
  "p" 'project-switch-project
  ;; "r"
  ;; "R"
  ;; "s"
  ;; "t"
  ;; "T"
  ;; "x"
  ;; "X"
  )

(+general-global-menu! "quit" "q"
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs
  "d" 'delete-frame
  "Q" 'kill-emacs)

(+general-global-menu! "searchhh" "s"
  "b" 'consult-line
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
  "x" 'kill-buffer-and-window)

;;vim-like completion
(general-create-definer completion-def
  :prefix "C-x")
)

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil) ;; бинды с evil-collection
  :custom
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

  (evil-mode)
)

(use-package evil-collection
  :ensure (:remotes ("origin"
                      ("fork" :repo "progfolio/evil-collection")))
  :after (evil)
  :config (evil-collection-init)
  :init
  (setq evil-collection-setup-minibuffer nil) ;; связано с (evil-want-minibuffer nil)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-ement-want-auto-retro t))

(use-package evil-anzu
  :after (evil anzu))

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
  (use-short-answers t) ;; yes-no to y-n
  (completion-styles '(flex basic partial-completion emacs22))
  ;; c corfu. если не будет работать. положить в init
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  :init
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  ;; don't want ESC as a modifier. 
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; переделать
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  

  ;; (setq display-buffer-alist
  ;;       `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
  ;;          (display-buffer-reuse-mode-window display-buffer-below-selected)
  ;;          (window-height . 0.33)
  ;;          (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))
  
  )

;;(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package which-key
  :demand t
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  ;;(which-key-idle-delay 0.2)
  ;;(which-key-idle-delay 1)
  )

;;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'
(defconst my-local-dir (concat user-emacs-directory ".local/"))

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
  ;; :config
  ;; (electric-pair-mode t)
  )

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

(use-package modus-themes
  :config 
  (setq modus-themes-custom-auto-reload nil
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t ;; что-то компактнее стало ? 
      modus-themes-italic-constructs t
      modus-themes-prompts '(bold intense)
      modus-themes-completions '((t . (extrabold)))
      modus-themes-headings
      '((0 . (variable-pitch 1))
        (t . (variable-pitch 1)) 

    ))
    ;;fonts
    (set-face-attribute 'default nil :height 190)
    (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif" :height 1.0 :weight 'medium)
    (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
    
    (setq modus-themes-common-palette-overrides
      '(
        (fringe unspecified)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)

        (constant "#bcbec4")
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
        )
      )
    ;; Make line numbers less intense
;; (setq modus-themes-common-palette-overrides
;;       '((fg-line-number-inactive "gray50")
;;         (fg-line-number-active fg-main)
;;         (bg-line-number-inactive unspecified)
;;         (bg-line-number-active unspecified)))
;;TODO
(setq modus-themes-vivendi-palette-overrides
      '((fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)))
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

    (setq modus-vivendi-palette-overrides
        '((bg-main  "#1e1f22");;idea
          (fg-main "#bcbec4")
         )) ;; idea
    (load-theme 'modus-vivendi t))

    ;;  (setq modus-themes-common-palette-overrides '((constant "#bcbec4")))
        ;;;;; font-lock
    ;; `(font-lock-builtin-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    ;; `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    ;; `(font-lock-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    ;; `(font-lock-constant-face ((,c :foreground ,constant)))
    ;; `(font-lock-doc-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    ;; `(font-lock-doc-markup-face ((,c :inherit modus-themes-slant :foreground ,docmarkup)))
    ;; `(font-lock-function-name-face ((,c :foreground ,fnname)))
    ;; `(font-lock-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    ;; `(font-lock-negation-char-face ((,c :inherit error)))
    ;; `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
    ;; `(font-lock-regexp-grouping-backslash ((,c :inherit modus-themes-bold :foreground ,rx-backslash)))
    ;; `(font-lock-regexp-grouping-construct ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
    ;; `(font-lock-string-face ((,c :foreground ,string)))
    ;; `(font-lock-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
    ;; `(font-lock-variable-name-face ((,c :foreground ,variable)))
    ;; `(font-lock-warning-face ((,c :inherit modus-themes-bold :foreground ,warning)))

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
  :hook (dired-mode-hook . dired-hide-details-mode)
  :config
  ;;(setq dired-omit-files (rx (seq bol ".")))
  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (when (featurep :system 'bsd)
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " ")))
)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode)
;;   :config
;;   (defun +dired-disable-icons-in-wdired-mode-a (&rest _)
;;     "Disable nerd-icons-dired-mode in wdired-mode."
;;     (when (eq major-mode 'wdired-mode)
;;       (setq-local +wdired-icons-enabled (if nerd-icons-dired-mode 1 -1))
;;       (when nerd-icons-dired-mode
;;         (nerd-icons-dired-mode -1))))

;;   (defun +dired-restore-icons-after-wdired-mode-a (&rest _)
;;     "Restore nerd-icons-dired-mode after leaving wdired-mode."
;;     (when (eq major-mode 'dired-mode)
;;       (when (and nerd-icons-dired-mode (boundp '+wdired-icons-enabled))
;;         (nerd-icons-dired-mode +wdired-icons-enabled))))
;; (advice-add 'wdired-change-to-wdired-mode :before #'+dired-disable-icons-in-wdired-mode-a)
;; (advice-add 'wdired-change-to-dired-mode :after #'+dired-restore-icons-after-wdired-mode-a)
)

(use-feature dired-aux
  :defer t)

(use-feature dired-x
 :after dired
 :hook (dired-mode . dired-omit-mode))

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 1 ;; default 0
      auto-window-vscroll nil
      scroll-preserve-screen-position t)
(pixel-scroll-precision-mode t)

(use-feature simple
  :general
  (+general-global-toggle
    "f" 'auto-fill-mode)
  :custom
  (eval-expression-debug-on-error nil)
  (fill-column 80 "Wrap at 80 columns."))

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

          helpful-mode
          help-mode
          flymake-diagnostics-buffer-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-feature autorevert
  :defer 2
  :custom
  (auto-revert-interval 0.01 "Instantaneously revert")
  :config
  (global-auto-revert-mode t))

(use-package anki-editor
  :ensure (anki-editor :host github :repo "orgtre/anki-editor")
  ;;orgtre/anki-editor
  ;;anki-editor/anki-editor
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

(use-package sqlite3
  :ensure (sqlite3 :host github :repo "pekingduck/emacs-sqlite3-api")
  :defer t)

(use-package leetcode
  :ensure (leetcode :host github :repo "kaiwk/leetcode.el" :files ("leetcode.el"))
  :defer t
  :custom
  (leetcode-prefer-language "java"))

(use-package elcord
  ;;:commands elcord 
  :config
  (setq elcord-use-major-mode-as-main-icon t)
  (setq elcord-display-buffer-details nil)
  (setq elcord-idle-message "Thinking 🤔")
  (setq elcord-quiet t)
  (elcord-mode))

(use-package telega
  :commands (telega)
  :defer t)

(use-feature bookmark
  :custom (bookmark-fontify nil)
  :general
  (+general-global-bookmark
    "j" 'bookmark-jump
    "s" 'bookmark-set
    "r" 'bookmark-rename))

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

(use-package anzu
  :defer 10
  :config (global-anzu-mode))

(use-feature files
  ;;:hook
  ;;(before-save . delete-trailing-whitespace)
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
  :custom
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
   "Store safe local variables here instead of in emacs-custom.el"))

(auto-save-visited-mode 1)
;; (setq auto-save-visited-interval 15) ;default is 5s
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

;; Temp files (save-place, recenf, undo-tree)
(defconst my-temp (expand-file-name "my-temp" user-emacs-directory))
(unless (file-exists-p my-temp)
  (make-directory my-temp))
(setq save-place-file (expand-file-name "saveplace" my-temp))
(save-place-mode 1)

(use-package undo-fu
  :defer t)

(use-package undo-fu-session
  :defer t
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (global-undo-fu-session-mode 1))

(use-package vundo
  :bind (("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-roll-back-on-quit nil))

(use-feature savehist
  :defer 1
  :config
  (savehist-mode 1))

(defface evil-state-face
  '((t (:weight bold)))
  "Bold"
  )

(defface evil-normal-face
    '((t (:inherit evil-state-face 
        :background "#ff5f5f"
        :foreground "white")))
    "White")
(defface evil-emacs-face
  '((t (:inherit evil-state-face
			:background "#3366ff"
			:foreground "white")))
  "The evil emacs state "
  )

(defface evil-insert-face
  '((t (:inherit evil-state-face
			:background "#3399ff"
			:foreground "white")))
  "The evil insert state"
  )

(defface evil-replace-face
  '((t (:inherit evil-state-face
			:background "#33ff99"
			:foreground "black")))
  "The evil replace state"
  )

(defface evil-operator-face
  '((t (:inherit evil-state-face
			:background "pink"
			:foreground "black")))
  "The evil operator state"
  )

(defface evil-motion-face
  '((t (:inherit evil-state-face
			:background "purple"
			:foreground "white")))
  "The evil motion state"
  )

(defface evil-visual-face
  '((t (:inherit (region evil-state-face))))
  "The evil visual state"
  )

(defun my-evil nil
     (let ((state (if (bound-and-true-p evil-state)
                     (symbol-name evil-state)
                    " ")))
     (propertize (concat " " (upcase state) " ") 'face (intern (format "evil-%s-face" state)))))

(setq-default mode-line-format '((:eval (my-evil))
    ""
     ("%e" mode-line-front-space
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
      display (min-width (5.0)))
     mode-line-frame-identification
     mode-line-buffer-identification "   "
     mode-line-position
     (project-mode-line project-mode-line-format) (vc-mode vc-mode) "  "
     minions-mode-line-modes
     mode-line-misc-info
     mode-line-frame-identification
     mode-line-end-spaces)))

(use-package minions
  :custom
  (minions-prominent-modes '(flymake-mode))
  :config
  (setq minions-mode-line-lighter "  "
        minions-mode-line-delimiters '("" . ""))
  
  (minions-mode 1))

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
  ;;:general
  ;;        (   "C-j" 'vertico-next
  ;;           "C-k" 'vertico-previous
  ;;           "<escape>" #'minibuffer-keyboard-quit 
  ;;           "M-<backspace>" 'vertico-directory-delete-word)

  ;; (:keymaps 'vertico-map
  ;;           "C-j" 'vertico-next
  ;;           "C-k" 'vertico-previous
  ;;           "<escape>" 'minibuffer-keyboard-quit 
  ;;           "M-<backspace>" 'vertico-directory-delete-word
  ;;           )
  ;; (general-define-key :states '(normal) :keymaps 'vertico-map
  ;;                      (kbd "<escape>") #'minibuffer-keyboard-quit
  ;;                      )
  ;; (global-leader
  ;;   :keymaps '(vertico-map)
  ;;   "<escape>" 'minibuffer-keyboard-quit)


  :config
  (setf (car vertico-multiline) "\n") ;; don't replace newlines
  (vertico-mode)
  ;; в офф доках такое есть
  ;; (setq minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (define-key vertico-map (kbd "C-h") #'+minibuffer-up-dir)
)

(use-package orderless
  :defer 1
  ;;:custom (completion-styles '(orderless basic)))
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
  :defer 2
  :config (marginalia-mode))

(use-package consult
  :demand t
  :config
  (consult-customize
   consult-recent-file
   consult--source-recent-file
   consult--source-buffer
   :preview-key nil)
  (define-key evil-normal-state-map (kbd "gb") 'consult-buffer)
  (define-key evil-visual-state-map (kbd "gb") 'consult-buffer)

  :general
  (+general-global-buffer
    "b" 'consult-buffer)
  :init

;; Use Consult to select xref locations with preview
;; получше чем дефолт
  ;; definitions хм 
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

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :defer 5
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  ;;(corfu-seperator ?-)
  (corfu-seperator ?\s)
  :config
  (global-corfu-mode)
  ;; не ясно как это работает. 
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point))))
  )

(use-feature corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popinfo-delay '(0.5 . 1.0)))

(use-package cape
  :general (:prefix "M-c"               ; Particular completion function
            "p" 'completion-at-point
            "t" 'complete-tag           ; etags
            "d" 'cape-dabbrev           ; or dabbrev-completion
            "f" 'cape-file
            "k" 'cape-keyword
            "s" 'cape-symbol
            "a" 'cape-abbrev
            "i" 'cape-ispell
            "l" 'cape-line
            "w" 'cape-dict
            "\\"' cape-tex
            "_" 'cape-tex
            "^" 'cape-tex
            "&" 'cape-sgml
            "r" 'cape-rfc1345
            )
  :custom
  (cape-dabbrev-min-length 3)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

(use-package eglot
  :hook
  (
   ;;(python-mode . eglot-ensure)
   ;;(c-mode . eglot-ensure)
   ;;(c++-mode . eglot-ensure)
   ;(java-ts-mode . eglot-ensure)
   (scala . eglot-ensure
   ))
  :custom
   ;(eglot-ignored-server-capabilities '(:documentLinkProvider
   ;:documentOnTypeFormattingProvider))
  (eglot-autoshutdown t)
  :config
  ;; (add-to-list 'eglot-server-programs '(kotlin-ts-mode "/home/snake/kotlin-language-server/server/build/install/server/bin/kotlin-language-server"))

  (cl-callf plist-put eglot-events-buffer-config :size 0)
  :init
  (defun +eglot-register (modes &rest servers)
    "Register MODES with LSP SERVERS.
     Examples:
     (+eglot-register 'vhdl-mode \"vhdl_ls\")
     (+eglot-register 'lua-mode \"lua-language-server\" \"lua-lsp\")
     (+eglot-register '(c-mode c++-mode) '(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")"
    (declare (indent 0))
    (with-eval-after-load 'eglot
      (add-to-list
       'eglot-server-programs
       (cons modes (if (length> servers 1)
                       (eglot-alternatives (ensure-list servers))
                     (ensure-list (car servers)))))))
  )

(use-package eglot-java
  :ensure (eglot-java :host github :repo "yveszoundi/eglot-java" :files (:defaults "*.el"))
  :custom
  (eglot-java-eclipse-jdt-args
   '("-XX:+UseAdaptiveSizePolicy"
     "-XX:GCTimeRatio=4"
     "-XX:AdaptiveSizePolicyWeight=90"
     "-Xmx8G"
     "-Xms2G"
     ))
  :config
  (defun eglot-java-run-main-fork ()
    "Run a main class."
    (interactive)
    (let* ((fqcn (eglot-java--class-fqcn))
           (cp   (eglot-java--project-classpath (buffer-file-name) "runtime")))
      (if fqcn
          (compile
           (concat "java -cp "
                   (mapconcat #'identity cp path-separator)
                   " "
                   fqcn)
           t)
        (user-error "No main method found in this file! Is the file saved?!"))))
  :hook (java-ts-mode . eglot-java-mode)
  )

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

;; (use-feature java-ts-mode
;;   :mode "\\.java\\'")

(use-package groovy-mode 
:mode (("build\\.gradle" . groovy-mode)
       ("Jenkinsfile" . groovy-mode))
:config
(+eglot-register '(groovy-mode) "groovy-language-server"))

(use-package kotlin-ts-mode 
  :ensure (kotlin-ts-mode :host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kts?m?\\'")

(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(
       ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres password=root dbname=testdb sslmode=disable"))
      ))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t))

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package jsonrpc)

(use-feature nxml-mode
  :mode "\\.xml\\'"
  :config
  (+eglot-register '(nxml-mode xml-mode) "lemminx"))

(use-package racket-mode
  :defer t
  :hook (racket-mode . racket-xp-mode)
 ;;   (define-key racket-mode-map (kbd "<up>") (kbd "M-p"))
 ;; (define-key racket-mode-map (kbd "<down>") (kbd "M-n"))
  )

(use-package geiser
  :defer t
  
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-chez
  :defer t
  )

(use-package geiser-guile
  :defer t
  )

(use-package geiser-mit
  :defer t
  )

;(use-package geiser-racket
;  :defer t
;  )

(use-package macrostep-geiser
  :after (geiser)
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup)
  ;; :init
  ;; (+map-local! :keymaps '(geiser-mode-map geiser-repl-mode-map)
  ;;   "m" '(macrostep-expand :wk "Expand macro")
  ;;   "M" #'macrostep-geiser-expand-all)
  )

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

;;(use-package sly-macrostep )

(use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-overlay
  :defer t)

(use-package scala-mode
  :defer t
  :interpreter ("scala" . scala-mode))

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

(use-package ejc-sql
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

(use-package flycheck-package
  :after (flychceck)
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

(use-package fvwm-mode
  :ensure (fvwm-mode :host github :repo "theBlackDragon/fvwm-mode")
  :defer t
  :commands fvwm-mode )

(use-feature ispell
  :config
  (setq ispell-alternate-dictionary (file-truename "~/.config/emacs/dict/english-words.txt")))

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

(use-package treemacs-magit
  :after (treemacs magit)
  )

(use-package org
  :ensure (:autoloads "org-loaddefs.el")
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))

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
    "oC"  '+org-capture-again
    "oi"  'org-insert-link
    "oj"  'org-chronicle
    "ok"  '(:ignore t :which-key "clock")
    "okg" 'org-clock-goto
    "oki" 'org-clock-in-last
    "okj" 'org-clock-jump-to-current-clock
    "oko" 'org-clock-out
    "okr" 'org-resolve-clocks
    "ol"  'org-store-link
    "om"  'org-tags-view
    ;;"os"  'org-search-view
    "oT"  'org-todo-list
    "ot"  '(:ignore t :which-key "timer")
    "ott" 'org-timer
    "otS" 'org-timer-stop
    "otC" 'org-timer-change-times-in-region
    "otc" 'org-timer-set-timer
    "ots" 'org-timer-start
    "oti" 'org-timer-item
    "otp" 'org-timer-pause-or-continue
    "otr" 'org-timer-show-remaining-time)

  (global-leader
    ;;for terminals
    :keymaps '(org-mode-map)
    "TAB" 'org-cycle
    "."  'org-time-stamp
    "!"  'org-time-stamp-inactive
    "<"  'org-date-from-calendar
    ">"  'org-goto-calendar

    "C"  '(:ignore t :which-key "clock")
    "Cc" 'org-clock-cancel
    "Ci" 'org-clock-in
    "Co" 'org-clock-out
    "Cr" 'org-clock-report
    "CR" 'org-resolve-clocks

    "d"  '(:ignore t :which-key "dates")
    "dd" 'org-deadline
    "df" '((lambda () (interactive) (+org-fix-close-times))
           :which-key "org-fix-close-time")
    "ds" 'org-schedule
    "di" 'org-time-stamp-inactive
    "dt" 'org-time-stamp

    "e"   '(:ignore t :which-key "export")
    "ee"  'org-export-dispatch

    "h"   '(:ignore t :which-key "heading")
    "hf"  'org-forward-heading-same-level
    "hb"  'org-backward-heading-same-level

    "i"  '(:ignore t :which-key "insert")
    "id" 'org-insert-drawer
    "ie" 'org-set-effort
    "if" 'org-footnote-new
    "iH" 'org-insert-heading-after-current
    "ih" 'org-insert-heading
    "ii" 'org-insert-item
    "in" 'org-add-note
    "ip" 'org-set-property
    "is" 'org-insert-structure-template
    "it" 'org-set-tags-command

    "l" '(:ignore t :which-key "links")
    "lc" 'org-cliplink
    "ll" 'org-insert-link

    "n"  '(:ignore t :which-key "narrow")
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element
    "ns" 'org-narrow-to-subtree
    "nt" 'org-toggle-narrow-to-subtree
    "nw" 'widen

    "s"  '(:ignore t :which-key "trees/subtrees")
    "sA" 'org-archive-subtree
    "sa" 'org-toggle-archive-tag
    "sb" 'org-tree-to-indirect-buffer
    "sc" 'org-cut-subtree
    "sh" 'org-promote-subtree
    "sj" 'org-move-subtree-down
    "sk" 'org-move-subtree-up
    "sl" 'org-demote-subtree
    "sp" '(:ignore t :which-key "priority")
    "spu" 'org-priority-up
    "spd" 'org-priority-down
    "sps" 'org-priority-show
    "sm" 'org-match-sparse-tree
    "sn" 'org-toggle-narrow-to-subtree
    "sr" 'org-refile
    "sS" 'org-sort
    "ss" '+org-sparse-tree

    "t"   '(:ignore t :which-key "tables")
    "ta"  'org-table-align
    "tb"  'org-table-blank-field
    "tc"  'org-table-convert

    "td"  '(:ignore t :which-key "delete")
    "tdc" 'org-table-delete-column
    "tdr" 'org-table-kill-row
    "tE"  'org-table-export
    "te"  'org-table-eval-formula
    "tH"  'org-table-move-column-left
    "th"  'org-table-previous-field
    "tI"  'org-table-import

    "ti"  '(:ignore t :which-key "insert")
    "tic" 'org-table-insert-column
    "tih" 'org-table-insert-hline
    "tiH" 'org-table-hline-and-move
    "tir" 'org-table-insert-row
    "tJ"  'org-table-move-row-down
    "tj"  'org-table-next-row
    "tK"  'org-table-move-row-up
    "tL"  'org-table-move-column-right
    "tl"  'org-table-next-field
    "tN"  'org-table-create-with-table.el
    "tn"  'org-table-create
    "tp"  'org-plot/gnuplot
    "tr"  'org-table-recalculate
    "ts"  'org-table-sort-lines

    "tt"  '(:ignore t :which-key "toggle")
    "ttf" 'org-table-toggle-formula-debugger
    "tto" 'org-table-toggle-coordinate-overlays
    "tw"  'org-table-wrap-region

    "T"  '(:ignore t :which-key "toggle")
    "Tc"  'org-toggle-checkbox
    "Te"  'org-toggle-pretty-entities
    "TE"  '+org-toggle-hide-emphasis-markers
    "Th"  'org-toggle-heading
    "Ti"  'org-toggle-item
    "TI"  'org-toggle-inline-images
    "Tl"  'org-toggle-link-display
    "TT"  'org-todo
    "Tt"  'org-show-todo-tree
    "Tx"  'org-latex-preview
    ;;"RET" 'org-ctrl-c-ret
    "RET" 'org-meta-return
    "#"   'org-update-statistics-cookies
    "'"   'org-edit-special
    "*"   'org-ctrl-c-star
    "-"   'org-ctrl-c-minus
    "A"   'org-attach)
  :config
  (setq org-tags-column -120) ;; так лучше 
  (setq org-link-frame-setup '((file . find-file))) ;; в org-ref это по дефолту
  (setq org-fontify-quote-and-verse-blocks t) ;; шрифт в comment и quote блоках. Почему в custom не работает ? 

  (defun +md-to-org-region (start end)
    "Convert region from markdown to org, replacing selection"
    (interactive "r")
    (shell-command-on-region start end "pandoc -f markdown -t org" t t))

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

  ;; Offered a patch to fix this upstream. Too much bikeshedding for such a simple fix.
  (defun +org-tags-crm (fn &rest args)
    "Workaround for bug which excludes \",\" when reading tags via `completing-read-multiple'.
  I offered a patch to fix this, but it was met with too much resistance to be
  worth pursuing."
    (let ((crm-separator "\\(?:[[:space:]]*[,:][[:space:]]*\\)"))
      (unwind-protect (apply fn args)
        (advice-remove #'completing-read-multiple #'+org-tags-crm))))

  (define-advice org-set-tags-command (:around (fn &rest args) comma-for-crm)
    (advice-add #'completing-read-multiple :around #'+org-tags-crm)
    (apply fn args))

  (add-to-list 'org-emphasis-alist
             '("*" (bold :foreground "#f1e00a")
               ))

  :custom
  ;;default:
  ;;(org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)
  ;;org-toc is interesting, but I'm not sure if I need it.

  (org-modules '(org-habit))
  ;; 
  (org-todo-keywords
   ;; '((sequence  "TODO(t)" "STARTED(s!)" "NEXT(n!)" "BLOCKED(b@/!)" "|" "DONE(d)")
   ;;   (sequence  "IDEA(i)" "|" "CANCELED(c@/!)" "DELEGATED(D@/!)")
   ;;   (sequence  "RESEARCH(r)" "|"))
   '((sequence  "TODO(t)" "DONE(d)" )
     (sequence "DEPRECATED(o)"))
   ;;move to theme?
   org-todo-keyword-faces
   `(("CANCELED" . (:foreground "IndianRed1" :weight bold))
     ("TODO" . (:foreground "#ffddaa"
                            :weight bold
                            :background "#202020"
                            :box (:line-width 3 :width -2 :style released-button)))
     ("DEPRECATED" . (:foreground "yellow-faint" :weight bold))
     ))
  (org-ellipsis (nth 5 '("↴" "˅" "…" " ⬙" " ▽" "▿")))
  (org-priority-lowest ?D)
  (org-priority-faces '((?A . nerd-icons-red)
                        (?B . warning)
                        (?C . success)))
  (org-fontify-done-headline t)
  (org-insert-heading-respect-content t) ;; вставить новый хеадер с уважением к контенту !
  (org-M-RET-may-split-line nil "Don't split current line when creating new heading"))

(defvar org-highlight-mode nil
  "Флаг для включения/выключения выделения текста в режиме org.")

(define-advice mouse-set-region (:after (click) org-highlight ())
  (when (and org-highlight-mode
             (derived-mode-p 'org-mode)
             (use-region-p))
    (let ((origin (buffer-substring (region-beginning) (region-end)))
          (emphasis-char "*"))
      (delete-region (region-beginning) (region-end))
      (insert emphasis-char origin emphasis-char))))

(defun org-highlight-mode-enable ()
  "Включить режим выделения текста в режиме org."
  (interactive)
  (setq org-highlight-mode t)
  (message "Режим выделения текста в режиме org включен."))

(defun org-highlight-mode-disable ()
  "Выключить режим выделения текста в режиме org."
  (interactive)
  (setq org-highlight-mode nil)
  (message "Режим выделения текста в режиме org выключен."))

(use-package org-cliplink
  :defer t)

(use-package org-modern
  :after (org)
  :config
  (global-org-modern-mode)
  (remove-hook 'org-agenda-finalize-hook 'org-modern-agenda)
  (setq org-modern-checkbox nil)
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-block-fringe nil
        org-modern-progress nil ;;  ?
        org-modern-table nil ;; поломано
        org-modern-horizontal-rule (make-string 36 ?─) ;; что это дает ?
        org-modern-priority nil ;; не нашел годного
   ))

(use-package auto-tangle-mode
  :ensure (auto-tangle-mode
           :host github
           :repo "progfolio/auto-tangle-mode.el"
           :local-repo "auto-tangle-mode")
  :commands (auto-tangle-mode))

(use-feature ob-tangle
  :after (org)
  :ensure nil
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
    "bes" 'org-babel-execute-subtree)
  :config
  (dolist (template '(("f" . "src fountain")
                    ("se" . "src emacs-lisp :lexical t")
                    ("ss" . "src shell")
                    ("sj" . "src javascript")))
  (add-to-list 'org-structure-template-alist template))
(use-feature ob-js
  :commands (org-babel-execute:js))
(use-feature ob-python
  :commands (org-babel-execute:python))
(use-feature ob-shell
  :commands (org-babel-execute:bash
             org-babel-execute:shell
             org-babel-expand-body:generic)
  :config (add-to-list 'org-babel-load-languages '(shell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
                )

(use-package org-roam
  :ensure (org-roam :host github :repo "org-roam/org-roam")
  :disabled t
  :general
  (+general-global-application
    "or" '(:ignore t :which-key "org-roam-setup"))
  :init (setq org-roam-v2-ack t))

(use-package ox-gfm :defer t)

(use-package ox-twbs
  :disabled t
  :after (org)
  :defer t)

(use-package olivetti
  :commands (olivetti-mode))

(use-package org-fancy-priorities
  :commands (org-fancy-priorities-mode)
  :hook (org-mode . org-fancy-priorities-mode)
  :diminish ""
  :config
  (setq org-fancy-priorities-list '("⚑" "⬆" "■"))
  )

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
  ;; :hook (
  ;;        (org-mode . org-download-enable)
  ;;        (org-mode . my-org-download-set-dir)
  ;;        )
  :custom
  (org-download-method 'directory)
  (org-download-image-org-width 600)
  (org-download-link-format "[[file:%s]]\n")
  (org-download-abbreviate-filename-function #'expand-file-name)
  (org-download-link-format-function #'org-download-link-format-function-default)
  :config
  (setq org-download-annotate-function (lambda (_link) "")) ;; #+Downloaded
  ;; (defun my-org-download-set-dir ()
  ;;   "Set `org-download-image-dir` to the directory of the current 
  ;;       buffer's file."
  ;;   (setq-local org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))))

  (defun my-org-download-set-dir ()
    (interactive) ;; TODO temp fix 
    "Set `org-download-image-dir` to an Images subdirectory in the current file's directory."
    (let* ((filename (buffer-file-name))
           (file-dir (file-name-directory filename))
           (file-name (file-name-nondirectory (file-name-sans-extension filename)))
           (images-dir (expand-file-name "Attachments" file-dir)))
      (setq-local org-download-image-dir 
                  (expand-file-name (concat file-name "-img") images-dir))))
  )

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

(use-package doct
  :defer t
  :commands (doct))

(use-package vterm
  :ensure (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists))))
  :commands (vterm vterm-other-window)
  :general
  (+general-global-open
    ;;"t" '(:ignore t :which-key "terminal")
    "T" 'vterm-other-window
    "t" 'vterm)
  :config
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package diminish
  :defer 10)

(use-feature winner
  :defer 5
  :general
  (+general-global-window
    "u" 'winner-undo
    "r" 'winner-redo)
  :config (winner-mode))

(use-package yasnippet
  :commands (yas-global-mode)
  :custom
  (yas-snippet-dirs '("~/.config/emacs/elpaca/repos/snippets")))

(use-package doom-snippets
:ensure (doom-snippets :host github :repo "doomemacs/snippets" :files ("*.el" "*"))
;;:load-path "~/.config/vanilla/snippets"
:after yasnippet)

(use-feature tramp
  :defer t
  :custom (tramp-terminal-type "tramp")
  :config (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

(use-feature vc-hooks
  :custom
  (vc-follow-symlinks t "Visit real file when editing a symlink without prompting."))

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

;;; Emacs 28+ (native-comp) stuff
;; (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
;;   (progn
;;     (setq native-comp-async-report-warnings-errors nil)
;;     (setq native-comp-deferred-compilation t)
;;     (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
;;     (setq package-native-compile t)))

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

(use-package project
  :config
  (add-to-list 'project-switch-commands '(+project-magit-status "Magit" "m"))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" "F"))
  :init
  (defun +project-magit-status ()
  (interactive)
  (magit-status (project-root (project-current))))
)

;; (use-package image-roll
;;   :ensure (image-roll :host github :repo "dalanicolai/image-roll.el")
;;   :defer t)

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

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-feature re-builder
  :custom
  (reb-re-syntax 'rx)
  :commands (re-builder))

(use-feature recentf
  :defer 1
  :config (recentf-mode)
  :custom
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files"))

(use-feature cus-edit
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
  (defvar +elfeed-feed-file (expand-file-name "~/Documents/rss-feeds.org"))

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
   "K" 'elfeed-show-prev))

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

(use-feature find-func
  :defer t
  :config (setq find-function-C-source-directory
                (expand-file-name "~/repos/emacs/src/")))

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

(use-package fontify-face
  :commands (fontify-face-mode))

(use-package fountain-mode
  :ensure (fountain-mode :host github :repo "rnkn/fountain-mode")
  :mode "\\.fountain\\'")

(use-package transient :defer t)

(use-package forge
  :ensure (:files (:defaults "docs/*"))
  :after magit
  :init (setq forge-add-default-bindings nil
              forge-display-in-status-buffer nil
              forge-add-pullreq-refspec nil))

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
  ;; :general
  ;; (general-define-key
  ;; :bind (("C-h e F" . helpful-function)
  ;;        ("C-h e C" . helpful-command)
  ;;        ("C-h e M" . helpful-macro)
  ;;        ("C-h e L" . helpful-callable)
  ;;        ("C-h e S" . helpful-at-point)
  ;;        ("C-h e V" . helpful-variable)))

(use-feature holidays
  :commands (org-agenda)
  :custom
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil))

(use-package htmlize
  :defer t)

(use-feature ielm
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

(use-package keycast
  :defer t)

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
  (transient-bind-q-to-quit))

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
  (markdown-command "/usr/bin/pandoc"))

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
  :commands (notmuch)
  :defer t
  :general
  (+general-global-open
    "m" '(notmuch :which-key "Notmuch"))
  :init
  (setq mail-user-agent 'message-user-agent
        message-mail-user-agent t
        )
  (setq notmuch-search-oldest-first nil
     
      message-send-mail-function 'message-send-mail-with-sendmail
      notmuch-always-prompt-for-sender t ;;test 
      message-kill-buffer-on-exit t
      message-directory "~/Mail"
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      notmuch-show-all-tags-list t 
      mail-specify-envelope-from t)
  (setq mail-host-address (system-name))
)

(use-feature whitespace
  :custom
  (whitespace-display-mappings '((tab-mark ?\t [?› ?\t])))
  ;;(whitespace-global-modes '(prog-mode text-mode))
  (whitespace-line-column nil)
  (whitespace-style '(empty face lines-tail tab-mark tabs trailing))
;;   :init 
;;   (defface hs-ellipsis
;;   '((((class color) (background light)) (:underline t))
;;     (((class color) (background dark)) (:underline t))
;;     (t (:underline t)))
;;   "Face for ellipsis in hideshow mode.")

;;   ;; Use this in whitespace-mode
;; (defun whitespace-change-ellipsis ()
;;   "Change ellipsis when used with `whitespace-mode'."
;;   (when buffer-display-table
;;     (set-display-table-slot buffer-display-table
;;                             'selective-display
;;                             ;;(string-to-vector " … ")
;;                             (let ((face-offset (* (face-id 'hs-ellipsis) (lsh 1 22))))
;;                               (vconcat (mapcar (lambda (c) (+ face-offset c)) " … ")))
;;                             )))
;; (add-hook 'whitespace-mode-hook #'whitespace-change-ellipsis)

;; ;; Use this in non-whitespace modes
;; (set-display-table-slot
;;  standard-display-table
;;  'selective-display
;;  (let ((face-offset (* (face-id 'hs-ellipsis) (lsh 1 22))))
;;    (vconcat (mapcar (lambda (c) (+ face-offset c)) " … ")))))
  )

(use-package web-mode
  :defer t
  :mode
  "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package xref
;    :bind
;   ;; Mimic VSCode
;;   ("s-<mouse-1>" . xref-find-definitions-at-mouse)
  :custom
  (xref-search-program 'ripgrep)
  )

(use-feature novice
  :custom
  (disabled-command-function nil "Enable all commands"))

(add-to-list 'auto-mode-alist '("/aliases\\'" . sh-mode))

(require 'extras)

(provide 'init)
;;; init.el ends here
