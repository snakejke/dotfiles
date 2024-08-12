;;; config.el --- -*- lexical-binding: t -*-

(setq user-full-name "John Doe"
            user-mail-address "john@doe.com")

(use-package! org-pandoc-import :after org)

(after! org-download
(setq org-download-method 'directory)
(setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
(setq org-download-image-org-width 600)
(setq org-download-link-format "[[file:%s]]\n"        org-download-abbreviate-filename-function #'expand-file-name)
(setq org-download-link-format-function #'org-download-link-format-function-default))

(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))

(pixel-scroll-precision-mode 1)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 21)
      doom-variable-pitch-font(font-spec :family "PT Serif" :size 21)
      doom-serif-font(font-spec :family "PT Serif" :size 21)
      doom-unicode-font (font-spec :family "PT Serif" :size 21))

(after! org
(setq org-my-anki-file "~/OrgFiles/ankiorg.org")
(add-to-list 'org-capture-templates
             '("a" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
             '("A" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))

(add-to-list 'org-capture-templates
             '("z" "Protocol"
               entry
               (file+headline , org-my-anki-file "Inbox")
               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"))
(add-to-list 'org-capture-templates
             '("L" "Protocol Link"
               entry
               (file+headline ,org-my-anki-file "Inbox")
               "* %? [[%:link][%:description]] \nCaptured On: %U"))

)

(defun zp/org-protocol-insert-selection-dwim (selection)
  "Insert SELECTION as an org blockquote."
  (unless (string= selection "")
    ;;(format "#+begin_quote\n%s\n#+end_quote" selection)
    (format "\n%s\n" selection)))

(setq org-roam-capture-ref-templates

      '(("r" "ref" plain "* %U\n
%(zp/org-protocol-insert-selection-dwim \"%i\")%?"
         :target (file+head "web/${slug}.org"
                            "#+title: ${title}\n
#+roam_key: ${ref}\n
#+created: %u\n"
                            )
         :unnarrowed t)))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(add-hook! 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(
      ((driver . "mysql") (dataSourceName . "root:local@tcp(localhost:)/sqlstepik"))
      ))

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Microsoft"
            (shell-command-to-string "uname -r")))
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))

(setq org-hide-emphasis-markers t)
(use-package! org-appear
  :hook ((org-mode . org-appear-mode)
         (org-roam-mode . org-appear-mode))
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(setq +tree-sitter-hl-enabled-modes '(java-mode go-mode))

;(setq doom-theme 'modus-operandi-tinted)
(setq doom-theme 'doom-gruvbox)
;;(add-hook 'prog-mode-hook (lambda () (load-theme 'jetbrains-darcula-theme t)))
;(use-package modus-themes
;  :init
;  (setq modus-themes-org-blocks 'gray-background)
;  )
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

(setq display-line-numbers-type 'relative)

(setq leetcode-prefer-language "java")
(setq leetcode-prefer-sql "mysql")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")

(setq org-directory "~/OrgFiles")
(setq org-agenda-files (directory-files-recursively "~/OrgFiles/" "\.org$"))
(setq org-roam-directory (file-truename "~/OrgFiles"))

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

(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($compact-p
          (if (eq last-command this-command)
              (get this-command 'compact-p)
            (> (- (line-end-position) (line-beginning-position)) fill-column)))
         (deactivate-mark nil)
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "NOERROR")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "NOERROR")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (if $compact-p
        (fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region $p1 $p2)))
    (put this-command 'compact-p (not $compact-p))))

(after! org
  ;; disable auto-complete in org-mode buffers
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  ;; disable company too
  (setq company-global-modes '(not org-mode))
  ;; ...
  )

(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
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
  (anki-editor-reset-cloze-number)
  )

(use-package ankiorg
  :commands
  ankiorg-pull-notes
  ankiorg-buffer-get-media-files
  ankiorg-pull-tags
  :custom
  (ankiorg-sql-database
   "/home/snake/.local/share/Anki2/snake/collection.anki2")
  (ankiorg-media-directory
   "/home/snake/.local/share/Anki2/snake/collection.media/"))

;;(require 'sqlite3) ;; без этого не работало вроде. ;; сейчас заработало на линуксе. хм.

(use-package! dockerfile-mode
  :mode "Dockerfile\\'"
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  )

;; (use-package! docker-compose-mode
;;   :mode
;;   (("docker-compose.yml\\'" . docker-compose-mode)
;;    ("docker-compose.yml\\'" . docker-compose-mode)
;;    ("docker_compose.yml\\'" . docker-compose-mode)
;;    ("docker_compose.yaml\\'" . docker-compose-mode)))
