;;; early-init.el --- Emacs pre package.el & GUI configuration -*- lexical-binding: t; -*-
;;; Code:

(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)
;; (setq use-package-compute-statistics t)

(setq native-comp-async-report-warnings-errors nil)
;; (debug-on-entry 'emacs-repository-branch-git)
;; (debug-on-entry 'emacs-repository-version-git)

;;; Debugging
;; Running this form will launch the debugger after loading a package.
;; This is useful for finding out when a dependency is requiring a package (perhaps earlier than you want).
;; Use by tangling this block and launching Emacs with =emacs --debug-init=.

;; (unless (string-empty-p file)
;;   (eval-after-load file
;;     '(debug)))

;; Similarly, this variable will hit the debugger when a message matches its regexp.

;; (setq debug-on-message "")

;; Adding a variable watcher can be a useful way to track down initialization and mutation of a variable.
;; (add-variable-watcher 'org-capture-after-finalize-hook
;;                       (lambda (symbol newval operation where)
;;                         (debug)
;;                         (message "%s set to %s" symbol newval)))

;; (setq debug-on-error t)

;; Пропуск  регулярных выражений в  =file-name-handler-alist= .
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Папка с кешем
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "cache/eln-cache/" user-emacs-directory)))

;; Настройка Garbage Collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; Увеличивает время запуска. Здесь не надо указывать.

;; (setq read-process-output-max (* 64 1024))  ; 64kb

;;Отключаем tool-bar, menu-bar, vertical-scroll
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Отключаем инструкции как закрывать emacsclient
(setq server-client-instructions nil)

;;Не менять размер фрейма в зависимости от шрифтов
;;+к времени запуска если nil
(setq frame-inhibit-implied-resize t)

;;Игнорирование настроек в  Xresources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Taken from:

;; [[https://github.com/vsemyonoff/emacsrc/blob/14649a5bafea99cc7e13e7d048e9d15aed7926ce/early-init.el]]

;; This helps with a bug I was hitting when using =desktop-save-mode='s =desktop-read=.

(setq desktop-restore-forces-onscreen nil)

;;(push '(font . "PT Serif") default-frame-alist)


;; (push '(font . "JetBrainsMono Nerd Font") default-frame-alist)
;; (set-face-font 'default "JetBrainsMono Nerd Font")
(push '(font . "JetBrains Mono") default-frame-alist)
(set-face-font 'default "JetBrains Mono")
(set-face-font 'variable-pitch "IBM Plex Serif")
(copy-face 'default 'fixed-pitch)

;; emoji without VARIATION SELECTOR-16 (U+FE0F)
;; (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
