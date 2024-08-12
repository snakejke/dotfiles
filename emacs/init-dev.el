;;; init-dev.el --- development settings for init file  -*- lexical-binding: t; -*-


;;; Commentary:
;; Evaluated when loading init file.
;; Cleaner to keep this in a separate file rather than a long single line at the top of init.org

;;; Code:
(setq-local org-confirm-babel-evaluate nil)
(require 'auto-tangle-mode)
(auto-tangle-mode)
(add-hook 'auto-tangle-after-tangle-hook (lambda ()
                                           (let ((elpaca-log-functions nil))
                                             (load-file "~/.config/emacs/init.el")
                                             (elpaca-process-queues))))
(eldoc-mode)
(provide 'init-dev)
;;; init-dev.el ends here
