;;; ob-scala-cli.el --- Org Babel support for Scala via scala-cli -*- lexical-binding: t; -*-

;; Author: based on ob-ammonite.el by xuwei and ob-scala-cli by luca
;; Keywords: literate programming, scala, org-babel
;; Version: 0.2.0

;;; Commentary:
;;
;; Org Babel backend for Scala with two execution modes:
;;
;; MODE 1 — REPL SESSION (default)
;;   Keeps a `scala-cli repl` process alive between blocks.
;;   Definitions persist across blocks — like a real interactive session.
;;   Use for exploration, learning, experimenting with pure Scala code.
;;
;;   #+begin_src scala-cli
;;   def greet(name: String) = s"Hello, $name!"
;;   greet("World")
;;   #+end_src
;;
;; MODE 2 — SCRIPT (activated by :dep or //> using directives)
;;   Runs `scala-cli run` on a temporary file.
;;   Supports //> using dep, //> using scala, @main, etc.
;;   Each block is independent — no shared state.
;;
;;   #+begin_src scala-cli :dep "com.lihaoyi::os-lib:0.11.4"
;;   //> using scala 3.8.3
;;   @main def run() =
;;     val paths = os.list(os.pwd)
;;     println(paths.length)
;;   #+end_src
;;
;;   Or with inline directives only (no header args needed):
;;
;;   #+begin_src scala-cli
;;   //> using dep "com.lihaoyi::os-lib:0.11.4"
;;   @main def run() =
;;     println(os.list(os.pwd).length)
;;   #+end_src
;;
;; SETUP in your init file:
;;
;;   (use-feature ob-scala-cli
;;     :load-path "~/.config/emacs/lisp/"
;;     :commands (org-babel-execute:scala-cli)
;;     :config
;;     (add-to-list 'org-babel-load-languages '(scala-cli . t)))

;;; Code:

(require 'ob)
(require 'ob-comint)
(require 'comint)

;; ---------------------------------------------------------------------------
;; Customisation
;; ---------------------------------------------------------------------------

(defgroup ob-scala-cli nil
  "Org Babel settings for scala-cli."
  :group 'org-babel)

(defcustom ob-scala-cli-command "scala-cli"
  "Path to the scala-cli executable."
  :type 'string
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-repl-args
  '("repl"
    "--java-opt" "--sun-misc-unsafe-memory-access=allow")
  "Arguments passed to scala-cli when starting the REPL.
The --sun-misc-unsafe-memory-access=allow flag silences the
sun.misc.Unsafe deprecation warnings on JVM 25 with Scala 3.8.x.
On older JVMs this flag is silently ignored."
  :type '(repeat string)
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-run-args
  '("run"
    "--java-opt" "--sun-misc-unsafe-memory-access=allow")
  "Base arguments passed to scala-cli in script mode.
Dependency and scala-version flags are appended dynamically
from the source block header arguments."
  :type '(repeat string)
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-prompt-regexp "^scala> "
  "Regexp that matches the scala-cli REPL prompt."
  :type 'string
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-buffer-name "*scala-cli-repl*"
  "Name of the buffer holding the running scala-cli REPL process."
  :type 'string
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-poll-interval 0.3
  "Seconds between checks when polling for REPL output."
  :type 'float
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-timeout 60
  "Maximum seconds to wait for a block to finish evaluating."
  :type 'integer
  :group 'ob-scala-cli)

(defcustom ob-scala-cli-script-timeout 120
  "Maximum seconds to wait for script mode (scala-cli run).
Script mode compiles from scratch and may download dependencies,
so it needs a longer timeout than REPL mode."
  :type 'integer
  :group 'ob-scala-cli)

;; ---------------------------------------------------------------------------
;; Language registration
;; ---------------------------------------------------------------------------

(add-to-list 'org-babel-tangle-lang-exts '("scala-cli" . "scala"))
(add-to-list 'org-src-lang-modes         '("scala-cli" . scala))

(defvar org-babel-default-header-args:scala-cli '()
  "Default header arguments for scala-cli source blocks.")

;; Teach Org Babel about the header args we support so they get completion.
(defconst org-babel-header-args:scala-cli
  '((dep           . :any)
    (scala-version . :any)
    (jvm           . :any))
  "Header arguments recognised by ob-scala-cli.")

;; ---------------------------------------------------------------------------
;; Internal state (REPL mode)
;; ---------------------------------------------------------------------------

(defconst ob-scala-cli--sentinel ";;;;;ob-scala-cli-done;;;;;"
  "String sent to the REPL as a marker signalling end of block evaluation.")

(defvar ob-scala-cli--output ""
  "Accumulator for raw output from the REPL process filter.")

(defvar ob-scala-cli--block-result ""
  "Output of the user's code block, saved before the sentinel phase.")

(defvar ob-scala-cli--done nil
  "Set to t by the process filter when the sentinel is detected.")

;; ---------------------------------------------------------------------------
;; Mode detection
;; ---------------------------------------------------------------------------

(defun ob-scala-cli--script-mode-p (body params)
  "Return non-nil when BODY/PARAMS require script mode instead of REPL mode.
Script mode is triggered by:
  - :dep header argument (one dependency or a list)
  - :scala-version header argument
  - :jvm header argument
  - //> using directives anywhere in BODY"
  (or (cdr (assq :dep           params))
      (cdr (assq :scala-version params))
      (cdr (assq :jvm           params))
      (string-match-p "^//>" body)))

;; ---------------------------------------------------------------------------
;; Temporary file helpers (script mode)
;; ---------------------------------------------------------------------------

(defvar ob-scala-cli--tmp-dir nil
  "Temporary directory used for script-mode files in this session.")

(defun ob-scala-cli--tmp-dir ()
  "Return (creating if needed) the temp directory for script files."
  (unless (and ob-scala-cli--tmp-dir
               (file-directory-p ob-scala-cli--tmp-dir))
    (setq ob-scala-cli--tmp-dir
          (make-temp-file "ob-scala-cli-" t)))
  ob-scala-cli--tmp-dir)

(defun ob-scala-cli--write-script (body params)
  "Write BODY to a temporary file, prepending :dep directives from PARAMS.
.sc extension is used for scripts (top-level expressions, no @main).
.scala extension is used when @main is present.
Returns the path to the file."
  (let* (;; .sc  = script: top-level expressions OK, @main NOT supported
         ;; .scala = regular: @main works, top-level expressions don't
         (ext  (if (string-match-p "@main" body) "scala" "sc"))
         (file (expand-file-name
                (format "block-%s.%s" (format-time-string "%s%N") ext)
                (ob-scala-cli--tmp-dir)))
         (deps (let ((d (cdr (assq :dep params))))
                 (cond ((null d)         '())
                       ((listp d)        d)
                       (t                (list d)))))
         (scala-ver (cdr (assq :scala-version params)))
         (jvm-ver   (cdr (assq :jvm params)))
         ;; Build //> using lines for header-arg dependencies that are NOT
         ;; already present as inline directives in the body.
         (extra-directives
          (concat
           (mapconcat (lambda (dep)
                        (format "//> using dep \"%s\"\n" dep))
                      deps "")
           (when (and scala-ver
                      (not (string-match-p "//> using scala" body)))
             (format "//> using scala \"%s\"\n" scala-ver))
           (when (and jvm-ver
                      (not (string-match-p "//> using jvm" body)))
             (format "//> using jvm \"%s\"\n" jvm-ver)))))
    (with-temp-file file
      (insert extra-directives)
      (insert body))
    file))

;; ---------------------------------------------------------------------------
;; Script mode execution
;; ---------------------------------------------------------------------------

(defun ob-scala-cli--run-script (body params)
  "Execute BODY as a scala-cli script, return output as a string.
Dependencies and version overrides are taken from PARAMS."
  (let* ((file (ob-scala-cli--write-script body params))
         (cmd  (append (list ob-scala-cli-command)
                       ob-scala-cli-run-args
                       (list file)))
         (cmd-str (mapconcat #'shell-quote-argument cmd " ")))
    (message "ob-scala-cli [script]: %s" cmd-str)
    (let ((raw (shell-command-to-string cmd-str)))
      (ob-scala-cli--clean-script-output raw))))

;; (defun ob-scala-cli--clean-script-output (raw)
;;   "Remove scala-cli compilation banners and JVM warnings from RAW output."
;;   (let* ((lines (split-string raw "\n"))
;;          (filtered
;;           (seq-filter
;;            (lambda (l)
;;              (not (string-match-p
;;                    (rx (or
;;                         ;; scala-cli compilation progress lines
;;                         (: bol (* space) "Compiling")
;;                         (: bol (* space) "Compiled")
;;                         (: bol (* space) "Running")
;;                         ;; JVM unsafe warnings
;;                         (: bol "WARNING:")))
;;                    l)))
;;            lines)))
;;     (string-trim (string-join filtered "\n"))))

(defun ob-scala-cli--clean-script-output (raw)
  "Remove scala-cli noise from RAW output."
  (let* (;; remove ANSI color sequences first
         (clean-raw (ansi-color-filter-apply raw))

         (lines (split-string clean-raw "\n"))

         (filtered
          (seq-filter
           (lambda (l)
             (not
              (string-match-p
               (rx (or
                    ;; scala-cli download chatter
                    (: bol "Downloading ")
                    (: bol "Downloaded ")

                    ;; scala-cli compilation banners
                    (: bol (* space) "Compiling")
                    (: bol (* space) "Compiled")
                    (: bol (* space) "Running")

                    ;; JVM warnings
                    (: bol "WARNING:")))

               l)))
           lines)))

    (string-trim (string-join filtered "\n"))))

;; ---------------------------------------------------------------------------
;; REPL lifecycle
;; ---------------------------------------------------------------------------

(defun ob-scala-cli--running-p ()
  "Return non-nil if the scala-cli REPL buffer has a live process."
  (comint-check-proc ob-scala-cli-buffer-name))

(defun ob-scala-cli--start-repl ()
  "Launch scala-cli repl and wait for the first prompt.  Return the buffer."
  (message "ob-scala-cli: starting REPL...")
  (let* ((buf-name (string-remove-prefix
                    "*" (string-remove-suffix "*" ob-scala-cli-buffer-name)))
         (buf (apply #'make-comint
                     buf-name
                     ob-scala-cli-command
                     nil
                     ob-scala-cli-repl-args)))
    (with-current-buffer buf
      (let ((deadline (+ (float-time) 120)))
        (while (and (< (float-time) deadline)
                    (not (save-excursion
                           (goto-char (point-min))
                           (re-search-forward ob-scala-cli-prompt-regexp nil t))))
          (sit-for 0.5)))
      (unless (save-excursion
                (goto-char (point-min))
                (re-search-forward ob-scala-cli-prompt-regexp nil t))
        (error "ob-scala-cli: timed out waiting for REPL prompt")))
    (message "ob-scala-cli: REPL ready.")
    buf))

(defun ob-scala-cli--get-repl ()
  "Return the REPL buffer, starting the process first if needed."
  (unless (ob-scala-cli--running-p)
    (ob-scala-cli--start-repl))
  (get-buffer ob-scala-cli-buffer-name))

;; ---------------------------------------------------------------------------
;; Process filter (REPL mode)
;; ---------------------------------------------------------------------------

(defun ob-scala-cli--make-filter (process)
  "Install a capturing process filter on PROCESS.
Accumulates clean output into `ob-scala-cli--output' and sets
`ob-scala-cli--done' when the sentinel string is detected."
  (set-process-filter
   process
   (lambda (proc str)
     (comint-output-filter proc str)
     (let ((clean (substring-no-properties (ansi-color-filter-apply str))))
       (setq ob-scala-cli--output (concat ob-scala-cli--output clean))
       (when (string-match-p (regexp-quote ob-scala-cli--sentinel)
                             ob-scala-cli--output)
         (setq ob-scala-cli--done t))))))

;; ---------------------------------------------------------------------------
;; REPL output post-processing
;; ---------------------------------------------------------------------------

(defun ob-scala-cli--clean-output (raw)
  "Extract meaningful Scala output from RAW REPL output.
Removes prompt echoes, JVM warnings, and sentinel lines."
  (let* ((lines (split-string raw "\n"))
         (before-sentinel
          (seq-take-while
           (lambda (l)
             (not (string-match-p (regexp-quote ob-scala-cli--sentinel) l)))
           lines))
         (trimmed
          (seq-filter
           (lambda (l)
             (not (string-match-p
                   (rx (or (: bol "scala" (? " ") (? ">") (* " ") eol)
                           (: bol "WARNING:")))
                   l)))
           before-sentinel)))
    (string-trim (string-join trimmed "\n"))))

;; ---------------------------------------------------------------------------
;; REPL mode execution
;; ---------------------------------------------------------------------------

(defun ob-scala-cli--run-repl (body)
  "Evaluate BODY in the persistent scala-cli REPL.  Return result string."
  (let* ((buf     (ob-scala-cli--get-repl))
         (process (get-buffer-process buf)))
    (unless process
      (error "ob-scala-cli: no process in REPL buffer"))

    (setq ob-scala-cli--output ""
          ob-scala-cli--done   nil)
    (ob-scala-cli--make-filter process)

    ;; Step 1: send the code block wrapped in braces.
    (comint-send-string buf (format "{\n%s\n}\n" body))

    ;; Step 2: wait for the prompt that signals evaluation is complete.
    (let ((deadline (+ (float-time) ob-scala-cli-timeout)))
      (while (and (< (float-time) deadline)
                  (not (string-match-p "scala> " ob-scala-cli--output)))
        (sit-for ob-scala-cli-poll-interval)))

    ;; Step 3: save block result, reset accumulator, send sentinel.
    (setq ob-scala-cli--block-result ob-scala-cli--output
          ob-scala-cli--output       ""
          ob-scala-cli--done         nil)
    (ob-scala-cli--make-filter process)
    (comint-send-string buf (format "\"%s\"\n" ob-scala-cli--sentinel))

    ;; Step 4: wait for sentinel to echo back.
    (let ((deadline (+ (float-time) ob-scala-cli-timeout)))
      (while (and (not ob-scala-cli--done)
                  (< (float-time) deadline))
        (sit-for ob-scala-cli-poll-interval))
      (unless ob-scala-cli--done
        (error "ob-scala-cli: timed out waiting for evaluation to finish")))

    (sit-for 0.1)
    (set-process-filter process #'comint-output-filter)
    (ob-scala-cli--clean-output ob-scala-cli--block-result)))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defun org-babel-execute:scala-cli (body params)
  "Execute a scala-cli source block.
Dispatches to script mode or REPL mode based on BODY and PARAMS.

Script mode is used when BODY contains //> using directives or
PARAMS contain :dep, :scala-version, or :jvm.
REPL mode is used otherwise — definitions persist between blocks."
  (if (ob-scala-cli--script-mode-p body params)
      (progn
        (message "ob-scala-cli: using script mode")
        (ob-scala-cli--run-script body params))
    (message "ob-scala-cli: using REPL mode")
    (ob-scala-cli--run-repl body)))

;; ---------------------------------------------------------------------------
;; Session stubs (required by Org Babel protocol)
;; ---------------------------------------------------------------------------

(defun org-babel-prep-session:scala-cli (_session _params)
  "Prepare a scala-cli session."
  (ob-scala-cli--get-repl))

(defun org-babel-scala-cli-initiate-session (&optional _session _params)
  "Return the scala-cli REPL buffer, starting it if needed."
  (ob-scala-cli--get-repl))

;; ---------------------------------------------------------------------------
;; Setup helper
;; ---------------------------------------------------------------------------

(defun ob-scala-cli-setup ()
  "Register scala-cli with Org Babel.
Alternatively, add \\='(scala-cli . t) to `org-babel-load-languages'."
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((scala-cli . t)))))

;; ---------------------------------------------------------------------------
;; Interactive helpers
;; ---------------------------------------------------------------------------

(defun ob-scala-cli-restart-repl ()
  "Kill the running REPL and start a fresh one."
  (interactive)
  (when (ob-scala-cli--running-p)
    (kill-buffer ob-scala-cli-buffer-name)
    (message "ob-scala-cli: old REPL killed."))
  (ob-scala-cli--start-repl))

(defun ob-scala-cli-show-repl ()
  "Switch to the scala-cli REPL buffer, starting it if needed."
  (interactive)
  (pop-to-buffer (ob-scala-cli--get-repl)))

(provide 'ob-scala-cli)
;;; ob-scala-cli.el ends here
