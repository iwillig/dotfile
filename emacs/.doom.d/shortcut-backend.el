;;; shortcut-backend.el --- Shared elisp for Shortcut backend developers -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; To use this do something like:
;;;
;;;   (when (file-exists-p "~/backend/elisp")
;;;     (add-to-list 'load-path "~/backend/elisp")
;;;     (require 'shortcut-backend))
;;;
;;; Alternatively:
;;;
;;;   (use-package shortcut-backend
;;;     :load-path "~/backend/elisp"
;;;     :if (file-exists-p "~/backend/elisp")
;;;     :hook (clojure-mode . shortcut-backend-font-lock)
;;;     :bind (("C-c C-r" . shortcut-backend-goto-defresource)))
;;;
;;; Ground rules:
;;;
;;; 1. Prefix functions and variables with 'shortcut-backend-'
;;; 2. flycheck is your friend.
;;; 3. You should only put shared functions or custom variables here.  At some
;;;    point we may be able to agree on a common prefix for keybindings, for
;;;    now, define your own.
;;;
;;; Code:
(require 'cider)

(defgroup shortcut-backend nil
  "Configuration for shortcut backend elisp"
  :group 'default)

(defcustom shortcut-backend-directory "~/backend"
  "Location of shortcut/backend checkout.  Defaults to \"~/backend\"."
  :group 'shortcut-backend
  :type 'string)

(defun shortcut-backend-modules ()
  "Return list of modules in shortcut backend."
  (let ((modules-dir (concat shortcut-backend-directory "/modules")))
    (seq-filter (lambda (dir)
                  (or (file-exists-p (concat modules-dir "/" dir "/shortcut-module.edn"))
                      (file-exists-p (concat modules-dir "/" dir "/clubhouse-module.edn"))))
                (directory-files modules-dir))))

(defvar shortcut-backend-default-clojure-cli-options
  "-J-server -J-Xmx4g -J-XX:+UseG1GC -J-Dapple.awt.UIElement=true -J-Dtika.config=tika-config.xml -M:backend-defaults:dev:test:cider"
  "The default options passed to clojure-cli when jacking
  in. Will be appended to cider-clojure-cli-global-options if you
  have that set as well.")

(defun shortcut-backend-jack-in-module (&optional module)
  "Start nREPL server for shortcut backend MODULE.
If MODULE is not specified prompt for one."
  (interactive)
  (let* ((module (or module (completing-read "module: " (shortcut-backend-modules))))
         (default-directory (concat shortcut-backend-directory "/modules/" module))
         (cider-clojure-cli-global-options
          (if (stringp cider-clojure-cli-global-options)
              (format "%s %s" cider-clojure-cli-global-options
                      shortcut-backend-default-clojure-cli-options)
            shortcut-backend-default-clojure-cli-options)))
    (shell-command "make" (concat "*make " module " output*"))
    (let ((params (thread-first '()
                   (plist-put :project-dir default-directory)
                   (cider--update-project-dir)
                   (cider--check-existing-session)
                   (cider--update-jack-in-cmd))))
      (nrepl-start-server-process
       (plist-get params :project-dir)
       (plist-get params :jack-in-cmd)
       (lambda (server-buffer)
         (cider-connect-sibling-clj params server-buffer))))
    nil))

(defun shortcut-backend-jack-in-dev-system ()
  "Start nREPL server for shortcut backend dev-system module."
  (interactive)
  (shortcut-backend-jack-in-module "dev-system"))

(defun shortcut-backend-find-defresource ()
  "Find defresource forms in the current buffer.
Return alist from NAME to MARKER."
  (save-excursion
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "^\s*(defresource \\(.*\\)" nil t)
        (let* ((match (match-data))
               (pos (marker-position (car match)))
               (name-start (marker-position (caddr match)))
               (name-end (marker-position (cadddr match)))
               (name (buffer-substring-no-properties name-start name-end)))
          (setf (alist-get name matches) pos)))
      (reverse matches))))

(defun shortcut-backend-goto-defresource ()
  "Go to defresource form in current buffer."
  (interactive)
  (let* ((matches (shortcut-backend-find-defresource))
         (selection (when matches
                      (completing-read "resource: " matches))))
    (when selection
      (goto-char (alist-get selection matches nil nil #'equal)))))

(defun shortcut-backend-insert-shortcut-story-url ()
  "Generate and insert story URL from branch."
  (interactive)
  (insert
   (save-excursion
     (when (re-search-forward "branch .*/\\(ch\\|sc-\\)\\([0-9]+\\)" nil t)
       (format "https://app.shortcut.com/internal/story/%s/" (match-string 2))))))

(defun shortcut-backend-get-recent-git-authors (n)
  "Return alpha sorted list of unique authors of last N git commits."
  (let ((default-directory (file-name-directory (buffer-file-name))))
    (split-string
     (shell-command-to-string
      (format "git log HEAD~%s.. --pretty='%%an <%%ae>' | sort | uniq" n))
     "\n")))

(defun shortcut-backend-insert-co-authored-by ()
  "Insert 'Co-authored-by' header for author from 50 most recent commits."
  (interactive)
  (insert (format "Co-authored-by: %s"
                  (completing-read "co-author: "
                                   (save-excursion
                                     (shortcut-backend-get-recent-git-authors 50))))))

(defgroup shortcut-backend-faces nil
  "Configuration for shortcut backend faces"
  :group 'shortcut-backend)

(defface shortcut-backend-bdd-keyword-face '((t . (:inherit (font-lock-doc-face) :weight bold)))
  "Face used for Scenario, Given, When, Then, And keywords."
  :group 'shortcut-backend-faces)

(defface shortcut-backend-todo-face '((t . (:inherit (font-lock-doc-face) :weight bold)))
  "Face used for TODO (and related) keywords."
  :group 'shortcut-backend-faces)

(defface shortcut-backend-nocommit-face '((t . (:inherit (font-lock-warning-face))))
  "Face used for NOCOMMIT keyword."
  :group 'shortcut-backend-faces)

(defun shortcut-backend-font-lock ()
  (font-lock-add-keywords
   nil '(("\\<\\(Scenario\\|Given\\|When\\|Then\\|And\\):"
          1 'shortcut-backend-bdd-keyword-face t)))
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|FIX\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 'shortcut-backend-todo-face t)))
  (font-lock-add-keywords
   nil '(("\\<\\(NOCOMMIT\\):"
          1 'shortcut-backend-nocommit-face t))))

(defun shortcut-backend-enable-matcher-combinator-test-output-colors ()
  "nubank/matcher-combinators uses non-standard ansi color
sequences. This teaches cider to interpret them properly in test
output."
  (advice-add 'cider-ansi-color-string-p :override
              (lambda (string) (string-match "" string)))
  (advice-add 'cider-font-lock-as
              :before
              (lambda (&rest r)
                (advice-add 'substring-no-properties :override #'substring)))
  (advice-add 'cider-font-lock-as
              :after
              (lambda (&rest r)
                (advice-remove 'substring-no-properties #'substring))))

(provide 'shortcut-backend)
;;; shortcut-backend.el ends here
