;; use word wrapping instead of truncation in test report and doc
;; buffers. Must be set before loading cider.
(setq cider-special-mode-truncate-lines nil)

(setq shortcut-elisp-loaded nil)
(when (file-exists-p "~/.doom.d/shortcut-backend.el")
  (load! "shortcut-shared.el"))

(when shortcut-elisp-loaded
  (shortcut-backend-enable-matcher-combinator-test-output-colors))

(defun tc/rename-buffer-to-ns ()
  (interactive)
  (when (buffer-file-name)
    (let ((ns (clojure-expected-ns)))
      (when (and (stringp ns)
                 (not (string= "" ns)))
        (rename-buffer ns)))))

(use-package! clojure-mode
  :init
  (add-hook! clojure-mode 'subword-mode)
  (setq clojure-indent-style :always-align))

(use-package! cider
  :init
  (add-hook! cider-repl-mode 'tc/turn-on-paredit)
  :bind
  (:map cider-repl-mode-map))

(use-package! lsp-mode
  :ensure true)

(setq
 lsp-lens-enable t
 lsp-signature-auto-activate nil)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

(when shortcut-elisp-loaded
  ;; use my local cider alias and set up scope-capture
  (setq shortcut-backend-default-clojure-cli-options
      "-J-server -J-Xmx8g -J-XX:+UseG1GC -J-Dapple.awt.UIElement=true -J-Dtika.config=tika-config.xml -M:backend-defaults:dev:test:cider")
  (add-to-list 'cider-jack-in-nrepl-middlewares "sc.nrepl.middleware/wrap-letsc")
  (cider-add-to-alist 'cider-jack-in-dependencies "vvvvalvalval/scope-capture-nrepl" "0.3.1"))

(defun tc/insert-spy ()
  (interactive)
  (move-beginning-of-line nil)
  (insert "(sc.api/spy)")
  (indent-for-tab-command))

(defun tc/insert-spy-letsc ()
  (interactive)
  (insert "(sc.api/letsc )")
  (backward-char))

(defun tc/insert-divider-comment ()
  (interactive)
  (move-beginning-of-line nil)
  (insert ";; -----  -----\n")
  (previous-line)
  (search-forward "- "))

(defun tc/insert-comment (text)
  (move-beginning-of-line nil)
  (insert (format ";; %s" text))
  (indent-for-tab-command))

(defun tc/insert-note (type)
  (tc/insert-comment (format "%s: (toby) " type)))

(defun tc/insert-fixme ()
  (interactive)
  (tc/insert-note "FIXME"))

(defun tc/insert-todo ()
  (interactive)
  (tc/insert-note "TODO"))

(defun tc/insert-nocommit ()
  (interactive)
  (tc/insert-note "NOCOMMIT"))

(defun tc/insert-given ()
  (interactive)
  (tc/insert-comment "Given: "))

(defun tc/insert-when ()
  (interactive)
  (tc/insert-comment "When: "))

(defun tc/insert-then ()
  (interactive)
  (tc/insert-comment "Then: "))

(defun tc/insert-and ()
  (interactive)
  (tc/insert-comment "And: "))
