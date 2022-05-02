(add-hook! git-commit-mode
  (lambda ()
    (when shortcut-elisp-loaded
      (define-key git-commit-mode-map (kbd "C-x C-l") 'shortcut-backend-insert-shortcut-story-url)
      (define-key git-commit-mode-map (kbd "C-x C-a") 'shortcut-backend-insert-co-authored-by))))
