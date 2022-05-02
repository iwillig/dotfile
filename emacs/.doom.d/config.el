;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ivan Willig"
      user-mail-address "iwillig@gmail.com")



;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; doom-nord
;; doom-palenight


(setq doom-theme 'doom-nord

      ;;      doom-font (font-spec :family "OpenDyslexicMono" :size 24 :weight 'bold)

      )


;;


;; Doom increases/decreases the font size by 2 by default, which is too much.
(setq doom-font-increment 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! ace-window
  :bind (("C-:" . ace-window)
         ("C-x :" . ace-window)))

(use-package! find-file-in-project
  :init
  (setq ffip-use-rust-fd t)
  :bind
  (("C-x y" . find-file-in-project)
   ("M-g s" . ag-project)))

(use-package! uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package! swiper
  :bind
  (("C-s" . swiper)
   :map swiper-map
   ("C-." .
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
   ("M-." .
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))))

;; general configuration

;; never insert a tab. First indent, then complete
(setq tab-always-indent 'complete)

;; kill entire line (including \n)
(setq kill-whole-line t)

;; Never background/iconify
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; advise zap-to-char to delete *up to* char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))

;; Pin buffers to windows
(defun tc/toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(global-set-key (kbd "C-x p") 'tc/toggle-current-window-dedication)

;; highlight the symbol at point
(use-package! idle-highlight-mode
  :init (global-idle-highlight-mode 1))

(use-package shortcut-backend
  :load-path "/home/ivan/development/clubhouse/backend/tools/elisp"
  :if (file-exists-p "/home/ivan/development/clubhouse/backend/tools/elisp")
  :hook (clojure-mode . shortcut-backend-font-lock)
  :bind (("C-c C-r" . shortcut-backend-goto-defresource)))

(use-package! doom-modeline
  :init
  ;; By default, doom-modeline uses 'auto mode, which uses project-relative file
  ;; path. I prefer the buffer name, since I have the path in the header line
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

(load! "functions.el")
(load! "clojure.el")
(load! "elisp.el")
(load! "flycheck.el")
(load! "header.el")
(load! "magit.el")
(load! "org.el")
