;; Increase garbage collection threshold to reduce start up time.
(setq gc-cons-threshold (* 100 1024 1024))

;; Change some defaults.
(fset 'yes-or-no-p 'y-or-n-p)
(set-default-coding-systems 'utf-8)
(setq-default vc-follow-symlinks t)

;; Bootstrap straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Hide menu bar, tool bar and scroll bars.
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

;; Stop nagging.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq use-dialog-box nil)

(defun display-startup-echo-area-message ()
  (message ""))

;; Stop littering.
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(use-package no-littering)

;; Set theme, font and general appearance.
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :config
  (column-number-mode 1)
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-enable-word-count t))

(set-face-attribute 'default nil :font "Iosevka-13")

;; Show line numbers and highlight current line.
(global-display-line-numbers-mode)
(global-hl-line-mode +1)

;; Highlight matching brackets and insert by pair.
(show-paren-mode 1)
(electric-pair-mode 1)

;; Ensure there's a newline at the EOF and show trailing whitespace.
(setq-default require-final-newline t)
(setq-default show-trailing-whitespace t)

;; Display available keybindings.
(use-package which-key
  :config
  (which-key-mode))

;; Autocomplete.
(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2))

;; Reduce garbage collection threshold so that garbage collection pauses are faster.
(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold (* 2 1024 1024))))
