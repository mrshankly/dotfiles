;; Hide menu bar, tool bar and scroll bars.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Stop nagging.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq use-dialog-box nil)

(defun display-startup-echo-area-message ()
  (message ""))

;; Stop creating backup~ and #autosave# files.
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; Set site-lisp local path.
(setq site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)

;; Set themes local path.
(setq themes-dir
  (expand-file-name "themes" user-emacs-directory))

(add-to-list 'custom-theme-load-path themes-dir)

;; Set theme and font.
(load-theme 'gruvbox-dark-hard t)
(set-face-attribute 'default nil :font "Iosevka Term SS05-11")

;; Show line numbers and highlight current line.
(global-display-line-numbers-mode)
(global-hl-line-mode +1)

;; Highlight matching brackets and insert by pair.
(show-paren-mode 1)
(electric-pair-mode 1)

;; Standard ML
(autoload 'sml-mode "sml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sml\\'" . sml-mode))

;; Rust
(setq rust-format-on-save t)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
