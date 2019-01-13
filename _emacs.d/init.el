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
(load-theme 'gruvbox-dark-medium t)
(set-face-attribute 'default nil :font "Iosevka Term SS05-11:weight=demibold")

;; Show line numbers and highlight current line.
(global-display-line-numbers-mode)
(global-hl-line-mode +1)

;; Highlight matching brackets and insert by pair.
(show-paren-mode 1)
(electric-pair-mode 1)

;; Standard ML
(autoload 'sml-mode "sml-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.sml\\'" . sml-mode))

;; OCaml
(setq tuareg-match-patterns-aligned t)
(load (expand-file-name "tuareg/tuareg-site-file" site-lisp-dir))

(let ((opam-share
  (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      ;; Merlin
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      (setq merlin-command 'opam)))
      ;; ocp-indent
      (require 'ocp-indent)
      ;; ocamlformat
      (require 'ocamlformat)
      (add-hook 'tuareg-mode-hook (lambda ()
        (add-hook 'before-save-hook 'ocamlformat-before-save)))

;; Rust
(setq rust-format-on-save t)
(autoload 'rust-mode "rust-mode" nil t nil)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Company
(add-to-list 'load-path (expand-file-name "company-mode" site-lisp-dir))
(autoload 'company-mode "company" nil t nil)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

(add-hook 'merlin-mode-hook 'company-mode t)
