;;; -*- lexical-binding: t -*-

;; Speed up initialization.
(defvar original-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-percentage 0.6
      gc-cons-threshold (* 400 1024 1024)
      message-log-max (* 16 1024)
      file-name-handler-alist nil)

(defun jm/reset-file-name-handler-alist ()
  (setq file-name-handler-alist original-file-name-handler-alist))

(add-hook 'after-init-hook #'jm/reset-file-name-handler-alist)

;; Not going to use package.el, disable it.
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Always load newest bytecode.
(setq load-prefer-newer t)

;; Answer questions with a simple y/n.
(fset 'yes-or-no-p #'y-or-n-p)

;; Decrease time to display incomplete keystrokes.
(setq echo-keystrokes 0.2)

;; Just kill the damn processes.
(setq confirm-kill-processes nil)

;; Stop nagging.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message ""
      use-dialog-box nil)

(defun display-startup-echo-area-message ()
  (message ""))

;; Save open buffers, cursor position and always start maximized.
(setq desktop-restore-frames nil)
(desktop-save-mode 1)
(save-place-mode 1)

;; Disable backups and auto-save.
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;; Prefer utf-8 encoding.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Disable alarms.
(setq ring-bell-function 'ignore)

;; Improve scrolling.
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-step 1
      hscroll-step 1
      scroll-margin 5
      hscroll-margin 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Configure and display line numbers.
(setq-default display-line-numbers-type t
              display-line-numbers-width 3
              display-line-numbers-widen t)

(global-display-line-numbers-mode 1)

;; Don't display line numbers on some modes.
(defun jm/maybe-hide-line-numbers ()
  (when (derived-mode-p 'term-mode
                        'shell-mode)
    (display-line-numbers-mode -1)))

(add-hook 'after-change-major-mode-hook #'jm/maybe-hide-line-numbers)

;; Highlight current line.
(global-hl-line-mode 1)

;; Highlight matching brackets and insert by pair.
(show-paren-mode 1)
(electric-pair-mode 1)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Ensure there's a newline at the EOF and show trailing whitespace.
(setq-default require-final-newline t
              show-trailing-whitespace t)

;; Don't show trailing whitespace on some modes.
(defun jm/maybe-hide-trailing-whitespace ()
  (when (derived-mode-p 'term-mode
                        'shell-mode)
    (setq show-trailing-whitespace nil)))

(add-hook 'after-change-major-mode-hook #'jm/maybe-hide-trailing-whitespace)

;; Enable recursive minibuffers and show on which level we are.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Show cursor line and column in the modeline.
(line-number-mode 1)
(column-number-mode 1)

;; Configure graphic display.
(when (display-graphic-p)
  ;; Hide scroll bars.
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)

  ;; Hide tool bar.
  (tool-bar-mode -1)

  ;; Don't blink.
  (blink-cursor-mode -1)

  ;; Set font.
  (set-face-attribute 'default nil :font "Iosevka-13"))

;; Follow symbolic links under version control.
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

;; `use-package' provides a very helpful macro to keep package configuration
;; organized and good with regards to performance.
(straight-use-package 'use-package)

;; Install a package with straight.el when using the `use-package' macro.
(setq straight-use-package-by-default t)

;; Make `use-package' always lazy load features unless told otherwise.
(setq use-package-always-defer t)

;; Keep ~/.emacs.d clean.
(use-package no-littering
  :demand t)

;; Disable menu bar.
(use-package menu-bar
  :straight nil
  :config (menu-bar-mode -1))

;; Use `ibuffer' instead of `list-buffers'.
(use-package ibuffer
  :straight nil
  :bind ([remap list-buffers] . ibuffer))

;; Display available keybindings.
(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-order 'which-key-prefix-then-key-order))

;; Highlight TODO and similar keywords.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Moves lines of text up and down.
(use-package lift
  :straight (:host github :repo "mrshankly/lift.el")
  :bind (("M-<up>" . lift-lines-up)
         ("M-<down>" . lift-lines-down)))

;; Better fuzzy matching.
(use-package flx)

;; Improve minibuffer completion with `ivy' and `counsel'.
(use-package ivy
  :demand t
  :bind
  ("C-x B" . ivy-switch-buffer-other-window)
  :config
  (ivy-mode 1)
  :custom
  (ivy-wrap t)
  (ivy-height 10)
  (ivy-magic-tilde nil)
  (ivy-use-virtual-buffers t)
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (t . ivy--regex-fuzzy))))

(use-package counsel
  :demand t
  :after ivy
  :config (counsel-mode 1))

;; Alternative to `i-search' that uses `ivy'.
(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))

;; Text completion with `company'.
(use-package company
  :hook
  (prog-mode . company-mode)
  :bind (;; Apply the following keybindings only when the completion
         ;; menu is active.
         :map company-active-map

         ;; Use C-n/C-p for next/previous instead of M-n/M-p.
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)

         ;; Tab always completes the current selection instead of just
         ;; the common prefix.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Return only completes the current selection when the user
         ;; has explicitly interacted with company.
         :filter (company-explicit-action-p)
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection))
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10))

;; Version control.
(use-package magit
  :bind (;; As recommended by the documentation.
         ("C-x g" . magit-status)
         ;; Helper to unstage everything while in `magit-mode'.
         :map magit-mode-map
         ("U" . magit-unstage-all))
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil))

;; Icon fonts.
(use-package all-the-icons)

;; Prettier modeline.
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t))

;; Hide minor-modes in a menu.
(use-package minions
  :demand t
  :config (minions-mode 1)
  :custom (minions-mode-line-lighter "--"))

;; Color theme configuration. Create a keybind to toggle between
;; dark and light color themes.
(defvar jm/dark-color-theme 'doom-one)
(defvar jm/light-color-theme 'doom-one-light)
(defvar jm/color-theme jm/light-color-theme)

(defun jm/toggle-color-theme ()
  (interactive)
  (unless (equal (list jm/color-theme) custom-enabled-themes)
    ;; Disable all the currently enabled color themes.
    (mapc #'disable-theme custom-enabled-themes)

    ;; Enable the defined color theme.
    (load-theme jm/color-theme 'no-confirm)

    ;; Update `jm/color-theme' to either dark or light depending on its
    ;; current value.
    (cond
     ((equal jm/color-theme jm/dark-color-theme)
      (setq jm/color-theme jm/light-color-theme))
     ((equal jm/color-theme jm/light-color-theme)
      (setq jm/color-theme jm/dark-color-theme)))))

(use-package doom-themes
  :demand t
  :config (jm/toggle-color-theme)
  :bind ("C-c t" . jm/toggle-color-theme))

;; Programming languages configurations.

;; OCaml - Major mode provided by `tuareg', context sensitive completion with
;; `merlin', indentation with `ocp-indent' and formatting with `ocamlformat'.
(use-package tuareg
  :custom (tuareg-match-patterns-aligned t))

(defun add-ocamlformat-before-save-local-hook ()
  (add-hook 'before-save-hook 'ocamlformat-before-save nil 'local))

;; Use the opam installed `merlin', `ocp-indent' and `ocamlformat'.
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (use-package merlin
      :straight nil
      :hook (tuareg-mode . merlin-mode))
    (use-package ocp-indent
      :straight nil
      :demand t)
    (use-package ocamlformat
      :straight nil
      :demand t
      :hook (tuareg-mode . add-ocamlformat-before-save-local-hook)
      :custom (ocamlformat-enable 'enable-outside-detected-project))))

;; Rust.
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :custom (rust-format-on-save t))

;; SQL - Syntax based indentation.
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; Markdown.
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc -f gfm -t html5 --mathjax --highlight-style pygments -s --quiet"))

;; Eglot - LSP client.
(use-package eglot
  :hook
  (rust-mode . eglot-ensure))

;; Reset garbage collection values when idle for 10 seconds.
(run-with-idle-timer
 10 nil
 (lambda ()
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (setq gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value)))))
