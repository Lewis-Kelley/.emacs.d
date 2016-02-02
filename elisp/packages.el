;;; packages --- Summary: Uses the use-package package to neatly install all needed packages.
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package arduino-mode
  :ensure t)

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-M-;" . avy-goto-line))

(use-package bm ;;make bookmarks and cycle through them
  :ensure t
  :bind
  ("s-d" . bm-toggle)
  ("s-j" . bm-next)
  ("s-k" . bm-previous))

(use-package buffer-move
  :ensure t
  :bind
  ("C-S-j" . buf-move-down)
  ("C-S-h" . buf-move-left)
  ("C-S-k" . buf-move-up)
  ("C-S-l" . buf-move-right))

(use-package c-eldoc
  :ensure t
  :config
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode))

(use-package c-mode
  :init
  (setq-default c-basic-offset 4)
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook '(lambda ()
                                   (add-to-list 'ac-sources 'ac-source-semantic))))

(use-package c++-mode
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook #'yas-minor-mode)
  (add-hook 'c++-mode-common-hook 'hs-minor-mode))

(use-package calc
  :bind
  ("s-c" . calc))

(use-package centered-window-mode
  :ensure t)

(use-package cdlatex
  :ensure t
  :diminish cdlatex-mode)

;; (use-package char-menu
;;   :ensure t
;;   :init
;;   (setq char-menu '("‘’" "“”"
;; 					("Math" "≠" "⋂" "⋃")
;; 					("Arrows" "←" "↓" "↑" "→" "↔")
;; 					("Lower Greek" "α" "β" "Y" "δ" "θ" "λ" "μ" "π")
;; 					("Upper Greek" "Δ" "Π" "Σ" "Ω")))
;;   :bind
;;   ("M-i" . #'char-menu))

(use-package cheatsheet ;;Something to keep in mind for the future
  :disabled t)          ;;Allows you to make a small cheatsheet of different keyboard shortcuts.

(use-package color-identifiers-mode
  :ensure t
  :diminish color-identifiers-mode
  :init
  (global-color-identifiers-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(use-package company-c-headers
  :ensure t)

(use-package diminish ;;hide minor modes from the minibar
  :ensure t)

(use-package diredful ;;colors files in dired mode according to type
  :ensure t)

(use-package dtrt-indent ;;auto-detect indentation on files
  :ensure t
  :config
  (add-hook 'java-mode-hook 'dtrt-indent-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package emacs-eclim ;;emacs-eclipse integration
  :disabled t
  :ensure t
  :init
  (require 'eclim)
  (require 'eclimd)
  (global-eclim-mode))

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))

(use-package evil
  :ensure t
  :config
  (evil-set-initial-state 'xkcd-mode 'emacs)
  (evil-set-initial-state 'blackbox-mode 'emacs)
  (evil-set-initial-state 'package-menu-mode 'motion)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (setq evil-move-cursor-back nil)
  (define-key evil-normal-state-map (kbd "C-k") (lambda ()
												  (interactive)
												  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda ()
												  (interactive)
												  (evil-scroll-down nil))))

(use-package evil-leader
  :ensure t
  :init
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
	")" #'delete-window
	"!" #'delete-other-windows
	"@" #'split-window-below
	"#" #'split-window-right
	"f" #'find-file
	"d" #'divide-evenly
	"s" #'save-buffer
	"o" #'other-window
	"O" #'switch-window
	"l" #'ispell-buffer
	"L" '(lambda () (kbd "C-u C-u C-c C-x C-l"))
	"`" #'evil-invert-char
	"k" #'goto-last-change
	"j" #'goto-last-change-reverse
	"b" #'ido-switch-buffer
	"r" #'quickrun
	"x" #'execute-extended-command))

(use-package evil-magit
  :ensure t
  :init
  (setq evil-magit-state 'motion))

(use-package evil-mc ;;multiple cursors
  :ensure t
  :diminish evil-mc-mode
  :init
  (global-evil-mc-mode 1))

(use-package evil-smartparens
  :ensure t
  :diminish evil-smartparens-mode
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package eww
  :bind
  ("s-b" . eww))

(use-package fic-mode
  :ensure t
  :diminish fic-mode
  :config
  (add-hook 'prog-mode-hook #'turn-on-fic-mode))

(use-package flx-ido
  :ensure t
  :init
  (setq gc-cons-threshold 20000000) ;; up the emacs garbage collection threshold
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-gcc-args "-std=gnu99")
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package flyspell
  :diminish flyspell-mode)

(use-package function-args
  :ensure t
  :diminish function-args-mode
  :init
  (fa-config-default))

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :config
  (add-hook 'c-mode-hook #'ggtags-mode)
  (add-hook 'c++-mode-hook #'ggtags-mode)
  (define-key ggtags-mode-map (kbd "M-g M-g") #'ggtags-find-tag-dwim)
  (define-key ggtags-mode-map (kbd "M-g M-s") #'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "M-g M-f") #'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "M-g M-c") #'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "M-g M-u") #'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-g M-r") #'ggtags-find-reference))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode 1))

(use-package gnuplot
  :ensure t
  :init
  (autoload 'gnuplot-mode "gnuplot" t)
  (autoload 'gnuplot-make-buffer "gnuplot" t))

(use-package goto-chg
  :ensure t)

(use-package helm
  :disabled t
  :ensure t
  :init
  (require 'helm-config)
  :config
  (helm-mode 1)
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files))

(use-package hs
  :disabled
  :init
  (add-hook 'hs-minor-mode-hook '(lambda () (diminish 'hs-minor-mode))))

(use-package ido
  :init
  (ido-mode t))

(use-package irony
  :disabled t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook '(lambda ()
                                (define-key irony-mode-map [remap completion-at-point]
                                  'irony-completion-at-point-async)
                                (define-key irony-mode-map [remap complete-symbol]
                                  'irony-completion-at-point-async)))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package ispell
  :disabled t
  :bind
  ("C-x l" . ispell-buffer))

(use-package java-imports ;;currently not working
  :disabled t
  :ensure t
  :init
  (define-key java-mode-map (kbd "M-I") 'java-imports-add-import))

(use-package jdee ;;java IDE
  :disabled t
  :ensure t)

(use-package makefile-mode
  :init
  (add-to-list 'auto-mode-alist '("Doxyfile" . makefile-mode)))

(use-package magit ;;git porcelain
  :ensure t
  :init
  (setq magit-restore-window-configuration t)
  :bind
  ("s-g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

(use-package minimap ;;shows a miniature version of the current file
  :disabled t
  :ensure t)

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai))

(use-package multicolumn
  :disabled t
  :ensure t
  :init
  (multicolumn-global-mode 1))

(use-package multi-line ;;Changes a list of comma-separated items to different formats
  :ensure t
  :bind
  ("s-m" . multi-line))

(use-package multiple-cursors
  :disabled t
  :ensure t
  :init
  (global-set-key (kbd "C-c j") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c k") 'mc/mark-previous-like-this)
  (global-set-key (kbd "s-m") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/bin/bash")
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode 0)))
  :bind
  ("s-t" . multi-term))

(use-package org-mode
  :init
  (setq org-startup-indented t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-ellipsis "…")
  (setq org-src-fontify-natively t)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'org-preview-latex-fragment)
  (add-hook 'org-cdlatex-mode-hook (lambda () (diminish 'org-cdlatex-mode)))
  (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))
  (add-hook 'org-mode-hook
			(lambda ()
			  (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l"))
			  (local-set-key (kbd "C-c 2") 'org-mark-subtree)
			  (local-set-key (kbd "C-c 3") 'org-update-statistics-cookies)
			  (local-unset-key (kbd "C-c @"))
			  (local-unset-key (kbd "C-c #"))))
  :bind
  ("C-c a" . org-agenda))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
		'("◉" "◎" "⚫" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package prettify-symbols-mode
  :init
  (global-prettify-symbols-mode 1))

(use-package puml-mode ;;uml writer
  :ensure t
  :mode
  ("\\.puml\\'" . puml-mode)
  ("\\.plantuml\\'" . puml-mode))

(use-package powerline-evil
  :ensure t
  :init
  (powerline-evil-center-color-theme)
  (setq powerline-default-separator nil)
  (display-time-mode nil))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook #'delete-trailing-whitespace))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package quickrun
  :ensure t
  :config
  (quickrun-add-command "c/gcc"
						'((:command . "gcc")
						  (:exec . ("%c %o -std=gnu11 -o %e %s" "%e")))
						:override t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package resize-window
  :ensure t
  :bind
  ("C-S-r" . resize-window))

(use-package seethru
  :ensure t
  :init
  (seethru 90))

(use-package slime ;; Superior Lisp Interaction Mode for Emacs
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

(use-package speed-type ;;Typing game
  :ensure t)

(use-package srefactor
  :ensure t
  :init
  (semantic-mode 1)
  :config
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

(use-package switch-window
  :ensure t
  :bind
  ("s-o" . switch-window))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package verilog-mode
  :init
  (setq verilog-tool 'verilog-linter)
  (setq verilog-linter "verilator --lint-only")
  (setq verilog-simulator "verilator")
  (setq verilog-compiler "verilator"))

(use-package wgrep
  :ensure t)

(use-package xkcd
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode)

(provide 'packages)
;;; packages.el ends here
