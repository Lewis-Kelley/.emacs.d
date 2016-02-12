;;; packages --- Summary: Uses the use-package package to neatly install all needed packages.
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package avy ;;jump to any visible line
  :ensure t
  :bind
  ("C-;" . avy-goto-char-2)
  ("C-M-;" . avy-goto-line))

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'cc-mode-hook 'aggressive-indent-mode)
  (add-hook 'java-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package bm ;;make bookmarks and cycle through them
  :ensure t
  :bind
  ("s-d" . bm-toggle)
  ("s-j" . bm-next)
  ("s-k" . bm-previous))

(use-package buffer-move
  :disabled t
  :ensure t
  :bind
  (if (= flag-colemak 1)
	  (progn
		("C-S-n" . buf-move-down)
		("C-S-h" . buf-move-left)
		("C-S-e" . buf-move-up)
		("C-S-i" . buf-move-right))
	(progn
	  ("C-S-j" . buf-move-down)
	  ("C-S-h" . buf-move-left)
	  ("C-S-k" . buf-move-up)
	  ("C-S-l" . buf-move-right))))

(use-package c-eldoc
  :disabled t
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
  (add-hook 'c-mode-common-hook '(lambda ()
                                   (add-to-list 'ac-sources 'ac-source-semantic))))

(use-package c++-mode
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook #'yas-minor-mode))

(use-package calc
  :bind
  ("s-c" . calc))

(use-package cdlatex
  :ensure t
  :diminish cdlatex-mode)

(use-package char-menu
  :ensure t
  :init
  (setq char-menu '("‘’" "“”" "…" "⌊⌋" "⋀" "⋁" "√"))
  :bind
  ("M-i" . char-menu))

(use-package cheatsheet ;;Allows you to make a small cheatsheet of different keyboard shortcuts.
  :ensure t
  :config
  (cheatsheet-add
   :group 'Motion
   :key (substitute-command-keys "\\[avy-goto-char-2]")
   :description "Jump to a 2-character sequence.")
  (cheatsheet-add
   :group 'Motion
   :key (substitute-command-keys "\\[avy-goto-line]")
   :description "Jump to a line.")
  (cheatsheet-add
   :group 'Tags
   :key "M-g M-g"
   :description "Jump to the definition of the symbol under the cursor.")
  (cheatsheet-add
   :group 'Tags
   :key "M-g r"
   :description "Jump back to the previous jump origin.")
  (cheatsheet-add
   :group 'Tags
   :key (substitute-command-keys "\\[semantic-complete-jump-local]")
   :description "Prompt for a function, then jump to the definition.")
  (cheatsheet-add
   :group 'Programming
   :key "C-c C-f"
   :description "Toggle code folding.")
  (cheatsheet-add
   :group 'Common
   :key (substitute-command-keys "\\[resize-window]")
   :description "Enter resize-window mode.")
  :bind
  ("C-h h" . cheatsheet-show))

(use-package color-identifiers-mode
  :ensure t
  :diminish color-identifiers-mode
  :config
  (global-color-identifiers-mode))

(use-package company ;;TODO Speed up
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(use-package company-c-headers
  :ensure t)

(use-package diminish ;;hide minor modes from the minibar
  :ensure t)

(use-package diredful ;;colors files in dired mode according to type
  :ensure t
  :config
  (diredful-mode 1))

(use-package dtrt-indent ;;auto-detect indentation on files
  :ensure t
  :init
  (add-hook 'java-mode-hook 'dtrt-indent-mode))

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))

(use-package evil ;;TODO Speed up
  :ensure t
  :config
  (evil-set-initial-state 'blackbox-mode 'emacs)
  (evil-set-initial-state 'package-menu-mode 'motion)
  (evil-set-initial-state 'org-agenda-mode 'motion)

  (setq evil-move-cursor-back nil) ;; Make it so the cursor doesn't pop back when leaving insert mode.

  (if (= flag-colemak 1)
	  (progn
		(colemak-evil-normal-state-remap)
		(colemak-evil-visual-state-remap)
		(colemak-evil-motion-state-remap))
	(progn
	  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
	  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)))

  (use-package alda-mode
	:ensure t
	:config
	(define-key evil-normal-state-map "gp" 'alda-evil-play-region))

  (use-package evil-leader
	:ensure t
	:init
	(setq evil-leader/in-all-states 1)
	(global-evil-leader-mode)
	:config
	(evil-leader/set-leader "SPC")
	(evil-leader/set-key
	  "0" #'delete-window
	  "1" #'delete-other-windows
	  "2" #'split-window-below
	  "3" #'split-window-right
	  "f" #'find-file
	  "d" #'divide-evenly
	  "s" #'save-buffer
	  "o" #'other-window
	  "O" #'switch-window
	  "l" #'ispell-buffer
	  "k" #'goto-last-change
	  "j" #'goto-last-change-reverse
	  "b" #'ido-switch-buffer
	  "r" #'quickrun
	  "x" #'execute-extended-command))

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

  (use-package evil-tabs
	:ensure t
	:diminish evil-tabs-mode
	:init
	(global-evil-tabs-mode t)))

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

(use-package forecast
  :ensure t
  :init
  (setq forecast-latitude 39.4665
		forecast-longitude -87.4132
		forecast-city "Terre Haute"
		forecast-country "USA"
		forecast-units 'us)
  (load (locate-user-emacs-file "forecast-api-key.el"))
  :bind
  ("s-f" . forecast))

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :config
  (add-hook 'c-mode-hook #'ggtags-mode)
  (add-hook 'c++-mode-hook #'ggtags-mode)
  (add-hook 'java-mode-hook #'ggtags-mode)
  (add-hook 'asm-mode-hook #'ggtags-mode)
  (define-key ggtags-mode-map (kbd "M-g M-g") #'ggtags-find-tag-dwim)
  (define-key ggtags-mode-map (kbd "M-g r") #'ggtags-prev-mark))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode 1))

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
  :init
  (add-hook 'hs-minor-mode-hook '(lambda () (diminish 'hs-minor-mode)))
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  :bind
  ("C-c C-f" . hs-toggle-hiding))

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
  (add-hook 'magit-mode-hook
			'(lambda ()
			   (require 'evil-magit)
			   (evil-motion-state)))
  :bind
  ("s-g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai))

(use-package multicolumn
  :disabled t
  :ensure t
  :init
  (multicolumn-global-mode 1))

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
  (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l"))))

  (use-package org-bullets
	:ensure t
	:init
	(setq org-bullets-bullet-list
		  '("◉" "◎" "⚫" "○" "►" "◇"))
	:config
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  :bind
  ("C-c a" . org-agenda))

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

(add-hook 'prog-mode-hook
		  '(lambda ()
			 (require 'quickrun)
			 (quickrun-add-command "c/gcc"
								   '((:command . "gcc")
									 (:exec . ("%c %o -std=gnu11 -o %e %s" "%e")))
								   :override t)))

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

(use-package semantic
  :init
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  :bind
  ("M-g TAB" . semantic-complete-analyze-inline)
  ("M-g g" . semantic-complete-jump-local))

(use-package slime ;; Superior Lisp Interaction Mode for Emacs
  :disabled t
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :disabled t
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package speed-type ;;Typing game
  :disabled t
  :ensure t)

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
  :disabled t
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (add-hook 'prog-mode-hook
			'(lambda ()
			   (yas-reload-all)
			   (yas-minor-mode))))


(provide 'packages)
;;; packages.el ends here
