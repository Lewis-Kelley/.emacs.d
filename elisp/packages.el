;;; packages --- Summary: Uses the use-package package to neatly install all needed packages.
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package arduino-mode
  :ensure t)

(use-package buffer-move
  :ensure t
  :init
  (global-set-key (kbd "C-S-j") 'buf-move-down)
  (global-set-key (kbd "C-S-h") 'buf-move-left)
  (global-set-key (kbd "C-S-k") 'buf-move-up)
  (global-set-key (kbd "C-S-l") 'buf-move-right))

(use-package c-mode
  :init
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (setq-default c-basic-offset 4))

(use-package c++-mode
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook #'yas-minor-mode)
  (add-hook 'c++-mode-common-hook 'hs-minor-mode))

(use-package calc
  :init
  (global-set-key (kbd "s-c") 'calc))

(use-package centered-window-mode
  :ensure t)

(use-package cdlatex
  :ensure t
  :init
  (add-hook 'cdlatex-mode-hook '(lambda () (diminish 'cdlatex-mode))))

(use-package color-identifiers-mode
  :ensure t
  :init
  (add-hook 'color-identifiers-mode-hook '(lambda () (diminish 'color-identifiers-mode)))
  (global-color-identifiers-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  (add-hook 'company-mode-hook '(lambda () (diminish 'company-mode))))

(use-package company-c-headers
  :ensure t)

(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :init
  (evil-mode)
  (setq evil-move-cursor-back nil)
  (define-key evil-normal-state-map (kbd "C-k") (lambda ()
						  (interactive)
						  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda ()
						  (interactive)
						  (evil-scroll-down nil))))

(use-package evil-easymotion
  :ensure t
  :init
  (evilem-default-keybindings "SPC"))

(use-package evil-mc
  :ensure t
  :init
  (global-evil-mc-mode 1)
  (add-hook 'evil-mc-mode-hook '(lambda() (diminish 'evil-mc-mode))))

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))

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
  :init
  (setq flycheck-gcc-args "-std=gnu99")
  (add-hook 'flycheck-mode-hook '(lambda () (diminish 'flycheck-mode))))

(use-package flyspell
  :init
  (add-hook 'flyspell-mode-hook '(lambda () (dimish 'flyspell-mode))))

(use-package function-args
  :ensure t
  :init
  (fa-config-default))

(use-package gdb)

(use-package gnuplot
  :ensure t
  :init
  (autoload 'gnuplot-mode "gnuplot" t)
  (autoload 'gnuplot-make-buffer "gnuplot" t))

(use-package gnus
  :init
  (global-set-key (kbd "s-g") 'gnus))

(use-package ido
  :init
  (ido-mode t))

(use-package ispell
  :init
  (global-set-key (kbd "C-x l") 'ispell-buffer))

(use-package magit
  :ensure t
  :init
  (setq magit-restore-window-configuration t)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  (global-magit-file-mode))

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai))

(use-package multicolumn
  :disabled t
  :ensure t
  :init
  (multicolumn-global-mode 1))

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
  (global-set-key (kbd "s-t") 'multi-term)
  (setq multi-term-program "/bin/bash"))

(use-package org-mode
   :init
   (setq org-startup-indented t)
   (setq org-agenda-include-diary t)
   (setq org-agenda-start-on-weekday nil)
   (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
   (add-hook 'org-mode-hook 'org-preview-latex-fragment)
   (add-hook 'org-cdlatex-mode-hook (lambda () (diminish 'org-cdlatex-mode)))
   (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))
   (global-set-key (kbd "C-c a") 'org-agenda)
   (add-hook 'org-mode-hook
	     (lambda () (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l")))))

(use-package puml-mode
  :disabled nil
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
	       '("\\.puml\\'" . puml-mode)
	       '("\\.plantuml\\'" . puml-mode)))

(use-package powerline-evil
  :ensure t
  :init
  (powerline-evil-center-color-theme)
  (display-time-mode t))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (add-hook 'projectile-mode-hook '(lambda () (diminish 'projectile-mode))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package seethru
  :ensure t
  :init
  (seethru 90))

(use-package speed-type
  :ensure t)

(use-package undo-tree
  :ensure t
  :init
  (add-hook 'undo-tree-mode-hook '(lambda () (diminish 'undo-tree-mode))))

(use-package xkcd
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'yas-minor-mode-hook '(lambda () (diminish 'yas-minor-mode))))

(provide 'packages)
;;; packages.el ends here
