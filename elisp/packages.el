;;; package --- Summary: Uses the use-package package to neatly install all needed packages.
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package arduino-mode
  :ensure t)

(use-package c-mode
  :init
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (setq-default c-basic-offset 4))

(use-package calc
  :init
  (global-set-key (kbd "s-c") 'calc))

(use-package cdlatex
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode))

(use-package company-c-headers
  :ensure t)

(use-package evil
  :ensure t
  :init
  (evil-mode))

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
  (setq flycheck-gcc-args "-std=c99"))

(use-package flyspell)

(use-package function-args
  :ensure t
  :init
  (fa-config-default))

(use-package gdb
  :init
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(use-package ggtags
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
		(ggtags-mode 1))))

  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

(use-package gnuplot
  :ensure t)

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
  :ensure t
  :init
  (multicolumn-global-mode 1))

(use-package multiple-cursors
  :ensure t
  :init
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
   (global-set-key (kbd "C-c a") 'org-agenda)
   (add-hook 'org-mode-hook
	     (lambda () (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l")))))

(use-package powerline-evil
  :ensure t
  :init
  (powerline-evil-center-color-theme))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package seethru
  :ensure t
  :init
  (seethru 90))

(use-package yasnippet
  :ensure t)

(provide 'packages)
;;; packages.el ends here
