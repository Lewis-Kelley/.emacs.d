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
  (setq-default c-basic-offset 4))

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

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-gcc-args "-std=c99"))

(use-package flyspell)

(use-package function-args
  :ensure t
  :init
  (fa-config-default))

(use-package gnuplot
  :ensure t)

(use-package magit
  :ensure t
  :init
  (setq magit-restore-window-configuration t)
  (global-magit-file-mode))

(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai))

(use-package multicolumn
  :ensure t
  :init
  (multicolumn-global-mode 1))

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/bin/bash"))

(use-package powerline-evil
  :ensure t
  :init
  (powerline-evil-center-color-theme))

(use-package seethru
  :ensure t
  :init
  (seethru 90))

(use-package yasnippet
  :ensure t)

(provide 'packages)
;;; packages.el ends here
