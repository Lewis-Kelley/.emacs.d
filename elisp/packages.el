;;; package --- Summary: Uses the use-package package to neatly install all needed packages.
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package evil
  :ensure t
  :init
  (setq evil-replace-state-cursor '("red" box)))

(use-package evil-leader
  :ensure t)

(use-package evil-org
  :ensure t)

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-gcc-args "-std=c99"))

(use-package flyspell)

(use-package auctex ;; currently not working
  :disabled t)

(use-package cdlatex
  :ensure t)

(use-package org-mode
  :init
  (setq org-startup-indented t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-start-on-weekday nil)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'org-preview-latex-fragment)
  (add-hook 'org-mode-hook
	    (lambda () (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l")))))

(use-package c-mode
  :init
  (add-hook 'c-mode-hook 'flycheck-mode)
  (setq-default c-basic-offset 4))
  

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/bin/bash"))

(use-package gnuplot
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package arduino-mode
  :ensure t)

(provide 'packages)
;;; packages.el ends here
