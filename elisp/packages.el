;;
;; Loads packages for init.el
;;

(use-package flycheck
  :ensure t)

(use-package flyspell
  :bind ("C-x l" . ispell-buffer))

(use-package auctex ;; currently not working
  :disabled t)

(use-package cdlatex
  :ensure t)

(use-package org-mode
  :init
  (setq org-startup-indented t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-start-on-weekday nil)
  (add-hook 'org-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook 'org-preview-latex-fragment)
  (add-hook 'org-mode-hook
	    (lambda () (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l")))))

(use-package c-mode
  :init
  (setq-default c-basic-offset 4)
  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "/bin/bash"))

(use-package gnuplot
  :ensure t)

(use-package yasnippet
  :ensure t)
