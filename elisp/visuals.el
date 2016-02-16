;;; visuals --- Summary: Uses the req-package package to neatly install and configure all packages used to make emacs look pretty.
;;; Commentary:
;;; Code:
(req-package fancy-battery
  (add-hook 'after-init-hook 'fancy-battery-mode))

(req-package color-identifiers-mode
  :diminish color-identifiers-mode
  :config
  (global-color-identifiers-mode))

(req-package monokai-theme
  :init
  (load-theme 'monokai))

(req-package org-bullets
  :init
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(req-package prettify-symbols-mode
  :init
  (global-prettify-symbols-mode 1))

(req-package powerline-evil
  :init
  (powerline-evil-center-color-theme)
  (setq powerline-default-separator nil)
  (display-time-mode nil))

(req-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package seethru
  :init
  (seethru 90))

(provide 'visuals)
;;; visuals.el ends here
