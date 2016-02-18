;;; usability --- Summary: Uses the req-package package to neatly install and configure all packages used for motion and some editing.
;;; Commentary:

;;; Code:
(req-package cdlatex
  :diminish cdlatex-mode
  :require auctex)

(req-package char-menu
  :init
  (setq char-menu '("‘’" "“”" "…" "⌊⌋" "⋀" "⋁" "√"))
  (global-set-key (kbd "M-i") 'char-menu))

(req-package cheatsheet ;;Allows you to make a small cheatsheet of different keyboard shortcuts.
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
  (cheatsheet-add
   :group 'Multiple-Cursors
   :key (substitute-command-keys "\\[evil-mc-make-all-cursors]")
   :description "Create cursors at all matching strings.")
  (cheatsheet-add
   :group 'Multiple-Cursors
   :key (substitute-command-keys "\\[evil-mc-undo-all-cursors]")
   :description "Remove all cursors.")
  (cheatsheet-add
   :group 'Multiple-Cursors
   :key (substitute-command-keys "\\[evil-mc-make-cursor-here]")
   :description "Create a cursor at the current location.")
  (cheatsheet-add
   :group 'Multiple-Cursors
   :key (substitute-command-keys "\\[evil-mc-make-and-goto-next-match]")
   :description "Make a new cursor at the current match and go to the next match.")
  (cheatsheet-add
   :group 'Multiple-Cursors
   :key (substitute-command-keys "\\[evil-mc-skip-and-goto-next-match]")
   :description "Go to the next match.")
  (cheatsheet-add
   :group 'Multiple-Cursors
   :key (substitute-command-keys "\\[evil-mc-make-and-goto-prev-match]")
   :description "Make a new cursor at the current match and go to the previous match.")
  (cheatsheet-add
   :group 'Programming
   :key (substitute-command-keys "\\[flycheck-next-error]")
   :description "Go to the next error in this program.")
  (global-set-key (kbd "C-h h") 'cheatsheet-show))

(req-package company ;;TODO Speed up
  :diminish company-mode
  :config
  (global-company-mode))

(req-package diminish)

(req-package elfeed
  :init
  (global-set-key (kbd "s-l") 'elfeed))

(req-package elfeed-goodies
  :require elfeed
  :init
  (elfeed-goodies/setup))

(req-package elfeed-org
  :require elfeed
  :init
  (elfeed-org))

(req-package flx-ido
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(req-package flyspell)

(req-package forecast
  :init
  (setq forecast-latitude 39.4665
        forecast-longitude -87.4132
        forecast-city "Terre Haute"
        forecast-country "USA"
        forecast-units 'us)
  (load (locate-user-emacs-file "forecast-api-key.el"))
  (global-set-key (kbd "s-f") 'forecast))

(req-package git-gutter-fringe
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode 1))

(req-package magit ;;git porcelain
  :init
  (setq magit-restore-window-configuration t)
  (add-hook 'magit-mode-hook
            '(lambda ()
               (require 'evil-magit)
               (evil-motion-state)))
  (global-set-key (kbd "s-g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(req-package org-mode
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)))

  (global-set-key (kbd "C-c a") 'org-agenda))

(req-package resize-window
  :init
  (global-set-key (kbd "C-S-r") 'resize-window))

(req-package smartparens
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode))

(req-package switch-window
  (global-set-key (kbd "s-o") 'switch-window))

(req-package undo-tree
  :diminish undo-tree-mode)

(req-package yasnippet
  :diminish yas-minor-mode
  :config
  (defvar yas-loaded)
  (setq yas-loaded 0)
  (add-hook 'c-mode-hook 'yas-minor-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'prog-mode-hook
            '(lambda ()
               (cond (= yas-loaded 0) ;; only load yassnippets when opening a prog file the first time
                     (setq yas-loaded 1)
                     (yas-reload-all)))))

(provide 'usability)
;;; usability.el ends here
