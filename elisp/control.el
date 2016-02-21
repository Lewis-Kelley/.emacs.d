;;; control --- Summary: Uses the req-package package to neatly install and configure all packages used for motion and some editing.
;;; Commentary:

;;; Code:

;; (req-package bm ;;make bookmarks and cycle through them
;;   :config
;;   (global-set-key (kbd "s-d") 'bm-toggle)
;;   (if (= flag-colemak 1)
;;       (progn
;;         (global-set-key (kbd "s-n") 'bm-next)
;;         (global-set-key (kbd "s-e") 'bm-previous))
;;     (progn
;;       (global-set-key (kbd "s-j") 'bm-next)
;;       (global-set-key (kbd "s-k") 'bm-previous))))

;; (use-package elscreen ;; makes each window a set of tabs
;;   :ensure t ;; this has to be use-package to make it work for some reason
;;   :init
;;   (elscreen-start)
;;   ;; set up tab commands
;;   (global-set-key (kbd "C-c 0") 'elscreen-kill)
;;   (global-set-key  (kbd "C-c 1") 'elscreen-kill-others)
;;   (global-set-key  (kbd "C-c 2") 'elscreen-create)
;;   (global-set-key  (kbd "C-c o") 'elscreen-next)
;;   (global-set-key  (kbd "C-c h") 'elscreen-previous)
;;   (global-set-key (kbd "C-c s-o") 'elscreen-goto))


;; (req-package evil ;;TODO Speed up
;;   :config
;;   (evil-set-initial-state 'blackbox-mode 'emacs)
;;   (evil-set-initial-state 'package-menu-mode 'motion)
;;   (evil-set-initial-state 'org-agenda-mode 'motion)
;;   (evil-set-initial-state 'elfeed-show-mode 'emacs)
;;   (evil-set-initial-state 'elfeed-search-mode 'emacs)
;;   (evil-set-initial-state 'dired-mode 'emacs)
;;   (evil-set-initial-state 'doc-view-mode 'emacs)

;;   (setq evil-move-cursor-back nil) ;; Make it so the cursor doesn't pop back when leaving insert mode.

;;   (if (= flag-colemak 1)
;;       (progn
;;         (colemak-evil-normal-state-remap)
;;         (colemak-evil-visual-state-remap)
;;         (colemak-evil-motion-state-remap))
;;     (progn
;;       (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
;;       (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))))

;; (req-package evil-leader
;;   :require evil
;;   :init
;;   (setq evil-leader/in-all-states 1)
;;   (global-evil-leader-mode)
;;   :config
;;   (evil-leader/set-leader "SPC")
;;   (evil-leader/set-key
;;     "0" 'delete-window
;;     "1" 'delete-other-windows
;;     "2" 'split-window-below
;;     "3" 'split-window-right
;;     "f" 'find-file
;;     "d" 'divide-evenly
;;     "s" 'save-buffer
;;     "o" 'other-window
;;     "O" 'switch-window
;;     "l" 'ispell-buffer
;;     "L" 'endless/ispell-word-then-abbrev
;;     "k" 'goto-last-change
;;     "j" 'goto-last-change-reverse
;;     "b" 'ido-switch-buffer
;;     "r" 'quickrun
;;     "x" 'execute-extended-command))

;; (req-package evil-mc ;;multiple cursors
;;   :require evil
;;   :diminish evil-mc-mode
;;   :init
;;   (global-evil-mc-mode 1))

;; (req-package evil-smartparens
;;   :require evil
;;   :diminish evil-smartparens-mode
;;   :init
;;   (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

;; (req-package ido
;;   :init
;;   (ido-mode t))

(provide 'control)
;;; control.el ends here
