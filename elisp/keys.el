;;; keys --- Summary:
;;; Commentary:

;;; Code:

;; various custom functions and frequently
;; used programs
;;(global-set-key (kbd "s-e") 'event)
;;(global-set-key (kbd "s-n") 'notes)
(global-set-key (kbd "s-r") 'clean-buffer)

;; custom desktop save/load commands
;;(global-set-key (kbd "C-c d") 'my-desktop-save)
;;(global-set-key (kbd "C-c r") 'desktop-read)
(global-set-key (kbd "C-x x") 'desktop-save-and-close)
(global-set-key (kbd "C-x C-0") 'toggle-windows-split)

;; coding
(global-set-key (kbd "C-c /") 'comment-line)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-M-k") 'beginning-of-defun)
(global-set-key (kbd "C-M-j") 'end-of-defun)

;; shortcuts to common emacs programs
(global-set-key (kbd "s-t") 'eshell)
(global-set-key (kbd "s-b") 'eww)
(global-set-key (kbd "s-c") 'calc)

;; miscellaneous
(global-set-key (kbd "C-x h") 'previous-buffer)
(global-set-key (kbd "C-x l") 'next-buffer)
(global-set-key (kbd "C-x )") 'delete-window)

;; strengthen escape
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'keys)

;;; keys.el ends here
