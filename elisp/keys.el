;;; keys --- Summary:
;;; Commentary:

;;; Code:

;; various custom functions and frequently
;; used programs
(global-set-key (kbd "s-e") #'event)
(global-set-key (kbd "s-n") #'notes)
(global-set-key (kbd "s-r") #'clean-buffer)

;; custom desktop save/load commands
(global-set-key (kbd "C-c d") #'my-desktop-save)
(global-set-key (kbd "C-c r") #'desktop-read)
(global-set-key (kbd "C-x x") #'desktop-save-and-close)
(global-set-key (kbd "C-x C-)") #'toggle-windows-split)

;; dealing with other frames
(global-set-key (kbd "C-c o") #'other-frame)
(global-set-key (kbd "C-c @") #'make-frame-command)
(global-set-key (kbd "C-c #") #'setup-new-frame)
(global-set-key (kbd "C-c )") #'delete-frame)
(global-set-key (kbd "C-c !") #'delete-other-frames)

;; coding
(global-set-key (kbd "C-c /") #'comment-line)
(global-set-key (kbd "C-c C-/") #'comment-or-uncomment-region)
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "C-M-k") #'beginning-of-defun)
(global-set-key (kbd "C-M-j") #'end-of-defun)

;; miscellaneous
(global-set-key (kbd "s-f") #'hs-toggle-hiding)
(global-set-key (kbd "C-x h") #'previous-buffer)
(global-set-key (kbd "C-x l") #'next-buffer)
(global-set-key (kbd "C-x )") #'delete-window)

(provide 'keys)

;;; keys.el ends here
