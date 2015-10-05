;;; package --- Summary:
;;; Commentary:

;;; Code:

;; various custom functions and frequently
;; used programs
(global-set-key (kbd "s-h") 'homework)
(global-set-key (kbd "s-e") 'event)
(global-set-key (kbd "s-n") 'notes)
(global-set-key (kbd "s-v") 'view-notes)

;; custom desktop save/load commands
(global-set-key (kbd "C-c d") 'my-desktop-save)
(global-set-key (kbd "C-c r") 'desktop-read)
(global-set-key (kbd "C-x c") 'desktop-save-and-close)

;; dealing with other frames
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c 2") 'make-frame-command)
(global-set-key (kbd "C-c 3") 'setup-new-frame)
(global-set-key (kbd "C-c 0") 'delete-frame)
(global-set-key (kbd "C-c 1") 'delete-other-frames)

;; miscellaneous
(global-set-key (kbd "s-f") 'hs-toggle-hiding)
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(provide 'keys)

;;; keys.el ends here
