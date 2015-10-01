;;
;; The global keybindings for init.el
;;

(global-set-key (kbd "s-h") 'homework)
(global-set-key (kbd "s-e") 'event)
(global-set-key (kbd "s-n") 'notes)
(global-set-key (kbd "s-v") 'view-notes)
(global-set-key (kbd "s-c") 'calc)
(global-set-key (kbd "s-t") 'multi-term)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x l") 'ispell-buffer)
(global-set-key (kbd "C-c d") 'my-desktop-save)
(global-set-key (kbd "C-c r") 'desktop-read)
(global-set-key (kbd "C-x c") 'desktop-save-and-close)

;; keys for dealing with other frames
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c 2") 'make-frame-command)
(global-set-key (kbd "C-c 0") 'delete-frame)
(global-set-key (kbd "C-c 1") 'delete-other-frames)
