;;; package --- Summary
;;; Commentary:

;;; Code:

;;
;; Set global custom variables
;;
(setq inhibit-splash-screen t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(org-agenda-files
   (quote
    ("~/homework/CSSE132.org" "~/homework/CSSE220.org" "~/homework/CLSK100.org" "~/homework/MA212.org" "~/homework/PH113.org" "~/planner.org")))
 '(package-selected-packages
   (quote
    (yasnippet monokai-theme zenburn-theme color-theme auctex use-package multi-term cdlatex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; load external files
;;
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "packages")
(load-library "functions")
(load-library "minor-modes")
(load-library "keys")

;;
;; bootup commands
;;
(load-theme 'monokai)
(show-paren-mode)
(split-window-right)
(org-agenda-list)
(other-window 1)


(provide 'init)

;;; init.el ends here
