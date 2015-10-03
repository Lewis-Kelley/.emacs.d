;;; package --- Summary
;;; Commentary:

;;; Code:

;;
;; Set global custom variables
;;

(setq inhibit-splash-screen t)
(setq diary-file "~/.emacs.d/diary")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ef5f1b745d16d9fbdbf55d624e0a38b8f7f15bc8f87887f1ebaf9d949e3778f2" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(flycheck-c/c++-gcc-executable "gcc-4.8")
 '(org-agenda-files
   (quote
    ("~/schedules/Y1/Q1.org" "~/homework/CSSE132.org" "~/homework/CSSE220.org" "~/homework/CLSK100.org" "~/homework/MA212.org" "~/homework/PH113.org" "~/planner.org")))
 '(package-selected-packages
   (quote
    (magit powerline-evil monokai function-args arduino-mode package-build shut-up epl git commander f dash s gnuplot flycheck evil yasnippet monokai-theme use-package multi-term cdlatex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; load external files
;;

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "packages")
(load-library "functions")
(load-library "minor-modes")
(load-library "keys")

;;
;; bootup commands
;;

(show-paren-mode)

(split-window-right)
(org-agenda-list)
(other-window 1)
(yas-reload-all)

(provide 'init)

;;; init.el ends here
