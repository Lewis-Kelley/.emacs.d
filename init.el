;;; init.el --- Initializes my emacs configs
;;; Commentary:

;;; Code:

;;
;; Set global custom variables
;;

(setq inhibit-splash-screen t)
(setq diary-file "~/.emacs.d/diary")
(setq-default tab-width 4 indent-tabs-mode t)
(setq-default c-basic-offset 4)
(setq redisplay-dont-pause t ;;Smooth scrolling
      scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)
(add-hook 'after-init-hook '(lambda ()
							  (dolist (f (face-list))
								(when (not (= 'default f))
								  (set-face-attribute f nil :height 1.0)))))

;; disable toolbar and the like
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-fullscreen)

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
	("045251e7ff119a8b065b4cb0072067eb2f297acc44a9e36407e6ff165e35c528" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "ef5f1b745d16d9fbdbf55d624e0a38b8f7f15bc8f87887f1ebaf9d949e3778f2" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(flycheck-c/c++-gcc-executable "gcc-4.8")
 '(org-agenda-files
   (quote
	("~/homework/CSSE232.org" "~/homework/CSSE230.org" "~/homework/MA275.org" "~/homework/MA381.org" "~/schedules/Y1/Q2.org" "~/planner.org")))
 '(package-selected-packages
   (quote
	(elpy multi-line ggtags quickrun fic-mode evil-leader graphene-meta-theme wgrep chess irony emacs-eclim switch-window sublimity rainbow-delimiters evil-mc evil-easymotion company pacmacs puml-mode wsd-mode xkcd autotetris-mode centered-window-mode buffer-move speed-type multiple-cursors mulitple-cursors flx-ido multicolumn company-c-headers seethru projectile magit powerline-evil monokai function-args arduino-mode package-build shut-up epl git commander f dash s gnuplot flycheck evil yasnippet monokai-theme use-package multi-term cdlatex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "white"))))
 '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "salmon2"))))
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "steel blue"))))
 '(powerline-evil-motion-face ((t (:inherit powerline-evil-base-face :background "medium orchid"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "lime green"))))
 '(powerline-evil-operator-face ((t (:inherit powerline-evil-operator-face :background "dodger blue"))))
 '(powerline-evil-replace-face ((t (:inherit powerline-evil-base-face :background "dark red"))))
 '(powerline-evil-visual-face ((t (:inherit powerline-evil-base-face :background "goldenrod")))))

(put 'downcase-region 'disabled nil)

;;
;; load external files
;;

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "~/.emacs.d/selectric-mode")
(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "packages")
(load-library "functions")
(load-library "minor-modes")
(load-library "keys")
(load-library "selectric-mode")

;;
;; bootup commands
;;

(show-paren-mode)

(split-window-right)
(org-agenda-list)
(other-window 1)

(require 'cc-mode)
(require 'semantic)

(yas-reload-all)
(yas-global-mode)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(evil-mode)

(provide 'init)

;;; init.el ends here
