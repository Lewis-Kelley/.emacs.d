;;; init.el --- Initializes my emacs configs
;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "2bfb9d4dd5c82cd77b3e9fa78fc6ad112d18cb811a5ad1e74f722cb043f3f1db" "395ab8733b275e8d1e2817a997935b5ff9e40ff56e4fb036fa7c342846defbf1" "045251e7ff119a8b065b4cb0072067eb2f297acc44a9e36407e6ff165e35c528" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "ef5f1b745d16d9fbdbf55d624e0a38b8f7f15bc8f87887f1ebaf9d949e3778f2" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen (quote nil))
 '(flycheck-c/c++-gcc-executable "gcc-4.8")
 '(fringe-mode (quote (10 . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(monokai-height-minus-1 1.0)
 '(monokai-height-plus-1 1.0)
 '(monokai-height-plus-2 1.0)
 '(monokai-height-plus-3 1.0)
 '(monokai-height-plus-4 1.0)
 '(monokai-use-variable-pitch t)
 '(org-agenda-files
   (quote
    ("~/notes/MA381.org" "~/homework/CSSE232.org" "~/homework/CSSE230.org" "~/homework/MA275.org" "~/homework/MA381.org" "~/planner.org")))
 '(overflow-newline-into-fringe nil)
 '(package-selected-packages
   (quote
    (smex alda-mode fancy-battery elfeed-org elfeed req-package aggressive-indent evil-magit c-eldoc cheatsheet markdown-mode char-menu srefactor zone-rainbow org-bullets evil-smartparens slime slime-company fireplace dired-filetype-face ess elisp--witness--lisp diredful resize-window elpy multi-line ggtags quickrun fic-mode evil-leader graphene-meta-theme wgrep chess irony emacs-eclim switch-window sublimity rainbow-delimiters evil-mc evil-easymotion company wsd-mode buffer-move multiple-cursors mulitple-cursors flx-ido multicolumn company-c-headers seethru projectile magit powerline-evil monokai function-args arduino-mode package-build shut-up epl git commander f dash s gnuplot flycheck evil yasnippet monokai-theme use-package multi-term cdlatex)))
 '(powerline-height nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-background-face ((t (:background "gray17"))))
 '(elscreen-tab-control-face ((t (:background "black" :foreground "white"))))
 '(elscreen-tab-current-screen-face ((t (:background "gray" :foreground "black"))))
 '(elscreen-tab-other-screen-face ((t (:background "ivory4" :foreground "gray20"))))
 '(fringe ((t (:background "gray17" :foreground "gray17"))))
 '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "salmon2"))))
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "steel blue"))))
 '(powerline-evil-motion-face ((t (:inherit powerline-evil-base-face :background "medium orchid"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "lime green"))))
 '(powerline-evil-operator-face ((t (:inherit powerline-evil-operator-face :background "dodger blue"))))
 '(powerline-evil-replace-face ((t (:inherit powerline-evil-base-face :background "dark red"))))
 '(powerline-evil-visual-face ((t (:inherit powerline-evil-base-face :background "goldenrod")))))

(let ((gc-cons-threshold most-positive-fixnum))
  (setq garbage-collection-messages t)

  ;;
  ;; Set global custom variables
  ;;

  (setq inhibit-splash-screen t)
  (setq diary-file "~/.emacs.d/diary")
  (setq-default tab-width 4 indent-tabs-mode nil)
  (setq default-input-method 'TeX)

  (setq redisplay-dont-pause t ;;Smooth scrolling
        scroll-margin 3
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position 1)

  ;; set the default font
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 90)

  ;; ;; set the fall-back font
  ;; ;; this is critical for displaying various unicode symbols, such as those used in my init-org.el settings
  ;; ;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20
                               :name "Symbola"))

  ;; ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  ;; (setq utf-translate-cjk-mode nil)

  ;; (set-language-environment 'utf-8)
  ;; (setq locale-coding-system 'utf-8)

  ;; ;; set the default encoding system
  ;; (prefer-coding-system 'utf-8)
  ;; (setq default-file-name-coding-system 'utf-8)
  ;; (set-default-coding-systems 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; ;; backwards compatibility as default-buffer-file-coding-system
  ;; ;; is deprecated in 23.2.
  ;; (if (boundp buffer-file-coding-system)
  ;;     (setq buffer-file-coding-system 'utf-8)
  ;;   (setq default-buffer-file-coding-system 'utf-8))

  ;; ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  ;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  (setq flag-colemak 1) ;; Set to 0 to use QWERTY bindings

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

  (put 'downcase-region 'disabled nil)

  (package-initialize)

  ;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle)

  ;;
  ;; load external files
  ;;

  (require 'cc-mode)
  (require 'semantic)
  (require 'req-package)
  (require 'use-package)

  ;; load up all literate org-mode files in this directory
  (org-babel-load-file "~/.emacs.d/emacs.org")

  (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
  (add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (load-library "functions")
  (req-package-finish)
  (load-library "minor-modes")
  (load-library "keys")

  (elscreen-start)
  ;;
  ;; bootup commands
  ;;

  (show-paren-mode)
  (evil-mode)

  ;; turn off garbage collection when using minibuffer
  ;; this may not be working (http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/)
  (add-hook 'minibuffer-setup-hook '(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook '(lambda () (setq gc-cons-threshold 800000)))
  (add-hook 'package-menu-mode-hook '(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'package--post-download-archives-hook '(lambda () (setq gc-cons-threshold 800000))))

(provide 'init)

;;; init.el ends here
