;;; init.el --- Initializes my emacs configs
;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"))
 '(avy-keys (quote (97 114 115 116 100 104 110 101 105 111)))
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "0f98f9c2f1241c3b6227af48dc96e708ec023dd68363edb5d36dc7beaad64c23" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "2bfb9d4dd5c82cd77b3e9fa78fc6ad112d18cb811a5ad1e74f722cb043f3f1db" "395ab8733b275e8d1e2817a997935b5ff9e40ff56e4fb036fa7c342846defbf1" "045251e7ff119a8b065b4cb0072067eb2f297acc44a9e36407e6ff165e35c528" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "ef5f1b745d16d9fbdbf55d624e0a38b8f7f15bc8f87887f1ebaf9d949e3778f2" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(fci-rule-color "#d6d6d6")
 '(flycheck-c/c++-gcc-executable "gcc")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(magit-diff-use-overlays nil)
 '(minimap-dedicated-window nil)
 '(minimap-highlight-line nil)
 '(minimap-mode t)
 '(monokai-height-minus-1 1.0)
 '(monokai-height-plus-1 1.0)
 '(monokai-height-plus-2 1.0)
 '(monokai-height-plus-3 1.0)
 '(monokai-height-plus-4 1.0)
 '(monokai-use-variable-pitch t)
 '(overflow-newline-into-fringe nil)
 '(package-selected-packages
   (quote
    (geiser smex alda-mode fancy-battery elfeed-org elfeed req-package aggressive-indent evil-magit c-eldoc cheatsheet markdown-mode char-menu srefactor zone-rainbow org-bullets evil-smartparens slime slime-company fireplace dired-filetype-face ess elisp--witness--lisp diredful resize-window elpy multi-line ggtags quickrun fic-mode evil-leader graphene-meta-theme wgrep chess irony emacs-eclim switch-window sublimity rainbow-delimiters evil-mc evil-easymotion company wsd-mode buffer-move multiple-cursors mulitple-cursors flx-ido multicolumn company-c-headers seethru projectile magit powerline-evil monokai function-args arduino-mode package-build shut-up epl git commander f dash s gnuplot flycheck evil yasnippet monokai-theme use-package multi-term cdlatex)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(powerline-height nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(breakpoint-disabled ((t (:foreground "red3"))))
 '(fringe ((t (:background "gray17" :foreground "white"))))
 '(minimap-active-region-background ((t (:background "midnight blue"))))
 '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "salmon2"))))
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "steel blue"))))
 '(powerline-evil-motion-face ((t (:inherit powerline-evil-base-face :background "medium orchid"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "lime green"))))
 '(powerline-evil-operator-face ((t (:inherit powerline-evil-operator-face :background "dodger blue"))))
 '(powerline-evil-replace-face ((t (:inherit powerline-evil-base-face :background "dark red"))))
 '(powerline-evil-visual-face ((t (:inherit powerline-evil-base-face :background "goldenrod"))))
 '(sml/col-number ((t (:foreground "deep sky blue" :weight bold))))
 '(sml/filename ((t (:inherit sml/global :foreground "forest green" :weight bold))))
 '(sml/folder ((t (:inherit sml/global :foreground "yellow green" :weight normal))))
 '(sml/git ((t (:foreground "gold2"))))
 '(sml/line-number ((t (:foreground "peru" :weight bold))))
 '(sml/position-percentage ((t (:foreground "hot pink" :weight normal))))
 '(sml/prefix ((t (:inherit sml/global :foreground "RoyalBlue1")))))

(let ((gc-cons-threshold most-positive-fixnum))
  (setq garbage-collection-messages t)

  ;;
  ;; Set global custom variables
  ;;

  (setq diary-file "~/.emacs.d/diary")
    
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

  (setq flag-colemak 1) ;; Set to 0 to use QWERTY bindings

  ;; disable toolbar and the like
;;  (toggle-frame-fullscreen)

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("SC" . "http://joseito.republika.pl/sunrise-commander/"))
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

  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (unless (package-installed-p 'req-package)
    (package-install 'req-package))
  
  (require 'use-package)
  (require 'req-package)

  ;; load up all literate org-mode files in this directory
  (org-babel-load-file "~/.emacs.d/emacs.org")

  (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
  (add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (load-library "functions")
  (load-library "smart-mode-line-gray")
  (req-package-finish)
  (load-library "minor-modes")
  (load-library "keys")

  ;;
  ;; bootup commands
  ;;

  (show-paren-mode)

  ;; turn off garbage collection when using minibuffer
  ;; this may not be working (http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/)
  (add-hook 'minibuffer-setup-hook '(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'minibuffer-exit-hook '(lambda () (setq gc-cons-threshold 800000)))
  (add-hook 'package-menu-mode-hook '(lambda () (setq gc-cons-threshold most-positive-fixnum)))
  (add-hook 'package--post-download-archives-hook '(lambda () (setq gc-cons-threshold 800000))))

(provide 'init)

;;; init.el ends here
