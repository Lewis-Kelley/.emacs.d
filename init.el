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
 '(browse-url-browser-function (quote eww-browse-url))
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("69fe2e2056b31b3d09e62cfcc8712b4f8865e394e075c4dbfe30c1964d9f4a16" "abc32ac25b5da830d82e30740c41b188f91acec7bf83bfdf279002d8856d2741" "01399b771710a8fb0859bf383594dd43bbe0fe7ab1f832196359de46a61bc487" "081d1c85358e2400122e6c6632698cb9bcce6a5bd9c18f3553ba7fdcdaa7dae8" "0133460faaa577d4ba8eb669cc4b2be0e7f354c182bf20cc7ede2ef4b2d15b96" "479986546b94b12c57bccc2414f72af0390b1ea868faf2780f52d628e9ff4662" "5a9dfc753c609d7a8011a92fd1c1ae43f31208af2f13127a12f71af291afe5a5" "e42f727d135034e9b6699053a064472df3d4f713223b18c1c56bdf96c99e2f8d" "b9193d5f92d10ef660b263cb13e484c0a97619c4beb4f7f851f438f33848c582" "9be5e80dfb4179bae55815d6ef5b96442a5f1c2ed82b42ef906fd6f9698f97ad" "01b601c721daaa701265331c7375274879209616b33b250588c25d03f5d3d216" "c4f4a47347331f4a1674a25d07b7dabe42feccbcb3ad61c95e23beaa21c3fb42" "d3374987a8cf3698f9e608605fd4a01acc27ed841797ecdca6cfb42c1aa8de1f" "507a247f70ccc2649f9cab12da7bade08fc1776fddccdbf5d06bc83690c28a7d" "6be97df1da3aeaab8c7f17491dd8f328e67c91afa7885ce65f9615a168209d39" "2f7b66342db3951c3c58c3718eb11825b7c0bfee2550bfecf0b8c20aaf1445b4" "ca7284bf1618e60b13bfe291a0505145aef8482a8f303f9d3e4ba0f8a0fa2910" "04aab6a4309af7733f6c0f8d29e27875a4f3c75159f8e28400ddec583390ab27" "103fc7c01370efa3168e2fcf608d4e6729b22a31e76c5d2de5100a13acf3dd56" "5332aec6093b402b6896aaffd6577b6c1f6e8b84982d6a541bf372a12ad4b0cf" "59c51d6c8dacad6714a3a679e75f4c7a9cf4be535ba6eb502de3a3019ed32789" "2671aaaf2a2d6b1eb24da0323e9bbe41cdff34e11300c728f29ef4e62052b83a" default)))
 '(default-frame-alist
    (quote
     ((left-fringe)
      (right-fringe . 0)
      (vertical-scroll-bars))))
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
 '(ledger-reports
   (quote
    (("account" "ledger -f /home/lewis/ledger reg %(account)")
     (#("bal" 0 1
        (idx 1))
      "%(binary) -f %(ledger-file) bal")
     (#("reg" 0 1
        (idx 2))
      "%(binary) -f %(ledger-file) reg")
     (#("payee" 0 1
        (idx 3))
      "%(binary) -f %(ledger-file) reg @%(payee)"))))
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
 '(org-agenda-files (quote ("~/planner.org")))
 '(overflow-newline-into-fringe t)
 '(package-selected-packages
   (quote
    (dad-joke helm ledger-mode org-mime meghanada jdee plantuml-mode org-gcal sx iedit link-hint darkokai-theme counsel swiper ryo-modal geiser smex alda-mode fancy-battery elfeed-org elfeed req-package aggressive-indent evil-magit c-eldoc cheatsheet markdown-mode char-menu srefactor zone-rainbow org-bullets evil-smartparens slime-company fireplace dired-filetype-face ess elisp--witness--lisp diredful resize-window elpy multi-line quickrun fic-mode graphene-meta-theme wgrep chess irony emacs-eclim switch-window sublimity rainbow-delimiters evil-easymotion company wsd-mode buffer-move multiple-cursors mulitple-cursors flx-ido multicolumn company-c-headers seethru projectile magit powerline-evil monokai function-args arduino-mode package-build shut-up epl git commander f dash s gnuplot flycheck monokai-theme use-package multi-term cdlatex)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(powerline-height nil)
 '(safe-local-variable-values (quote ((eval git-gutter-mode nil) (eval 80-column-rule))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sml/full-mode-string " …")
 '(sml/vc-mode-show-backend t)
 '(smtpmail-smtp-server "exchange.rose-hulman.edu")
 '(smtpmail-smtp-service 587)
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
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (req-package-finish)

  ;; load one tex-utils package?
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/tex-utils/")
  (require 'xdvi-search)

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
(put 'narrow-to-region 'disabled nil)
