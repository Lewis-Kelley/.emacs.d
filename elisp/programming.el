;;; programming --- Summary: Uses the req-package package to neatly install and configure all packages used for programming
;;; Commentary:

;;; Code:
(req-package aggressive-indent
  :init
  (unless (string-equal system-type "windows-nt") ;; aggressive indent kills the windows version a lot, so don't use it
    (add-hook 'programming-mode-hook 'aggressive-indent-mode)))

(req-package c-mode
  :init
  (setq-default c-basic-offset 4)
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  (add-hook 'c-mode-common-hook '(lambda ()
                                   (add-to-list 'ac-sources 'ac-source-semantic))))

(req-package c++-mode
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'yas-minor-mode))

(req-package company-c-headers
  :require company)

(req-package dtrt-indent) ;;auto-detect indentation on files

(req-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode))

(req-package fic-mode ;; hightlights certain keywords like todo
  :diminish fic-mode
  :config
  (add-hook 'prog-mode-hook 'turn-on-fic-mode))

(req-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-gcc-args "-std=gnu99")
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(req-package ggtags
  :diminish ggtags-mode
  :config
  (add-hook 'c-mode-hook #'ggtags-mode)
  (add-hook 'c++-mode-hook #'ggtags-mode)
  (add-hook 'java-mode-hook #'ggtags-mode)
  (add-hook 'asm-mode-hook #'ggtags-mode)
  (define-key ggtags-mode-map (kbd "M-g M-g") #'ggtags-find-tag-dwim)
  (define-key ggtags-mode-map (kbd "M-g r") #'ggtags-prev-mark))

(req-package hs
  :init
  (add-hook 'hs-minor-mode-hook '(lambda () (diminish 'hs-minor-mode)))
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (global-set-key (kbd "C-c C-f") 'hs-toggle-hiding))

(req-package makefile-mode
  :init
  (add-to-list 'auto-mode-alist '("Doxyfile" . makefile-mode)))

(req-package projectile
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(add-hook 'prog-mode-hook
		  '(lambda ()
			 (require 'quickrun)
			 (quickrun-add-command "c/gcc"
								   '((:command . "gcc")
									 (:exec . ("%c %o -std=gnu11 -o %e %s" "%e")))
                                   :override t)))

(req-package semantic
  :init
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (global-set-key (kbd "M-g TAB") 'semantic-complete-analyze-inline)
  (global-set-key (kbd "M-g g") 'semantic-complete-jump-local))

(req-package verilog-mode
  :init
  (setq verilog-tool 'verilog-linter)
  (setq verilog-linter "verilator --lint-only")
  (setq verilog-simulator "verilator")
  (setq verilog-compiler "verilator"))

(provide 'programming)
;;; programming.el ends here
