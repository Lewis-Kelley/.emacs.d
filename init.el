;;
;; required packages
;;
(require 'zone)
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
	     '("article"
	       "\\documentclass{article}"
	       ("\\section{%s}" . "\\section*{%s}")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;
;; global variables
;;
(defvar notes-class)
(defvar homework-class)

;;
;; customized variables
;;

;; default modes for files
(setq auto-mode-alist (cons '("\\.d$" . asm-mode) auto-mode-alist))

;; org-mode
(setq org-startup-indented t) ;; start org mode with the clean style
(setq org-agenda-include-diary t) ;; include diary in agenda view
(setq org-agenda-start-on-weekday nil) ;; set the starting day to be today
(setq inhibit-splash-screen t) ;; keep the startup splash from showing

;; c-mode
(setq-default c-basic-offset 4)

;; calc
(setq calc-gnuplot-name "/home/lewis/.emacs.d/elpa/gnuplot-0.6.0/gnuplot.el")

;; flycheck
(setq flycheck-gcc-language-standard "c99")

;; multi-term
(setq multi-term-program "/bin/bash")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "fe243221e262fe5144e89bb5025e7848cd9fb857ff5b2d8447d115e58fede8f7" default)))
 '(org-agenda-files
   (quote
    ("~/homework/CLSK100.org" "~/homework/MA212.org" "~/planner.org" "~/homework/CSSE132.org" "~/homework/CSSE220.org" "~/homework/PH113.org" "~/schedules/Y1/Q1.org")))
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (java-snippets yasnippet multi-term multi-project use-package cdlatex px nasm-mode iasm-mode auto-complete flycheck gnuplot zenburn-theme zen-and-art-theme xkcd monokai-theme main-line magit flatland-theme color-theme-monokai babcore))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;;
;; interactive function definitions
;;
(defun view-notes (class)
  "Generate and view the summary page for the CLASS."
  (interactive "sWhich class? ")
  (if (file-exists-p (format "~/notes/%s" class))
      (progn
	(if (get-buffer "summary.org")
	    (kill-buffer "summary.org"))
	(shell-command (format "~/bin/compile-orgs.sh ~/notes/%s/" class))
	(if (<= (count-windows) 1)
	    (progn
	      (split-window-right)
	      (other-window 1)))
	(find-file (format "~/notes/%s/summary.org" class))
	(org-shifttab 2))
    (message (format "The class %s does not exist." class))))
  

(defun quit-event ()
  "Used by event to close the buffer and return key configs to normal."
  (interactive)
  (kill-buffer "event.org")
  (delete-window)
  (shell-command "rm ~/.emacs.d/event.org*")
  (message "Done"))

(defun store-event ()
  "Used by event to add the input data into planner.org."
  (interactive)
  (save-buffer)
  (shell-command "cat ~/.emacs.d/event.org >> ~/planner.org")
  (quit-event)
  (kill-buffer "~/planner.org"))

(defun event ()
  "Bring up a small buffer which, when closed, add it's contents to the planner."
  (interactive)
  (split-window-below)
  (enlarge-window 16)
  (other-window 1)
  (find-file "~/.emacs.d/event.org")
  (event-mode)
  (message "Press C-c s to store this event or C-c q to quit."))

(defun quit-notes ()
  "Quits out of the notes buffer and deletes the today.org file."
  (interactive)
  (kill-buffer "today.org")
  (shell-command (format "rm ~/notes/%s/today.org*" notes-class))
  (setq notes-class nil)
  (message "Done"))

(defun store-notes (topics)
  "Asks for the TOPICS covered in the notes, then renames the temporary notes file."
  (interactive "sEnter the topics covered: ")
  (save-buffer)
  (shell-command (format "cp ~/notes/%s/today.org ~/notes/%s/%s.org" notes-class notes-class topics))
  (find-file (format "~/notes/%s/%s.org" notes-class topics))
  (org-shifttab 4)
  (goto-char (point-max))
  (quit-notes))

(defun notes (class)
  "Generate a buffer for taking notes in CLASS, then when writing out, ask for the note's subject."
  (interactive "sWhich class? ")
  (if (file-exists-p (format "~/notes/%s" class))
      (progn
	(setq notes-class class)
	(find-file (format "~/notes/%s/today.org" notes-class))
	(shell-command (format "cd ~/notes/%s" notes-class))
	(notes-mode)
	(message "Press C-c s to save these notes or C-c q to discard."))
    (message (format "The class %s does not exist." class))))

(defun quit-homework ()
  "Quits out of the homework buffer and deletes the homework.org file."
  (interactive)
  (kill-buffer "homework.org")
  (delete-window)
  (shell-command "rm ~/homework/homework.org")
  (setq homework-class nil)
  (message "Done"))

(defun store-homework ()
  "Save the entered homework to the homework file in the previously specified class then quits."
  (interactive)
  (save-buffer)
  (kill-buffer (format "%s.org" homework-class))
  (shell-command (format "cat ~/homework/homework.org >> ~/homework/%s.org" homework-class))
  (quit-homework))

(defun homework (class)
  "Generate a small buffer for adding a homework assignment in the given CLASS."
  (interactive "sWhich class? ")
  (if (file-exists-p (format "~/homework/%s.org" class))
      (progn
	(setq homework-class class)
	(split-window-below)
	(enlarge-window 16)
	(other-window 1)
	(find-file "~/homework/homework.org")
	(homework-mode)
	(message "Press C-c s to save these assignments or C-c q to discard."))
      (message (format "The class %s does not exist." class))))

(defun my-desktop-save ()
  "Saves the current window to ~/.emacs.d/."
  (interactive)
  (desktop-save "~/.emacs.d/"))

(defun desktop-save-and-close ()
  "Saves the current window configuration then quits."
  (interactive)
  (desktop-save "~/.emacs.d/")
  (save-buffers-kill-terminal))

;;
;; minor modes
;;
(define-minor-mode event-mode
  "A simple mode for appending a small temporary file to ~/planner.org"
  :lighter " Event"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c s") 'store-event)
	    (define-key map (kbd "C-c q") 'quit-event)
	    map))

(define-minor-mode notes-mode
  "A simple mode for renaming and storing a temporary notes buffer"
  :lighter " Notes"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c s") 'store-notes)
	    (define-key map (kbd "C-c q") 'quit-notes)
	    map)
  (make-local-variable notes-class))

(define-minor-mode homework-mode
  "Opens a small buffer to input an assignment for a given class."
  :lighter " Homework"
  :keymap (let ((map (make-sparse-keymap)))
	     (define-key map (kbd "C-c s") 'store-homework)
	     (define-key map (kbd "C-c q") 'quit-homework)
	     map)
  (make-local-variable homework-class))

;;
;; hooks
;;

(add-hook 'org-mode-hook 'turn-on-org-cdlatex) ;; start cdlatex for latex snippets
(add-hook 'org-mode-hook 'org-preview-latex-fragment) ;; load latex preview for the whole buffer on loadup

;; launch flyspell mode when loading various modes
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)

;; change flyspell to prog-mode in certain modes
(add-hook 'latex-mode-hook 'flyspell-prog-mode)

(add-hook 'emacs-lisp-mode 'flycheck-mode) ;; launches flycheck when editing elisp


;;
;; custom key configs
;;
(global-set-key (kbd "s-h") 'homework)
(global-set-key (kbd "s-e") 'event)
(global-set-key (kbd "s-n") 'notes)
(global-set-key (kbd "s-v") 'view-notes)
(global-set-key (kbd "s-c") 'calc)
(global-set-key (kbd "s-t") 'multi-term)

(global-set-key (kbd "C-c a") 'org-agenda) ;; bind the agenda key
(global-set-key (kbd "C-x l") 'ispell-buffer) ;; rebind the flyspell to something more useable
(global-set-key (kbd "C-c d") 'my-desktop-save) ;; link to save windows
(global-set-key (kbd "C-c r") 'desktop-read) ;; link to load windows after a save
(global-set-key (kbd "C-x c") 'desktop-save-and-close) ;; save windows then closes

;; set a keyboard shortcut to preview all latex fragments in the buffer
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-c C-x M-l") (kbd "C-u C-u C-c C-x C-l"))))


;; keys for dealing with other frames
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c 2") 'make-frame-command)
(global-set-key (kbd "C-c 0") 'delete-frame)
(global-set-key (kbd "C-c 1") 'delete-other-frames)

;;
;; startup functions
;;
(split-window-right) ;; splits window vertically in prep for agenda
(org-agenda-list) ;; start in agenda view
(other-window 1) ;; switch from the agenda window to the empty window
(zone-when-idle 180) ;; set "screensaver" after 3 min
(show-paren-mode) ;; highlights matching parenthesis

 ;; check OS
(cond ((string-equal system-type "gnu/linux") ; linux
	  (progn
	    (color-theme-monokai)))) ;; switch to the monokai color theme
