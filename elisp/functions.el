;;; functions.el --- A list of custom functions.
;;; The definitions for all custom functions created in init.el.
;;; Commentary:

;;; Code:

(defvar homework-class)

(defun quit-event ()
  "Used by event to close the buffer and return key configs to normal."
  (interactive)
  (kill-buffer "event.org")
  (kill-buffer "planner.org")
  (delete-window)
  (shell-command "rm ~/.emacs.d/event.org*")
  (message "Done"))

(defun store-event ()
  "Used by event to add the input data into planner.org."
  (interactive)
  (save-buffer)
  (shell-command "cat ~/.emacs.d/event.org >> ~/planner.org")
  (quit-event)
  (kill-buffer "planner.org"))

(defun event ()
  "Bring up a small buffer which, when closed, add it's contents to the planner."
  (interactive)
  (split-window-below)
  (enlarge-window 16)
  (other-window 1)
  (find-file "~/planner.org")
  (find-file "~/.emacs.d/event.org")
  (event-mode)
  (message "Press C-c s to store this event or C-c q to quit."))

(defun quit-homework ()
  "Quits out of the homework buffer and deletes the homework.org file."
  (interactive)
  (save-buffer)
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

(defun my-org-latex-export-to-latex ()
  "Generate the .tex file for the current 'org-mode' buffer and its .dvi."
  (interactive)
  ;; (message (format "%s" major-mode)))
  (if (string= (format "%s" major-mode) "org-mode")
	  (progn
		(org-latex-export-to-latex)
		(shell-command "sh ~/bin/compile-tex.sh"))
	(message "There's a time and place for everything, but not now.")))

(defun my-desktop-save ()
  "Save the current window to ~/.emacs.d/."
  (interactive)
  (desktop-save "~/.emacs.d/"))

(defun desktop-save-and-close ()
  "Save the current window configuration then quits."
  (interactive)
  (desktop-save "~/.emacs.d/")
  (save-buffers-kill-terminal))

(defun setup-new-frame ()
  "Open a new maximized frame and set the transparency to 90."
  (interactive)
  (toggle-frame-fullscreen)
  (seethru 90))

(defun divide-evenly (windows)
  "Break up the current frame into a collection of evenly spaced number of WINDOWS up to 9."
  (interactive "nHow many windows do you want? ")
  (delete-other-windows)
  (let ((rows 0)
		(cols 0)
		(row-ct 1)
		(col-ct 1))
	(cond ((= windows 1)
		   (setq rows 1)
		   (setq cols 1))
		  ((= windows 2)
		   (setq rows 1)
		   (setq cols 2))
		  ((= windows 3)
		   (setq rows 1)
		   (setq cols 3))
		  ((= windows 4)
		   (setq rows 2)
		   (setq cols 2))
		  ((= windows 5)
		   (setq rows 2)
		   (setq cols 3))
		  ((= windows 6)
		   (setq rows 2)
		   (setq cols 3))
		  ((= windows 7)
		   (setq rows 3)
		   (setq cols 3))
		  ((= windows 8)
		   (setq rows 3)
		   (setq cols 3))
		  ((= windows 9)
		   (setq rows 3)
		   (setq cols 3))
		  ((> windows 9)
		   (return)))

	(while (< col-ct cols)
	  (split-window-horizontally)
	  (setq col-ct (1+ col-ct)))

	(setq col-ct 0)
	(while (< col-ct cols)
	  (setq row-ct 1)
	  (while (< row-ct rows)
		(split-window-below)
		(other-window 1)
		(setq row-ct (1+ row-ct)))
	  (other-window 1)
	  (setq col-ct (1+ col-ct)))

	(balance-windows-area)
	(follow-mode)))

(defun clean-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun toggle-windows-split ()
  "Switch back and forth between one window and a set of windows in a frame.
The idea is to maximize the current buffer, while being able to go back to
the previous split of windows in the frame simply by calling this command again.
Source: https://ignaciopp.wordpress.com/page/6/"
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
	  (progn
		(if (< 1 (count-windows))
			(progn
			  (window-configuration-to-register ?u)
			  (delete-other-windows))
		  (jump-to-register ?u))))
  (my-iswitchb-close))

(defun update-doxygen ()
  "Check to see if there is a Doxyfile in the current directory.
If it does, update the file.  If not, generate a new Doxyfile."
  (interactive)
  (if (file-exists-p "Doxyfile")
	  (shell-command "doxygen Doxyfile > /dev/null")
	(progn
	  (shell-command "doxygen -g > /dev/null; doxygen Doxyfile > /dev/null")
	  (message "Created new Doxyfile"))))

;; Taken from: http://ergoemacs.org/emacs/elisp_eval_lisp_code.html
(defun xah-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(defun colemak-evil-normal-state-remap ()
  "Redo a lot of the normal state keys to accomodate Colemak layout."
  ;; Motion (H is the same)
  (define-key evil-normal-state-map (kbd "n") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "e") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "i") 'evil-forward-char)

  ;; Other
  (define-key evil-normal-state-map (kbd "l") 'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "u") 'evil-insert)
  (define-key evil-normal-state-map (kbd "k") 'evil-search-next)
  (define-key evil-normal-state-map (kbd "K") 'evil-search-previous)
  (define-key evil-normal-state-map (kbd "C-n") (lambda ()
												  (interactive)
												  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-e") (lambda ()
  												  (interactive)
  												  (evil-scroll-down nil))))

(defun colemak-evil-visual-state-remap ()
  "Redo a lot of the visual state keys to accomodate Colemak layout."
  ;; Motion (H is the same)
  (define-key evil-visual-state-map (kbd "n") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "e") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "i") 'evil-forward-char)

  ;; Other
  (define-key evil-visual-state-map (kbd "l") 'undo-tree-undo)
  (define-key evil-visual-state-map (kbd "u") 'evil-insert)
  (define-key evil-visual-state-map (kbd "k") 'evil-search-next)
  (define-key evil-visual-state-map (kbd "K") 'evil-search-previous)
  (define-key evil-visual-state-map (kbd "C-n") (lambda ()
  												  (interactive)
  												  (evil-scroll-up)))
  (define-key evil-visual-state-map (kbd "C-e") (lambda ()
  												  (interactive)
  												  (evil-scroll-down))))

(defun colemak-evil-motion-state-remap ()
  "Redo a lot of the visual state keys to accomodate Colemak layout."
  ;; Motion (H is the same)
  (define-key evil-motion-state-map (kbd "n") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "e") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "i") 'evil-forward-char)

  ;; Other
  (define-key evil-motion-state-map (kbd "l") 'undo-tree-undo)
  (define-key evil-motion-state-map (kbd "u") 'evil-insert)
  (define-key evil-motion-state-map (kbd "k") 'evil-search-next)
  (define-key evil-motion-state-map (kbd "K") 'evil-search-previous)
  (define-key evil-motion-state-map (kbd "C-n") (lambda ()
  												  (interactive)
  												  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-e") (lambda ()
  												  (interactive)
  												  (evil-scroll-down nil))))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(provide 'functions)
;;; functions.el ends here
