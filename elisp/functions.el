;;; functions.el --- A list of custom functions.
;;; The definitions for all custom functions created in init.el.
;;; Commentary:

;;; Code:

(defvar homework-class)
(defvar notes-class)
(defvar class-list)

(setq class-list (list "PH113" "MA212" "CSSE132" "CSSE220" "CLSK100"))

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
  (save-buffer)
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

(provide 'functions)
;;; functions.el ends here
