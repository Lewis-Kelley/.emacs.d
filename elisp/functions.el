;;; functions.el --- A list of custom functions.
;;; The definitions for all custom functions created in init.el.
;;; Commentary:

;;; Code:

(defvar homework-class)
(defvar notes-class)
(defvar class-list)

(let ((min) (hour) (dow))
  (defun start-notes ()
	"Open the correct notes file given the time and start 'notes-mode'."
	(interactive)
	(setq min (nth 1 (decode-time)))
	(setq hour (nth 2 (decode-time)))
	(setq dow (nth 6 (decode-time)))

	(if (= dow 3) ;; if Wednesday
		(progn
		  (if (= hour 14)
			  (if (> min 30)
				  (progn
					(find-file "~/notes/CSSE232.org")
					(notes-mode)
					(return))))
		  (if (= hour 15)
			  (progn
				(find-file "~/notes/CSSE232.org")
				(notes-mode)
				(return))))
	  (if (= hour 16)
		  (if (< min 15)
			  (progn
				(find-file "~/notes/CSSE232.org")
				(notes-mode)
				(return))))
	  )
	(progn
	  (if (or (= dow 1) (= dow 2) (= dow 4)) ;; if Mon, Tue, or Thu
		  (progn
			(if (= hour 9)
				(progn
				  (find-file "~/notes/CSSE230.org")
				  (notes-mode)
				  (return)))
			(if (= hour 10)
				(if (< min 45)
					(progn
					  (find-file "~/notes/CSSE230.org")
					  (notes-mode)
					  (return))))
			(if (= hour 13)
				(progn
				  (if (> min 35)
					  (progn
						(find-file "~/notes/MA381.org")
						(notes-mode)
						(return)))))
			(if (= hour 14)
				(progn
				  (if (< min 25)
					  (progn
						(find-file "~/notes/MA381.org")
						(notes-mode)
						(return))))
			  (progn
				(if (> min 30)
					(progn
					  (find-file "~/notes/CSSE232.org")
					  (notes-mode)
					  (return)))))
			(if (= hour 15)
				(progn
				  (if (< min 20)
					  (progn
						(find-file "~/notes/CSSE232.org")
						(notes-mode)
						(return))))
			  (progn
				(if (> min 25)
					(progn
					  (find-file "~/notes/MA275.org")
					  (notes-mode)
					  (return)))))
			(if (= hour 16)
				(progn
				  (if (< min 15)
					  (progn
						(find-file "~/notes/MA275.org")
						(notes-mode)
						(return)))))
			(if (= dow 5) ;; if Friday
				(progn
				  (if (= hour 13)
					  (progn
						(if (> min 35)
							(progn
							  (find-file "~/notes/MA381.org")
							  (notes-mode)
							  (return)))))
				  (if (= hour 14)
					  (progn
						(if (< min 25)
							(progn
							  (find-file "~/notes/MA381.org")
							  (notes-mode)
							  (return))))
					(progn
					  (if (> min 30)
						  (progn
							(find-file "~/notes/CSSE232.org")
							(notes-mode)
							(return)))))
				  (if (= hour 15)
					  (progn
						(if (< min 20)
							(progn
							  (find-file "~/notes/CSSE232.org")
							  (notes-mode)
							  (return))))
					(progn
					  (if (> min 25)
						  (progn
							(find-file "~/notes/MA275.org")
							(notes-mode)
							(return)))))
				  (if (= hour 16)
					  (progn
						(if (< min 15)
							(progn
							  (find-file "~/notes/MA275.org")
							  (notes-mode)
							  (return)))))))))))
  (message "Not currently in a class (probably)"))

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

(provide 'functions)
;;; functions.el ends here
