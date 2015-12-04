;;; minor-modes --- Holds my own minor modes to be read by init.el.
;;; Commentary:

;;; Code:

(define-minor-mode event-mode
  "A simple mode for appending a small temporary file to ~/planner.org"
  :lighter " Event"
  :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "C-c s") 'store-event)
			(define-key map (kbd "C-c q") 'quit-event)
			map))

(define-minor-mode notes-mode
  "Used primarily to deal with updating the .dvi files upon each save when typing notes."
  :lighter " Notes"
  (add-hook 'after-save-hook 'my-org-latex-export-to-latex))

(define-minor-mode homework-mode
  "Opens a small buffer to input an assignment for a given class."
  :lighter " Homework"
  :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "C-c s") 'store-homework)
			(define-key map (kbd "C-c q") 'quit-homework)
			map)
  (make-local-variable homework-class))

(define-minor-mode doxygen-mode
  "Update the Doxyfile after each save."
  :lighter " Doxygen"
  (add-hook 'after-save-hook 'update-doxygen))

(provide 'minor-modes)

;;; minor-modes.el ends here
