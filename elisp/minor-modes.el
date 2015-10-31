;;
;; Custom minor modes to be loaded by init.el.
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
