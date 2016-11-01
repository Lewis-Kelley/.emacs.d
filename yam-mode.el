;;; yam-mode.el --- Yet Another Modal Mode

;;; Commentary:

;; yam-mode is first and foremost a learning project for me, so don’t look here
;; for good elisp.  My hope is to be able to create something similar to the
;; functionality of modalka or ryo-modal but with the ability to have an
;; arbitrary number of different states akin to Vim’s Normal / Visual modes.

;;; Code:
(defvar yam-mode-maps (list (make-sparse-keymap))
  "The list of all the different keymaps used by yam-mode.")

(defvar yam-mode-cursor-type (list t)
  "Change the cursor when entering a given mode.")

(defvar yam-mode-cursor-color (list "blue")
  "A list of the colors used for each state.")

(defvar yam-mode-states-key (make-hash-table :test 'equal)
  "Hashtable of all the different created states to keep track of their index.")

(defvar yam-mode-curr-state nil
  "The index of the currently selected state.  Nil if no state selected.")

(defun yam-mode-create-state (name)
  "Create a new state with the given NAME."
  (add-to-list yam-mode-maps (make-sparse-keymap))
  (add-to-list yam-mode-cursor-type t)
  (add-to-list yam-mode-cursor-color (list "white"))
  (add-to-list yam-mode-states-key name))

(defun yam-mode-add-binding (name key func)
  "Add a new binding to yam state NAME.
When KEY is pressed in NAME state, run FUNC."
  (let* ((index (gethash name yam-mode-states-key))
         (yam-mode-map (nth index yam-mode-maps)))
    (define-key yam-mode-map (kbd key) func)))

(defun yam-mode-toggle-state (name)
  "If the current mode is not equal to NAME, set the current state to NAME.
Otherwise, set the current state to nil."
  (let ((index (gethash name yam-mode-states-key)))
    (setq yam-mode-curr-state (if (equal yam-mode-curr-state index)
                                  nil
                                index))))

(provide 'yam-mode)
;;; yam-mode.el ends here
