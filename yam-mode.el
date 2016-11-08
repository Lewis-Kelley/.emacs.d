;;; yam-mode.el --- Yet Another Modal Mode

;;; Commentary:

;; yam-mode is first and foremost a learning project for me, so don’t look here
;; for good elisp.  My hope is to be able to create something similar to the
;; functionality of modalka or ryo-modal but with the ability to have an
;; arbitrary number of different states akin to Vim’s Normal / Visual modes.

;;; Code:
(defvar yam-maps (list (make-sparse-keymap))
  "The list of all the different keymaps used by yam-mode.")

(defvar yam-cursor-type (list t)
  "Change the cursor when entering a given mode.")

(defvar yam-cursor-color (list "blue")
  "A list of the colors used for each state.")

(defvar yam-states-key (make-hash-table :test 'equal)
  "Hashtable of all the different created states to keep track of their index.")
(puthash "Normal" 0 yam-states-key)

(defvar yam-curr-state 0
  "The index of the currently selected state.")

(defvar yam-mode-map (car yam-maps))

(defun yam-wipe ()
  "Reset all yam variables to their initial default values."
  (interactive)
  (setq yam-maps (list (make-sparse-keymap)))
  (setq yam-cursor-type (list t))
  (setq yam-cursor-color (list "blue"))
  (setq yam-states-key (make-hash-table :test 'equal))
  (puthash "Normal" 0 yam-states-key)
  (setq yam-curr-state 0)
  (setq yam-mode-map (car yam-maps)))

(defun yam-create-state (name)
  "Create a new state with the given NAME."
  (add-to-list 'yam-maps (make-sparse-keymap))
  (add-to-list 'yam-cursor-type t)
  (add-to-list 'yam-cursor-color (list "white"))
  (puthash name (hash-table-count yam-states-key) yam-states-key))

(defun yam-add-binding (name key func)
  "Add a new binding to yam state NAME.
When KEY is pressed in NAME state, run FUNC."
  (let* ((index (- (hash-table-count yam-states-key)
                   (gethash name yam-states-key)
                   1))
         (yam-map (nth index yam-maps)))
    (define-key yam-map (kbd key) func)))

(defun yam-set-state (name)
  "Set the current yam-state to NAME."
  (interactive "sEnter mode: ")
  (setq yam-curr-state (gethash name yam-states-key))
  (setq yam-mode-map (nth (- (hash-table-count yam-states-key) yam-curr-state 1)
                          yam-maps)))

(define-minor-mode yam-mode
  "Toggle `yam-minor-mode'."
  nil " yam" yam-mode-map)

(yam-add-binding "Normal" "n" (lambda () (interactive) (message "Hello YAM!")))
(yam-create-state "Delete")
(yam-add-binding "Delete" "k" (lambda () (interactive) (message "Goodbye YAM!")))
(yam-set-state "Delete")

(provide 'yam-mode)
;;; yam-mode.el ends here
