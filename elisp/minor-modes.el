;;; minor-modes --- Holds my own minor modes to be read by init.el.
;;; Commentary:

;;; Code:

(define-minor-mode sk/dubcaps-mode
  "Toggle `sk/dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type.
Stolen from http://sriramkswamy.github.io/dotemacs/#orgheadline18."
  :init-value nil
  :lighter (" DC")
  (if sk/dubcaps-mode
      (add-hook 'post-self-insert-hook #'sk/dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'sk/dcaps-to-scaps 'local)))

(define-minor-mode doxygen-mode
  "Update the Doxyfile after each save."
  :lighter " Doxygen"
  (add-hook 'after-save-hook 'update-doxygen))

(add-hook 'text-mode-hook #'sk/dubcaps-mode)
(add-hook 'org-mode-hook #'sk/dubcaps-mode)

(provide 'minor-modes)

;;; minor-modes.el ends here
