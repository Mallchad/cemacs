
;; This is a fix for aggressive-indent causing problems
(require 'aggressive-indent)

;;; Code:
(defun aggressive-indent--indent-if-changed (buffer)
  "Indent any region that changed in BUFFER in the last command loop."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and aggressive-indent-mode aggressive-indent--changed-list)
        (save-excursion
          (save-selected-window
            (aggressive-indent--while-no-input
              (aggressive-indent--proccess-changed-list-and-indent))))))))
(defun aggressive-indent--keep-track-of-changes (l r &rest _)
  "Store the limits (L and R) of each change in the buffer."
  (when aggressive-indent-mode
    (push (list l r) aggressive-indent--changed-list)
    (when (timerp aggressive-indent--idle-timer)
      (cancel-timer aggressive-indent--idle-timer))
    (setq aggressive-indent--idle-timer
          (run-with-idle-timer aggressive-indent-sit-for-time nil #'aggressive-indent--indent-if-changed (current-buffer)))))

(provide 'aggressive-indent-patch)
;;; aggressive-indent-patch.el ends here
