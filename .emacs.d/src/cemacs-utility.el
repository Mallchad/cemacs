;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrent whatsoever.

;;; cemacs-utility.el --- A collection of useful helper and interactive functions in one tidy package

;;; Code:
;; Dependencies
;; Constants
(defconst cemacs-universal-argument  '(4)
  "Represents the value a single 'universal-argument' call passes.
The value essentially a list with the single value of 4"
  )
(defconst cemacs-universal-argument-double  '(16)
  "Represents the value two 'universal-argument' calls passes.
The value essentially a list with the single value of 16"
  )
;; Variables
(defvar cemacs-custom-directory-list nil
  "A list of custom directories being used by this config."
  )
(defvar cemacs-custom-directory-clean-list nil
  "A list of custom config directories in which is safe to delete.
This is without without too much trouble when cleaning up,
 especially after a clean Emacs install.
This primarily targets files in cemacs-custom-directory-list but
 it isn't actually a pre-requisite"
  )
(defvar cemacs-buffer-tmp-list nil
  "A list of buffers that is safe to semi-clean up at any point.
This helps with cleaning up a bunch of buffers in bulk, particularly
scratch buffers.")
;; Custom Functions
(defun cemacs-add-multiple-to-list (list-symbol &rest entries)
  "Add ecah item in ENTRIES to LIST-SYMBOL, skipping duplicates.

This is just a shorthand function"
  (interactive)
  (dolist (x-entry entries)
    (add-to-list list-symbol x-entry))
  )
(defun cemacs-append-multiple-to-list (list-symbol &rest entries)
  "Append each item in ENTRIES to LIST-SYMBOL, skipping duplicates.

This is just a shorthand function."
  (interactive)
  (dolist (x-entry entries)
    (add-to-list list-symbol x-entry :append))
  )
(defun cemacs-natural-beginning-of-line ()
  "A version of ' that acknowledges significant stops.

This function will move the point in the order
- beginning of visual line
- beginning of logical line, the real line, jumping to first significnat char
- beginning of the true line

The behaviour for this function is borrowed concept of the package crux
\(a Collcetion of Rediciously Useful eXtensions).
This allowed for jumpping to the first significant character, rather than
whitespace, which was generally more useful.
Optionally jumping to the very beginning of the line, if already on the first
significant character.

However, the problem with this function is it ignored visual linnes, which was
really confusing.
Since truncating long lines is really, really, annoying, and not a good
 alternative.
This fixes that problem and visits the beginning of the visual line first."
  (interactive)
  (let ((original-point (point))
        ;; Aparent line wrapped line beginning
        (visual-line-beginning (save-excursion (beginning-of-visual-line) (point)))
        ;; First significant character of the true line
        (logical-line-beginning (save-excursion (beginning-of-line)
                                                (cemacs-forward-whitespace) (point)))
        (true-line-beginning (line-beginning-position))
        )
    (cond ((= original-point visual-line-beginning)
           ;; try jumping to the true line beginning, first significant char
           (goto-char logical-line-beginning)
           )
          ((= original-point logical-line-beginning)
           (beginning-of-line)
           )
          ((= original-point true-line-beginning)
           (goto-char logical-line-beginning)
           )
          ;; Default behaviour
          (t (goto-char visual-line-beginning)
             (cemacs-forward-whitespace))
          ))
  )
(defun cemacs-natural-end-of-line ()
  "A version of 'end-of-line' that stops at a visual line end.

This function allows the user to choose if they wish to visit the end
of the visual line, or the end of the real line.

Pressing once will try you to the end of the visual line,
pressing twice will always ensure you end up at the end of the real line."
  (interactive)
  (let ((original-point (point))
        ;; Aparent line wrapped line end
        (visual-line-end (save-excursion (end-of-visual-line) (point)))
        (true-line-end (line-end-position))
        )
    (cond ((= original-point visual-line-end)
           ;; try jumping to the true line end, first significant char
           (goto-char true-line-end)
           )
          ;; Default behaviour
          (t (goto-char visual-line-end))
          ))
  )
(defun cemacs-forward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-forward " \t\n")
    (skip-chars-forward " \t")
    )
  )
(defun cemacs-backward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-backward " \t\n")
    (skip-chars-backward " \t")
    )
  )
(defun slay-function ()
  "Kill the function surrounding the point.
Emacs' built in 'mark-defun' is used so that is what determines what is
aconsidered a function,
This function is effectively a shorthand of 'mark-defun' 'kill-region'."
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end))
  )
(defun slay-whole-buffer ()
  "Kill the buffer in it's entirety."
  (interactive)
  (kill-region (point-min) (point-max))
  (kill-region (region-beginning) (region-end))
  )
(defun cemacs-activate-mark ()
  "Activates the region between the point and mark.
This is a good alternative to dealing with functions that assume
you use transient mark mode since it lets you run without having to
traverse back to set the region again"
  (interactive)
  (activate-mark)
  )
(defun cemacs-scroll-up-in-place ()
  "Scroll buffer up 1 line without moving cursor position vertically."
  ;; TODO(mallchad) this could accept an arg quite easilly
  ;; TODO(mallchad) function feels like it skips 1 line up or down
  ;; occasionally
  (interactive)
  (forward-line -1)
  (scroll-down-command 1)
  )
(defun cemacs-scroll-down-in-place ()
  "Scroll buffer down 1 line without moving cursor position vertically.

This is a reverse version of 'cemacs-scroll-up-in-place"
  ;; TODO(mallchad) this could easilly be made mirror it's counterpart
  (interactive)
  (forward-line 1)
  (scroll-down-command -1)
  )
(defun cemacs-delete-word (&optional mult)
  "Delete characters forward until encountering the end of a word.
With argument MULT, repeat this that many times, or perform deletion backwards
if negative.

This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word (or mult 1))
     (point))))
(defun cemacs-delete-word-backwards (&optional mult)
  "Delete characters backward until encountering the beginning of a word.
With argument MULT, repeat this many times.

This command is a reverse of `cemacs-delete-word'"
  (interactive "p")
  (cemacs-delete-word (- (or mult 1)))
  )
(defun cemacs-natural-forward-word ()
  "Move the point 1 word block forwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word deletion behaviour, where each command stops after whitespace,
 so you have a little more granular control over what you are deleting,
rather than skipping across lines to delete a word.
e.g. by default (if we assume '|' is the cursor)
\(defun cemacs-natural-delete-word ()
  |(interactive))

 Running a typical `backward-kill-word' command
would delete all the way up to 'natural-delete-'

Which is both confusing and wholly excessive in nature.

This version will delete all whitespace forwards when 2 or more blank/whitespace
characters are present, or 1 newline, leaving actual characters alone.

A call only 1 character away from a 'word' object  will result in 'normal' EMACS
 word deletion.
In fact, this uses a custom word deletion function, so as to not pollute the
kill ring."
  (interactive)
  (let ((original-point (point)))
    ;; Following two characters are whitespace/blank or end of line
    (if (or (and (string-match "[[:blank:]]" (string (char-after)))
                 (string-match "[[:blank:]]" (string (char-after
                                                      (+ (point) 1))
                                                     )))
            (eq (line-end-position) original-point))
        (progn (cemacs-forward-whitespace :traverse-newlines)
               ;; Delete whitespace hungrily upto line beginning
               (delete-region original-point (point))
               )
      ;; Normal delete
      (cemacs-delete-word)
      ))
  )
(defun cemacs-natural-delete-word-backwards ()
  "Delete a word block backwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word deletion behaviour, where each command stops after whitespace,
 so you have a little more granular control over what you are deleting,
rather than skipping across lines to delete a word.
e.g. by default (if we assume '|' is the cursor)
'(defun cemacs-natural-delete-word ()
  |(interactive))'

 Running a typical `backward-kill-word' command
would delete all the way up to 'natural-delete-|'

Which is both confusing and wholly excessive in nature.

This version will delete all whitespace backwards (stopping at newlines)
 when 2 or more blank/whitespace characters are present leaving actual
 characters alone.

Using this command at the beginning of a line will hungrily delete whitespace
until a non-blank/whitespace character is found

A call only 1 character away from a 'word' object  will result in a 'normal'
EMACS word deletion.
In fact, this uses a custom word deletion function, so as to not pollute the
kill ring."
  (interactive)
  (let ((original-point (point)))
    ;; Delete whitespace hungrily if at line beginning across lines
    (cond ((= (line-beginning-position) (point))
           (cemacs-backward-whitespace :traverse-newlines)
           (delete-region original-point (point))
           )
          ;; Previous two characters are whitespace/blank
          ((and (string-match "[[:blank:]]" (string (char-before)))
                (string-match "[[:blank:]]" (string (char-before
                                                     (- (point) 1))
                                                    )))
           ;; Delete whitespace hungrily upto line beginning
           (cemacs-backward-whitespace)
           (delete-region original-point (point))
           )
          ;; Normal delete
          (:default (cemacs-delete-word-backwards))))
  )
(defun cemacs-warn (warning-message)
  "Create a warning event outputting WARNING-MESSAGE duplicate it to the minibuffer."
  (display-warning 'emacs
                   warning-message
                   :debug)
  (message warning-message)
  )
(defun cemacs-find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  ;; Use truename to follow symlinks
  (find-file (file-truename user-init-file))
  )
(defun cemacs-open-files-background (filelist)
  "Open all files in FILELIST in offscreen buffers."
  (cl-loop for x-file in filelist
           do (if (and (file-exists-p x-file ) (file-regular-p x-file))
                  (find-file-noselect x-file)
                )))
(defun cemacs-open-files-in-directory (directory-path)
  "Opens all files in a DIRECTORY-PATH in offscreen buffers."
  (if (file-directory-p directory-path)
      (cemacs-open-files-background
       (directory-files directory-path t)
       ))
  )
(defun cemacs-defdir (var-name new-dir &optional associated-var local-only)
  "Define VAR-NAME equal to NEW-DIR a path which is then automatically created.

If there is a direct, existing variable which the path is an intermediate for
than then it can be spceified using ASSOCIATED-VAR.
This also hooks into a directory creation and destruction list, it can be
specified whether or not this directory contains LOCAL-ONLY files that aren't
too important if they are lost between computers when LOCAL-ONLY is non-nil"
  (interactive)
  (push new-dir cemacs-custom-directory-list)
  (set var-name new-dir)
  ;; A value is supplied to associated-var
  (when (and (boundp 'associated-var)
             (symbol-value 'associated-var))
    (set associated-var new-dir)
    )
  ;; Directory does not already exist
  (if (not (file-exists-p new-dir))
      (make-directory new-dir :recursive)
    (cemacs-warn (concat new-dir " has a special file or directory already present!"))
    )
  )
(defun cemacs-deffile (var-name new-dir &optional associated-var local-only)
  "Define VAR-NAME equal to NEW-PATH a path which is then automatically created.

If there is a direct, existing variable which the path is an intermediate for than
then it can be spceified using ASSOCIATED-VAR.
This also hooks into a directory creation and destruction list, it can be specified whether or not this directory contains LOCAL-ONLY files that aren't too important if
they are lost between computers when LOCAL-ONLY is non-nil"
  (interactive)
  (set var-name new-dir)
  (push new-dir cemacs-custom-directory-list)
  ;; A value supplied to associated-var
  (when (and (boundp 'associated-var)
             (symbol-value 'associated-var))
    (set associated-var new-dir)
    )
  (if (not (file-exists-p new-dir))
      (make-empty-file new-dir :recursive)
    (cemacs-warn (concat new-dir " has a special file or directory already present!"))
    )
  )
(defvar cemacs-kill-volatile-buffer-pre-hook nil)
(defun cemacs-buffer-kill-volatile ()
  "Kill the current buffer unconditionally.

This function should be used with care, since it will NOT ask to save work.
This config uses a modified backup-each-save to make sure there are always
very recent versions of work kept around.
This means the buffer is saved elsewhere before killing, so work is
relatively safe."
  (interactive)
  (run-hooks 'cemacs-kill-volatile-buffer-pre-hook)
  ;; Slight hack required to bypass user promtps,
  ;; since its not offered by the generic 'kill-buffer' function
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer))
  )
(defun cemacs-scratch-buffer-create (&optional new-major-mode)
  "Create a new scratch buffer optionally with a default MAJOR-MODE."
  (interactive)
  (let* ((scratch-major-mode (if (fboundp new-major-mode)
                                 new-major-mode
                               'emacs-lisp))
         (new-buffer (generate-new-buffer
                      (concat "*scratch-" (symbol-name scratch-major-mode) "*")
                      )))
    (switch-to-buffer new-buffer)
    (add-to-list 'cemacs-buffer-tmp-list new-buffer)
    (setq-local major-mode 'c++-mode)
    (funcall major-mode)
    )
  )
(defun cemacs-scratch-buffer-create-cpp (&optional new-major-mode)
  "Create a new scratch buffer optionally with a default MAJOR-MODE."
  (interactive)
  (cemacs-scratch-buffer-create 'c++-mode)
  )
(defun cemacs-scratch-buffer-create-c (&optional new-major-mode)
  "Create a new scratch buffer optionally with a default MAJOR-MODE."
  (interactive)
  (cemacs-scratch-buffer-create 'c-mode)
  )
(defun cemacs-scratch-buffer-kill-all ()
  ""
  (interactive)
  (while (car cemacs-buffer-tmp-list)
    ;; Just have to deal with prompts for saved files here
    (kill-buffer (pop cemacs-buffer-tmp-list)))
  )
(provide 'cemacs-utility)
;;; cemacs-utility.el ends here
