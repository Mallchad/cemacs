;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;; -*- lexical-binding: t; -*-

;;; cemacs-utility.el --- A collection of useful helper and interactive functions in one tidy package

;;; Code:
(require 'cl-lib)

;; Constants
(defconst cemacs-universal-argument  '(4)
  "Represents the value a single `universal-argument' call passes.
The value essentially a list with the single value of 4"
  )
(defconst cemacs-universal-argument-double  '(16)
  "Represents the value two `universal-argument' calls passes.
The value essentially a list with the single value of 16"
  )
;; Variables
(defvar cemacs-debug-enabled nil
  "If t will enable some useful but obstrusive debugging functionality")
(defvar cemacs-init-complete nil
  "A state to show when the last stage of startup has been passed")
(defvar cemacs-custom-directory-list nil
  "A list of custom directories being used by this config."
  )
(defvar cemacs-custom-directory-clean-list nil
  "A list of custom config directories in which is safe to delete.
This is without without too much trouble when cleaning up,
 especially after a clean Emacs install.
This primarily targets files in `cemacs-custom-directory-list' but
 it isn't actually a pre-requisite"
  )
(defvar cemacs-buffer-tmp-list nil
  "A list of buffers that is safe to semi-clean up at any point.
This helps with cleaning up a bunch of buffers in bulk, particularly
scratch buffers.")
(defvar cemacs-personal-config-file (concat user-emacs-directory "src/personal.el")
  "A custom file that applies personal or situation dependant configuration.

This is applied late in the init process, or can manually be reloaded.
If you do not want to polluted by upstream changes in personal.el you might
want to change this variable.

Due to the nature of when this file is loaded, it is limited to configuration
is safe to be applied after everything else has laoded."
  )
(defvar cemacs-assets-directory (concat user-emacs-directory "assets/")
  "A directory for persistent binary files and stored data for various purpsoes."
  )
;; Custom Functions
(defun cemacs-add-multiple (list-symbol &rest entries)
  "Add each item from ENTRIES to LIST-SYMBOL, skipping duplicates.

The returned value is the modified list.

This is just a shorthand function"
  (interactive)
  (dolist (x-entry entries)
    (add-to-list list-symbol x-entry))
  (symbol-value list-symbol)
  )

(defun cemacs-push-multiple (list-symbol &rest entries)
  "push each item from ENTRIES to LIST-SYMBOL.

The returned value is the modified list.

This is just a shorthand function"
  (when (symbolp list-symbol)
    (dolist (x-entry entries)
      (push x-entry 'list-symbol)))

  (symbol-value list-symbol)
  )

(defmacro cemacs-push-list (list-symbol entries)
  "push each item from the list ENTRIES to LIST-SYMBOL.

The returned value is the modified list.

This is just a shorthand function"
  `(progn
     ;; else
     (dolist (x-entry ,entries)
       (push x-entry ,list-symbol)))
  )

(defalias 'cemacs-add-multiple-to-list 'cemacs-add-multiple
  "Obsolete functin, use the shorthand instead.")
(defun cemacs-append-multiple (list-symbol &rest entries)
  "Append each item in ENTRIES to LIST-SYMBOL, skipping duplicates.

This is just a shorthand function."
  (interactive)
  (dolist (x-entry entries)
    (add-to-list list-symbol x-entry :append))
  )
(defalias 'cemacs-append-multiple-to-list 'cemacs-append-multiple
  "Obsolete function, use the shorthand instead."
  )
(defun cemacs-add-multiple-splicing (list-symbol &rest lists)
  "Add each entry in each list from LISTS to LIST-SYMBOL, skipping duplicates.

The returned value is the modified list.

This is just a shorthand function."
  (interactive)
  (dolist (x-list lists)
    (dolist (x-entry x-list)
      (add-to-list list-symbol x-entry)
      ))
  (symbol-value list-symbol)
  )
(defun cemacs-or-point (&optional pos)
  "Return POS if an integer, or the point

Useful as a tidier version for (or pos (point))
with added integer checking"
  (if (integerp pos)
      pos
    (point))
  )
(defun cemacs-or-line (&optional line)
  "Return LINE if an integer, or the current line

Useful as a tidier version for '(or line (line-number-at-pos))'
with added integer checking"
  (if (integerp line)
      line
    (line-number-at-pos))
  )
(defun cemacs-char-at-pos (&optional pos)
  "Return the char at POS without properties."
  (interactive)
  (let ((position (or pos (point))))
    (buffer-substring-no-properties (point) (+ 1 (point))))
  )
(defun cemacs-org-hide-all-subtrees ()
  "Hide all global subtrees in orgmode."
  (interactive)
  (org-global-cycle 1)
  )
(defmacro cemacs-excursion (&rest form)
  "Do a `save-excursion' and return the point."
  `(let* ((original-point (make-symbol "original-point"))
          (excursion-point (make-symbol "excursion-point")))
     (setq original-point (point))
     ,@form
     (setq excursion-point (point))
     (goto-char original-point)
     excursion-point)
  )
(defun cemacs-startup-time ()
  (float-time
   (time-subtract after-init-time before-init-time))
  )
(defun cemacs-column-at-position (&optional pos)
  "Return the column at POS.

If POS is nil, the point is used instead."
  (save-excursion (goto-char (cemacs-or-point pos)) (current-column))
  )
(defun cemacs-next-tab-stop (&optional pos)
  "Return the column of the next tab stop after POS."
  (indent-next-tab-stop (cemacs-column-at-position (cemacs-or-point pos)))
  )
(defun cemacs-last-tab-stop (&optional pos)
  "Return the column of the last tab stop just before POS."
  (let* ((pos-column (cemacs-column-at-position (cemacs-or-point pos)))
         (preceding-stops (indent-accumulate-tab-stops pos-column))
         (last-stop (or (car (last preceding-stops)) 0)))
    last-stop)
  )
(defun cemacs-whitespace-backward-extent (&optional pos)
  "Return the position of the end of contigious whitespace from POS.

If pos is not an integer or nil, the point is used."
  (cemacs-excursion (goto-char (cemacs-or-point pos)) (cemacs-backward-whitespace))
  )
(defun cemacs-whitespace-forward-extent (&optional pos)
  "Return the position of the end of contigious whitespace from POS

If pos is not an integer or nil, the point is used."
  (cemacs-excursion (goto-char (cemacs-or-point pos)) (cemacs-forward-whitespace))
  )
(defun cemacs-contigious-whitespace-p (pos-1 &optional pos-2)
  "Return t if there is only whitespace chars between POS-1 and POS-2.

POS-2 must be a point after POS-1."
  (cl-assert (> pos-2 pos-1) :show-args "pos-2 does not come after pos-1")
  (>= (cemacs-whitespace-forward-extent pos-1) pos-2)
  )
(defun cemacs-line-first-char-position (&optional pos)
  "Return the position of the first significant character on the line at POS.

If POS is unbound or not an integer, the point will be used instead."
  (cemacs-excursion (goto-char (cemacs-or-point pos)) (beginning-of-line-text))
  )
(defun cemacs-line-last-char-position (&optional pos)
  "Return the position of the last significant character on the line at POS.

If POS is unbound or not an integer, the point will be used instead."
  (cemacs-excursion (goto-char (cemacs-or-point pos))
                    (end-of-line)
                    (cemacs-backward-whitespace))
  )
(defun cemacs-first-char-position-at-line (&optional line)
  "Return the first significant character's position on LINE.

If LINE is unbound or not an integer, the line at point will be used instead."
  (cemacs-excursion (goto-line (cemacs-or-line line)) (beginning-of-line-text))
  )
(defun cemacs-line-text-before-position-p (&optional pos)
  "Returns t if there is non-whitespace text before POS on the same line.

If POS is unbound or or not an integer, the point will be used instead."
  (< (cemacs-line-first-char-position) (cemacs-or-point pos))
  )
(defun cemacs-line-text-after-position-p (&optional pos)
  "Returns t if there is non-whitespace text after POS on the same line.

If POS is unbound or or not an integer, the point will be used instead."
  (> (cemacs-line-first-char-position) (cemacs-or-point pos))
  )
;; End of helper only functions
(defun cemacs-forward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters.

If TRAVERSE-NEWLINES is non-nil, allow travelling to a new line."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-forward " \t\n")
    (skip-chars-forward " \t"))
  )
(defun cemacs-backward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters.

If TRAVERSE-NEWLINES is non-nil, allow travelling to an new line."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-backward " \t\n")
    (skip-chars-backward " \t"))
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
     (point)))
  )
(defun cemacs-delete-word-backwards (&optional mult)
  "Delete characters backward until encountering the beginning of a word.
With argument MULT, repeat this many times.

This command is a reverse of `cemacs-delete-word'"
  (interactive "p")
  (cemacs-delete-word (- (or mult 1)))
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
  (dolist (x-file filelist)
    (if (and (file-exists-p x-file ) (file-regular-p x-file))
        (ignore-errors (find-file-noselect x-file))
      ))
  )
(defun cemacs-find-files-open-all (directory-path)
  "Opens all files in a DIRECTORY-PATH in offscreen buffers."
  (interactive "D")
  (if (file-directory-p directory-path)
      (cemacs-open-files-background
       (directory-files directory-path t)
       ))
  )
(defalias 'cemacs-open-files-in-directory 'cemacs-find-files-open-all
  "An alias for the original name after it was changed to mesh better with emacs-isms")
(defun cemacs-defdir (var-name new-dir &optional associated-var local-only)
  "Define VAR-NAME equal to NEW-DIR a path which is then automatically created.

If there is a direct, existing variable which the path is an intermediate for
than then it can be spceified using ASSOCIATED-VAR.
This also hooks into a directory creation and destruction list, it can be
specified whether or not this directory contains LOCAL-ONLY files that aren't
too important if they are lost between computers when LOCAL-ONLY is non-nil

TODO this *supposed* to clean up deprecated files and put them in a trash
folder when the config fire replaces it with new ones
Although some particular folders like recentf and custom might benefit from
adopting the files instead of cleaning them"
  (interactive)
  (push new-dir cemacs-custom-directory-list)
  (set var-name new-dir)
  ;; A value is supplied to associated-var
  (when (and (boundp 'associated-var)
             (symbol-value 'associated-var))
    (set associated-var new-dir))
  ;; Directory does not already exist
  (if (not (file-exists-p new-dir))
      (make-directory new-dir :recursive)
    (cemacs-warn
     (concat new-dir " has a special file or directory already present!"))
    ))
(defun cemacs-deffile (var-name new-dir &optional associated-var local-only)
  "Define VAR-NAME equal to NEW-DIR a path which is then automatically created.

If there is a direct, existing variable which the path is an intermediate for
than then it can be spceified using ASSOCIATED-VAR.
This also hooks into a directory creation and destruction list, it can be
specified whether or not this directory contains LOCAL-ONLY files that aren't
too important if they are lost between computers when LOCAL-ONLY is non-nil

TODO this *supposed* to clean up deprecated files and put them in a trash
folder when the config fire replaces it with new ones
Although some particular folders like recentf and custom might benefit from
adopting the files instead of cleaning them"
  (interactive)
  (set var-name new-dir)
  (push new-dir cemacs-custom-directory-list)
  ;; A value supplied to associated-var
  (when (and (boundp 'associated-var)
             (symbol-value 'associated-var))
    (set associated-var new-dir))
  (if (not (file-exists-p new-dir))
      (make-empty-file new-dir :recursive)
    (cemacs-warn
     (concat new-dir " has a special file or directory already present!"))
    ))
(defvar cemacs-kill-volatile-buffer-pre-hook nil)
(defun cemacs-buffer-kill-volatile ()
  "Kill the current buffer unconditionally.

This function should be used with care, since it will NOT ask to save work.
This config uses a modified `backup-each-save' to make sure there are always
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
(defun cemacs-revert-buffer (&optional preserve-mode)
  "Modified `revert-buffer' which skips past confirmaiton screen
and fixes buffer-modified flag not being cleared properly.

If PRESERVE-MODE is non-nil or a prefix arg is supplied, try to persist
modes and some buffer-local data through the revert.
WARNING: This will destroy all your unsaved data but will
preserve any undo-data in the buffer.

Sometimes it may be more useful to keep mode data persistent, particularly with
session-like modes like 'lsp-mode'"
  (interactive "P")
  (set-buffer-modified-p nil)
  (revert-buffer nil :skip-confirm (or preserve-mode nil))
  )
(defun cemacs-scratch-buffer-create (&optional new-major-mode)
  "Create a new scratch buffer optionally with a default NEW-MAJOR-MODE."
  (interactive)
  (let* ((scratch-major-mode (if (fboundp new-major-mode)
                                 new-major-mode
                               'emacs-lisp))
         (new-buffer (generate-new-buffer
                      (concat "*scratch-" (symbol-name scratch-major-mode) "*"))
                     ))
    (switch-to-buffer new-buffer)
    (add-to-list 'cemacs-buffer-tmp-list new-buffer)
    (setq-local major-mode 'c++-mode)
    (funcall major-mode))
  )
(defun cemacs-scratch-buffer-create-cpp ()
  "Create a new scratch buffer optionally with the mode 'c++-mode'."
  (interactive)
  (cemacs-scratch-buffer-create 'c++-mode)
  )
(defun cemacs-scratch-buffer-create-c ()
  "Create a new scratch buffer optionally with the default mode 'c-mode'."
  (interactive)
  (cemacs-scratch-buffer-create 'c-mode)
  )
(defun cemacs-scratch-buffer-kill-all ()
  "Kill all scratch buffers created by cemacs."
  (interactive)
  (while (car cemacs-buffer-tmp-list)
    ;; Just have to deal with prompts for saved files here
    (kill-buffer (pop cemacs-buffer-tmp-list)))
  )
(defun cemacs-personal-reload-config ()
  "Reload the bindings found in personal.el"
  (interactive)
  (load cemacs-personal-config-file)
  )

(defun cemacs-set (symbol value)
  "Set value only variable is already bound

This is a slightly more safe and informative abstraction on `set'"
  (if (not (boundp symbol))
      (error "Trying to set a value to an unbound symbol"))
  (set symbol value)
  )
(defun cemacs-buffer-visiblep (buffer-or-name)
  "Return the window a buffer is on or nil if not visible"
  (get-buffer-window buffer-or-name 'visible)
  )

(defun cemacs-bury-compilation (buffer finish-type)
  "Clear the compilation buffer offscreen if finished with no errors occured"
  (let ((compilation-window (get-buffer-window buffer)))
    (when (and (string-match "finished" finish-type)
               compilation-window)
      (with-selected-window compilation-window (quit-window))))
  )

(defun cemacs-compilation-hook (buffer finish-type)
  "Performs several sorted compilation finished hooks

- Goto first error otherwise, goto end of buffer
- Goto end of compilation buffer"
  (let ((compilation-window (get-buffer-window buffer))
        (new-point nil)
        (error-found nil))
    (with-current-buffer buffer
      ;; Covers case where for some reason the compilation buffer is offscreen
      ;; This will set display the location to the buffer's point and not mangle it
      (setq-local switch-to-buffer-preserve-window-point nil)

      (setq error-found (search-forward "error:" nil :noerror)
            new-point error-found)
      (when (not error-found)
        (setq new-point (point-max)))
      (goto-char new-point)

      ;; Buffer is onscreen
      (when compilation-window
        (set-window-point compilation-window new-point))
      ))
  )

(provide 'cemacs-utility)
;;; cemacs-utility.el ends here
