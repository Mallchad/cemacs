;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;;; natural.el --- Alternative more intuitive functions for common emacs operations

;;; Commentary:

;; Emacs has a lot of amazingly useful functions that can supercharge your
;; productivity.
;; However, due to the long an drawn out evolution of emacs, a lot of basic
;; commands have been left with the same quirky behaviour that can be very
;; unintuitive or cumbersome to use, especially for a new-ish emacs user.
;; Or at least, not as intuitive as it could be.
;;
;; natural.el tries to provide slightly alternative of slightly modified,
;; more intuitive behaviour for some native emacs functions, and maybe even
;; in the future non-native functions.
;;
;; The focus of the provided functions is optimizing for determinism,
;; the highest probability and frequency use case.
;;
;; What this means in practice is that behaviour should be consistent.
;; If you have an indentation function, the most useful version would be
;; the one that can initiate the indentation with the cursor anywhere  in
;; the current line, and have have the same behaviour (default behaviour).
;;
;; If the command is expected to have many, situation-dependent behaviours,
;; then the ones that are the most used should be prioritised.
;; For example, with a newline or indent command, in code files, its safe to
;; assume, and more useful, to indent every non-comment line.
;; So the behaviour should lean towards indenting with newlines.

;; Note, currently this file depends on helper functions defined in
;; natural-utility,
;; however, this might change in the future.

;;; Code:
(defmacro natural-excursion (&rest form)
  "Do a `save-excursion' and return the point."
  `(let* ((original-point (make-symbol "original-point"))
          (excursion-point (make-symbol "excursion-point")))
     (setq original-point (point))
     ,@form
     (setq excursion-point (point))
     (goto-char original-point)
     excursion-point)
  )
(defun natural-delete-whitespace ()
  "An alternative to `delete-whitespace-horizontally' which traverses lines."
  (interactive)
  (let ((whitespace-start (natural-excursion (natural-backward-whitespace :cross-lines)))
        (whitespace-end (natural-excursion (natural-forward-whitespace :cross-lines))))
    (delete-region whitespace-start whitespace-end))
  )
(defun natural-one-space ()
  "An alternative to `just-one-space' which traverses lines."
  (interactive)
  (let ((whitespace-start (natural-excursion (natural-backward-whitespace :cross-lines)))
        (whitespace-end (natural-excursion (natural-forward-whitespace :cross-lines))))
    (delete-region whitespace-start whitespace-end)
    (insert-char ?\s))
  )
(defun natural-beginning-of-line ()
  "A version of `beginning-of-line' that acknowledges significant stops.

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
        (original-line (line-number-at-pos (point)))
        ;; Aparent line wrapped line beginning
        (visual-line-beginning
         (save-excursion (beginning-of-visual-line) (point)))
        ;; First significant character of the true line
        (logical-line-beginning
         (save-excursion (beginning-of-line)
                         (natural-forward-whitespace)
                         (point)))
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
          (:default (goto-char visual-line-beginning)
                    (natural-forward-whitespace))
          ))
  )
(defun natural-end-of-line ()
  "A version of `end-of-line' that stops at a visual line end.

This function allows the user to choose if they wish to visit the end
of the visual line, or the end of the real line.

Pressing once will try you to the end of the visual line,
pressing twice will always ensure you end up at the end of the real line."
  (interactive)
  (let ((original-point (point))
        ;; Where 'end-of-visual-line' thinks we should land
        (aparent-visual-line-number (save-excursion
                                      (end-of-visual-line)
                                      (line-number-at-pos)))
        ;; Aparent line wrapped line end
        (visual-line-end (save-excursion (end-of-visual-line) (point)))
        (true-line-end (line-end-position)))
    (cond ((not (= aparent-visual-line-number (line-number-at-pos)))
           ;; Something went wrong and we changed lines
           ;; It was likely invisible text, do a normal end of line
           (end-of-line)
           )
          ((= original-point visual-line-end)
           (goto-char true-line-end)
           )
          (:default (goto-char visual-line-end))
          ))
  )
(defun natural-forward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters.

If TRAVERSE-NEWLINES is non-nil, allow travelling to a new line."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-forward " \t\n")
    (skip-chars-forward " \t"))
  )
(define-obsolete-function-alias 'cemacs-forward-whitespace 'natural-forward-whitespace "cemacs naming culling")
(defun natural-backward-whitespace (&optional traverse-newlines)
  "Move point backwards to the end of the preceding whitespace block.
Each such block may be a single newline, or a sequence of
consecutive space and/or tab characters.

If TRAVERSE-NEWLINES is non-nil, allow travelling to an new line."
  (interactive)
  (if (bound-and-true-p traverse-newlines)
      (skip-chars-backward " \t\n")
    (skip-chars-backward " \t"))
  )
(define-obsolete-function-alias 'cemacs-backward-whitespace 'natural-backward-whitespace "cemacs name culling")
(defun slay-function ()
  "Kill the function surrounding the point.
Emacs' built in 'mark-defun' is used so that is what determines what is
aconsidered a function,
This function is effectively a shorthand of 'mark-defun' 'kill-region'."
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end))
  )
(defun natural-forward-word ()
  "Move the point 1 word block forwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word deletion behaviour, where each command stops after whitespace,
 so you have a little more granular control over what you are deleting,
rather than skipping across lines to delete a word.
e.g. by default (if we assume '|' is the cursor)
\(defun natural-delete-word ()
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
  (let ((original-point (point))
        (following-two-chars-blank
         (and (string-match "[[:blank:]]" (string (char-after)))
              (string-match "[[:blank:]]" (string (char-after (+ (point) 1)))))
         ))
    (cond ((= (line-end-position) (point))
           (natural-forward-whitespace :traverse-newlines)
           )
          ((or following-two-chars-blank (= ?\t (following-char)))
           (natural-forward-whitespace)
           )
          ;; Normal backward word
          (:default (forward-word 1))
          )
    ;; Keep the point inside an input field where applicable
    (constrain-to-field nil (point))
    (point))
  )
(defun natural-backward-word ()
  "Move the point 1 word block backwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word movement behaviour, where each command stops after whitespace,
so you have a little more granular control over how you traverse words,
rather than skipping across lines haphazardly.
e.g. by default (if we assume '|' is the cursor)
\(defun natural-delete-word ()
  |(interactive))

 Running a typical `backward-kill-word' command
would delete all the way up to 'natural-delete-'

Which is both confusing and wholly excessive in nature.

This version will move travese all whitespace backwards when 2 or more
blank/whitespace characters are present, or 1 newline, leaving actual
characters alone.

A call only 1 character away from a 'word' object  will result in using
the `backwards-to-word' command.

This will also attempt to prevent traversing read-only prompt text
This will also move the cursor to the beginning of the line if travesing
lines."
  (interactive)
  (let ((original-point (point))
        (original-line (line-number-at-pos (point)))
        (previous-two-chars-blank
         (and (string-match "[[:blank:]]" (string (char-before)))
              (string-match "[[:blank:]]" (string (char-before (- (point) 1))))))
        )
    ;; Delete whitespace hungrily if at line beginning across lines
    (cond ((= (line-beginning-position) (point))
           (natural-backward-whitespace :traverse-newlines)
           )
          ;; Previous two characters are whitespace/blank
          ((or previous-two-chars-blank (= ?\t (preceding-char)))
           ;; traverse all whitespace upto line beginning
           (natural-backward-whitespace)
           )
          ;; Normal backward word
          (:default
           (backward-word 1)
           (if (not (= original-line (line-number-at-pos (point))))
               ;; Try not to move the cursor too far
               (end-of-line))
           ))
    ;; Keep the point inside an input field where applicable
    (constrain-to-field nil original-point)
    (point)
    ))
(defun natural-delete-word ()
  "Delete a word block backwards, treating whitespace blocks as words.

This is designed to have funcitonality closer to other common editor's
word deletion behaviour, where each command stops after whitespace,
 so you have a little more granular control over what you are deleting,
rather than skipping across lines to delete a word.
e.g. by default (if we assume '|' is the cursor)
\(defun natural-delete-word ()
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
    (natural-forward-word)
    (delete-region original-point (point)))
  )
(defun natural-delete-word-backwards ()
  "Delete a word block backwards, treating whitespace blocks as words.

This command uses `natural-backward-word' to achieve more sane deletion
 behaviour.
It will delete all whitespace forwards when 2 or more blank/whitespace
characters are present, or 1 newline, leaving actual characters alone.

A call only 1 character away from a 'word' object  will result in close to
EMACS `backward-to-char' movement deleting everything between the new and
 old points."
  (interactive)
  (let ((original-point (point)))
    (natural-backward-word)
    (delete-region original-point (point)))
  )
(defun natural-tab-to-tab-stop ()
  "Move following word block to the next tab stop.

The original `tab-to-tab-stop' command has a quirk where existing
whitespace and indentation can upset the tab stop algorithm.  But
only  when  the  cursor  is  not next  to  the  text  immediately
following it.

This behaviour might have intended to move the cursor to the next
tab  stop, relative  to the  cursor  position, since  that is  in
effect Although,  it might  be far more  desirable and  useful to
move  text to  tab stops  follow its  beginning, rather  than the
cursor.

The result  is that the  command will consistently move  the text
immediately following  the cursor  to the  next tab  stop without
needing to move  to the text beginning,  correcting formatting in
the adjecent whitespace as it goes.

If the point is in a line of text, it will ignore
`indent-tabs-mode' and indent using text, correcting any previous
in-line indentaiton as it goes."
  (interactive)
  (natural-forward-whitespace)
  (let* ((last-tab-stop
          (or (car (last (indent-accumulate-tab-stops
                          (current-column))))
              0))
         (next-tab-stop (indent-next-tab-stop (current-column)))
         (last-stop-pos (+ (line-beginning-position) last-tab-stop))
         (whitespace-left-extent (natural-excursion (natural-backward-whitespace)))
         (last-stop-contiguous-whitespace (> last-stop-pos whitespace-left-extent))
         (line-first-char-pos (natural-excursion (beginning-of-line-text)))
         (text-before-point (< line-first-char-pos (point)))
         (indent-tabs-mode indent-tabs-mode))
    ;; Reset whitespace
    (delete-horizontal-space)

    (and abbrev-mode (= (char-syntax (preceding-char)) ?w)
         (expand-abbrev))

    ;; Don't use tabs in the middle of lines
    (when (and text-before-point indent-tabs-mode)
      (setq indent-tabs-mode nil))
    (indent-to next-tab-stop))
  )
(provide 'natural)
