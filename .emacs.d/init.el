;;; Commentary:
;; Extra Load Paths
(add-to-list 'load-path '"~/.emacs.d/etc/")
;;; Code:
;; Macros
(defmacro WITH_SYSTEM(type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))
(defmacro WHEN_WINDOWS (&rest body)
  "Evalute BODY when the OS is Windows"
  (declare (indent defun))
  (when (eq system-type 'windows-nt)
    (body)
    )
  )
(defmacro WHEN_LINUX (&rest body)
  "Evalute BODY when the OS is Linux"
  (declare (indent defun))
  (when (eq system-type 'gnu/linux)
    (body)
    )
  )
;; Enable Package Repositories
(require 'package)
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
;; Melpa
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)
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
;; Custom Functions
(defun slay-function()
  "Kill the function surrounding the point.
Emacs' built in 'mark-defun' is used so that is what determines what is
aconsidered a function,
This function is effectively a shorthand of 'mark-defun' 'kill-region'."
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end))
  )
(defun slay-whole-buffer()
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
(defun cemacs-scroll-up-in-place()
  "Scroll buffer up 1 line without moving cursor position vertically."
  ;; TODO(mallchad) this could accept an arg quite easilly
  ;; TODO(mallchad) function feels like it skips 1 line up or down
  ;; occasionally
  (interactive)
  (forward-line -1)
  (scroll-down-command 1)
  )
(defun cemacs-scroll-down-in-place()
  "Scroll buffer down 1 line without moving cursor position vertically.

This is a reverse version of 'cemacs-scroll-up-in-place"
  ;; TODO(mallchad) this could easilly be made mirror it's counterpart
  (interactive)
  (forward-line 1)
  (scroll-down-command -1)
  )
(defun cemacs-delete-word(mult)
  "Delete characters forward until encountering the end of a word.
With argument MULT, repeat this that many times, or perform deletion backwards
if negative.

This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word mult)
     (point))))
(defun cemacs-delete-word-backwards(mult)
  "Delete characters backward until encountering the beginning of a word.
With argument MULT, repeat this many times.

This command is a reverse of cemacs-delete-word"
  (interactive "p")
  (cemacs-delete-word (- mult))
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
(defvar cemacs-kill-volatile-buffer-post-hook nil)
(defun cemacs-kill-volatile-buffer()
  "Kill the current buffer unconditionally."
  (interactive)
  (run-hooks 'cemacs-kill-volatile-buffer-pre-hook)
  (when (not (file-exists-p
              (buffer-file-name (current-buffer))
              ))
    (message "Buffer does not have associated file, killing instantly")
    (set-buffer-modified-p nil)
    )
  (kill-buffer (current-buffer))
  (run-hooks 'cemacs-kill-volatile-buffer-post-hook)
  )
;; Configuration
(defun cemacs-cc-mode()
  (interactive)
  (setq tab-width 4)
  (setq-default c-basic-offset 4)
  )
(add-hook 'c-mode-common-hook 'cemacs-cc-mode)
(defun cemacs-cpp-mode()
  (interactive)
  (c-set-style "stroustrup")
  )
(add-hook 'c-mode-common-hook 'cemacs-cpp-mode)
(defun cemacs-c-mode()
  (interactive)
  )
(add-hook 'c-mode-hook 'cemacs-c-mode)
(defun cemacs-elisp-mode()
  )
(add-hook 'emacs-lisp-mode 'cemacs-elisp-mode)
(defun cemacs-prog-mode()
  "Set up buffer for programming."
  (setq
   electric-indent-mode nil)
  (display-line-numbers-mode)
  )
(add-hook 'prog-mode-hook 'cemacs-prog-mode)
(defun cemacs-markdown-mode()
  (interactive)
  (flyspell-mode)
  )
(add-hook 'markdown-mode-hook 'cemacs-markdown-mode)
(defun cemacs-org-mode()
  (interactive)
  (org-indent-mode)
  (flyspell-mode)
  (auto-fill-mode)
  (setq-local org-hide-leading-stars nil)
  (setq-local ws-butler-mode nil)
  )
(add-hook 'org-mode-hook 'cemacs-org-mode)
(defun cemacs-text-mode-hook()
  (flyspell-mode)
  )
(add-hook 'text-mode-hook #'cemacs-org-mode)
(defvar cemacs-init-setup-hook nil
  ;;A normal hook that runs at the end of init setup
  )
(defun cemacs-void-function (&rest)
  "Does absolutely nothing, useful for eating a function call."
  )
;; Setup Functions
;; TODO this *supposed* to clean up deprecated files and put them in a trash
;; folder when the config fire replaces it with new ones
;; Although some particular folders like recentf and custom might benefit from
;; adopting the files instead of cleaning them
;; (defun cemacs-pre-init-setup ()
;; (loop for x-folder in cemacs-directory-clean-list
;; )
(defun cemacs-init-local-frame(frame)
  "Set the frame paramaters for FRAME."
  ;; (split-window-horizontally)
  (set-frame-parameter frame 'menu-bar-lines nil)
  (set-frame-parameter frame 'vertical-scroll-bars nil)
  (set-frame-parameter frame 'horizontal-scroll-bars nil)
  (set-frame-parameter frame 'tool-bar-lines nil)
  (set-frame-parameter frame 'width (x-display-pixel-width))
  (set-frame-parameter frame 'height (x-display-pixel-height))
  )
(add-hook 'after-make-frame-functions 'cemacs-init-local-frame)
(defun cemacs-configure-session-decorations()
  "Set the default frame paramaters and aethetics for the whole Emacs session.
Note this assumes that a frame does not already exist, for frame
configuration see cemacs-init-local-frame"
  (interactive)
  ;;Set Fonts
  (WITH_SYSTEM gnu/linux
    (add-to-list 'default-frame-alist
                 '(font . "MesloLGS NF-13:style=Regular")
                 ))
  (WITH_SYSTEM windows-nt
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-13:style=Regular")
                 ))
  ;;Enable built-in modes
  (global-hl-line-mode)
  ;; Disable Window Decorations
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame))
    )
  (setq-default mode-line-format nil
                vertical-scroll-bar nil
                horizontal-scroll-bar nil
                tool-bar-mode nil
                menu-bar-mode nil
                )
  (fringe-mode (cons 0 0))
  (setq whitespace-style '(trailing tabs tab-mark))
  )
(defun cemacs-vanilla-keys-configure()
  "Set up personal keybinds after initilization."
  (interactive)
  ;; Emacs Control Bindings
  ;; Navigation
  (global-set-key (kbd "C-x r") 'revert-buffer)
  (global-set-key (kbd "M-p") 'cemacs-scroll-up-in-place)
  (global-set-key (kbd "M-n") 'cemacs-scroll-down-in-place)
  (global-set-key (kbd "<C-backspace>") 'cemacs-delete-word-backwards)
  (global-set-key (kbd "C-,") 'pop-to-mark-command)
  ;; Editing Commands
  (global-set-key (kbd "M-d") 'cemacs-delete-word)
  ;; (global-set-key (kbd "C-M-c") 'upcase-char)
  ;; (global-set-key (kbd "C-M-c") 'downcase-char)
  ;; Other
  (global-set-key (kbd "C-x k") #'cemacs-kill-volatile-buffer)
  ;; Org Mode
  (global-set-key (kbd "C-M-#")
                  '(lambda()
                     (interactive)
                     (call-interactively
                      (org-time-stamp cemacs-universal-argument-double 'inactive))
                     ))
  (global-set-key (kbd "M-o") #'ff-find-other-file)
  ;; Unbind Keys
  (unbind-key (kbd "<insert>"))         ; 'overwrite-mode
  (unbind-key (kbd "<insertchar>"))     ; 'overwrite-mode
  )
(defun cemacs-init-setup()
  "Run after-initilization setup."
  (interactive)
  ;;Misc Setup
  (delete-other-windows)
  (setq-default indent-tabs-mode nil
                )
  (setq inhibit-compacting-font-caches t   ;performance improvement
        ;; Mode Setting
        indent-tabs-mode nil               ;use spaces for indendation
        transient-mark-mode nil
        global-hl-line-mode t
        )
  (delete-selection-mode t)             ; delete activated region on typing
  (global-subword-mode t)               ; treat delimited words seperately
  (global-whitespace-mode t)            ; visualize tab characters
  (global-visual-line-mode t)           ; make some commands to operate on visual lines
  ;; Backup
  (setq make-backup-files nil
        backup-by-copying t
        auto-save-default nil)
  ;; Niggles
  (setq recentf-max-saved-items 1000
        )
  (setq-default fill-column 80        ; Change where auto-line wrap fill modes trigger
                )
  ;; Save recentf on every file open
  (add-hook 'find-file-hook #'recentf-save-list)
  ;; (add-hook 'write-file-functions #'recentf-save-list)
  ;; (add-hook 'kill-buffer-hook #'recentf-save-list)
  (fset 'yes-or-no-p 'y-or-n-p ) ; Make all yes or no prompts consistent
  ;; TODO(mallcahd): This is a lazy way compared to finding the right key to unbind
  (fset 'overwrite-mode 'cemacs-void-function) ; Disable pain in the arse insert mode
  ;; Re-enable disabled functions
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; Run Functions
  (cemacs-configure-session-decorations)
  (cemacs-vanilla-keys-configure)
  (run-hooks 'admin-cemacs-init-setup-hook)
  (run-hooks 'cemacs-init-setup-hook)
  )
;; Run early setup to prettify the session
(defun cemacs-early-init ()
  (interactive)
  ;; Variables
  (setq warning-minimum-log-level :debug)
  (cemacs-defdir 'cemacs-var-dir (concat user-emacs-directory "var/"))
  (cemacs-deffile 'cemacs-custom-file (concat cemacs-var-dir "custom.el")
                  'custom-file :local-only)
  (cemacs-defdir 'cemacs-recentf-save-file (concat cemacs-var-dir "recentf")
                 'recentf-save-file :local-only)
  ;; Just prettify the frame whilst waiting for loading
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame))
    )
  (load custom-file)
  )
(cemacs-early-init)
;;Req Package Setup
;; Attempt to install req-package if it's not alread loaded
(when (not (require 'req-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'req-package)
  )
(require 'req-package)
(setq use-package-always-ensure t)
;; Built in Packages
(req-package flyspell
  :config
  ;; Unbind Default keys
  (define-key flyspell-mode-map [(control ?\,)] nil)
  (define-key flyspell-mode-map [(control ?\.)] nil)
  )
(req-package org
  :require
  ;; A cool new package
  org-cliplink
  :config
  (defvar cemacs-org-priority-list
    '(("* *" "top")
      ("ARCHIVE" "bottom")
      ))
  (org-defkey org-mode-map (kbd "C-,") 'pop-to-mark-command)
  ;; Functions
  ;; NOTE(mallchad): Hardcoded section for personal setup, feel free to
  ;; change.
  (defun cemacs-org-load-files ()
    (interactive)
    (cl-loop for x-folder in
             '("~/org"
               "~/notes"
               "~/userdata/org"
               "~/userdata/notes"
               )
             do (cemacs-open-files-in-directory x-folder)
             ))
  (defun cemacs-org-tagwise-comp-func (taglist-left taglist-right)
    (interactive)
    (let ((less-than 'no-match)
          (comp-left (list ""))
          (comp-right (list ""))
          (less-than 'no-priority)
          (comp-left-tag-string "")
          (comp-right-tag-string "")
          )
      (while (or (car taglist-left) (car taglist-right))
        (set 'left-tag (or (pop taglist-left) ""))
        (set 'right-tag (or (pop taglist-right) ""))
        (when (stringp left-tag)
          (push (downcase left-tag) comp-left)
          )
        (when (stringp right-tag)
          (push (downcase right-tag) comp-right)
          ))
      ;; This currently only pushes the "ARCHIVE" tag to the bottom
      ;; Whilst this does the job for general todo lists it is far from ideal
      ;; Here `less-than` actually counter-intuitively means the beginning of the
      ;; buffer, despite it being physically at the top, this is purely down to
      ;; character storing mechanics where in ASCII
      ;; `a == 0 + 97` and  `a == 0 + 122`
      ;; Which leads values closer to a being considered "lower" and being sorted
      ;; closer to the numerically lower buffer position 0, the very top left
      (cond ((member "archive" comp-left)
             (set 'less-than nil)
             )
            ((member "archive" comp-right)
             (set 'less-than t)
             ))
      ;; Blindly comapre tags alphanumerically
      (while (car comp-left)
        (set 'comp-left-tag-string
             (concat comp-left-tag-string (pop comp-left)
                     )))
      (while (car comp-right)
        (set 'comp-right-tag-string
             (concat comp-right-tag-string (pop comp-right)
                     )))
      (if (eq less-than 'no-priority)
          (set 'less-than
               (string-collate-lessp comp-left-tag-string comp-right-tag-string))
        )
      less-than
      )
    )
  (defun cemacs-org-sort-taglist-get ()
    (or (org-get-tags) (list ""))
    )
  (defun cemacs-org-sort-entries ()
    (interactive)
    ;; (set 'point-start (point))
    ;; (beginning-of-buffer cemacs-universal-argument)
    (org-global-cycle) ; Hide all subtrees
    (org-sort-entries nil ?f 'cemacs-org-sort-taglist-get 'cemacs-org-tagwise-comp-func)
    ;; (set-window-point (get-buffer-window (current-buffer)) point-start)
    )
  ;; This is an absolutely disgusting hack I found online and it needs to go.
  (custom-set-faces '(org-checkbox ((t (:foreground nil :inherit org-todo)))))
  (defface org-checkbox-todo-text
    '((t (:inherit org-todo)))
    "Face for the text part of an unchecked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-todo-text prepend))
   'append)
  (defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
   'append)
  )
;; External Packages
(req-package async
  :config
  (async-bytecomp-package-mode t)
  )
(req-package color-theme-sanityinc-tomorrow
  :config
  (if (and (boundp 'cemacs-selected-sanityinc-theme)
           (symbol-value 'cemacs-selected-sanityinc-theme))
      (load-theme cemacs-selected-sanityinc-theme)
    (load-theme 'sanityinc-tomorrow-bright t)
    )
  )
(req-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode)
  :config
  )
(req-package all-the-icons
  :require async
  :config
  (when (not (boundp 'cemacs-all-the-icons-fonts-installed))
    (all-the-icons-install-fonts 'skip)
    (customize-save-variable 'cemacs-all-the-icons-fonts-installed t))
  )
(req-package avy
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char)
  (global-set-key (kbd "M-r") 'avy-pop-mark)
  (setq avy-highlight-first t
        avy-background t
        )
  ;; The colours picked here are designed to maximize readabiltiy
  ;;
  ;; This is achieved by reducing 'glare' or 'eye-catching' colours
  ;; of the background by using dark and slightly desatured colours
  ;; The resulting effect is that the actual character part is the
  ;; most attenting grabbing part, which is exactly the pint of avy -
  ;; going where you're looking
  ;;
  ;; However rather annoyingly the text / boxes are spaced closed together,
  ;; making them bleed into each other
  ;; This is likely impossible to fix in GNU emacs fonts aside
  ;; Increasing font size works. But is not optimal
  (set-face-attribute 'avy-lead-face nil :background '"dark red" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "#1d1d62" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "#2a3418" :foreground "white")
  )
(req-package backup-each-save
  :config
  ;; TODO Stop being lazy and turn this is into a custom function
  (defun backup-each-save ()
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)
                            ))
        (let ((bfn (buffer-file-name)))
          (when (and (or backup-each-save-remote-files
                         (not (file-remote-p bfn)))
                     (funcall backup-each-save-filter-function bfn)
                     (or (not backup-each-save-size-limit)
                         (<= (buffer-size) backup-each-save-size-limit)))
            (copy-file bfn (backup-each-save-compute-location bfn) t t t)))))
  :hook
  (after-save . backup-each-save)
  (cemacs-kill-volatile-buffer-pre . backup-each-save)
  )
(req-package beacon
  :hook
  (cemacs-init-setup . beacon-mode)
  :config
  (setq beacon-color "gold"
        beacon-blink-when-point-moves-vertically 1    ;; blink if the line changes
        beacon-blink-when-point-moves-horizontally 20
        )
  )
;;; A robust, prettified calender framework
(req-package calfw
  :require calfw-org
  )
(req-package centaur-tabs
  :hook
  (cemacs-init-setup . centaur-tabs-mode)
  :bind
  ("<C-tab>" . centaur-tabs-forward)
  ("<C-S-tab>" . centaur-tabs-backward)
  ("<C-iso-lefttab>" . centaur-tabs-backward)
  :init
  ;;Misc Settings
  (setq centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-close-button nil
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-style "bar"
        centaur-tabs-set-bar t
        centaur-tabs-bar 'over
        centaur-tabs-modified-marker "*"
        centaur-tabs-mode t
        )
  :config
  ;;Create Uniform Tabbar Appearance
  (centaur-tabs-headline-match)
  ;;Group tabs by projects
  (centaur-tabs-group-by-projectile-project)
  )
(req-package company
  :hook
  (prog-mode . company-mode)
  :config
  ;; Apply company-tng patch
  (company-tng-configure-default)
  (setq company-require-match 'never
        company-idle-delay 0
        )
  (define-key company-tng-map (kbd "C-p") nil)
  (define-key company-tng-map (kbd "C-n") nil)
  (WHEN_WINDOWS
    ;; Slow down company-idle-delay so it doesn't choke emacs
    (setq company-idle-delay 0.02)
    )
  )
(req-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-x e") 'cemacs-find-user-init-file)
  (global-set-key (kbd "C-j") 'crux-top-join-line)
  (global-set-key (kbd "C-x C-o") 'crux-swap-windows)
  (global-set-key (kbd "C-o") 'crux-smart-open-line-above)
  )
(req-package cmake-mode
  )
(req-package csharp-mode
  :require csproj-mode
  :hook
  (csharp-mode . cemacs-cc-mode)
  :config
  (defun cemacs-csharp-mode()
    (interactive)
    (setq tab-width 4)
    (setq-default c-basic-offset 4)
    (c-set-style "c#")
    )
  (add-hook 'csharp-mode-hook 'cemacs-csharp-mode)
  )
(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )
(req-package fireplace
  :config
  ;; Variable Config
  (setq fireplace-smoke-on t)
  ;; Paramter hack to allow the fireplace to resize properly
  (defun fireplace--update-locals-vars (&optional nil-window)
    "Update `fireplace' local variables."
    (setq fireplace--bkgd-height (- (floor (window-height (get-buffer-window fireplace-buffer-name))) 1)
          fireplace--bkgd-width  (- (round (window-width (get-buffer-window fireplace-buffer-name))) 1)
          fireplace--flame-width (min fireplace--bkgd-height (round (/ fireplace--bkgd-width 2.5)))
          fireplace--flame-pos fireplace-flame-pos))
  (defun cemacs-fireplace-visit (&optional window)
    (interactive)
    (if (windowp  window)
        (select-window window)
      (set 'window (selected-window))
      )
    (unless (get-buffer "*fireplace*")
      (fireplace)
      )
    (set-window-buffer window (get-buffer "*fireplace*"))
    )
  (defun cemacs-fireplace-split-window-right ()
    (interactive)
    (cemacs-fireplace-visit (split-window-right))
    )
  (defun cemacs-fireplace-split-window-below ()
    (interactive)
    (cemacs-fireplace-visit (split-window-below))
    )
  (global-set-key (kbd "C-x 3") 'cemacs-fireplace-split-window-right)
  (global-set-key (kbd "C-x 2") 'cemacs-fireplace-split-window-below)
  (add-hook 'after-make-frame-functions 'cemacs-fireplace-visit)
  )
(req-package flycheck
  :require flycheck-inline
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-inline-mode)
  (global-flycheck-mode . global-flycheck-inline-mode)
  :config
  )
(req-package free-keys
  )
(req-package god-mode
  :config
  )
(req-package helm
  :after hydra
  :require
  helm-flycheck
  helm-projectile
  helm-rg
  helm-swoop
  :init
  ;; Completley hide helm header
  (fset 'helm-display-mode-line #'ignore)
  (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil)
    )
  (add-hook 'helm-after-initialize-hook
            (defun hide-mode-line-in-helm-buffer ()
              "Hide mode line in `helm-buffer'."
              (with-helm-buffer
                (setq-local mode-line-format nil)
                )
              )
            )
  :config
  (require 'helm-config)
  (helm-mode)
  ;;Helm minibuffer config
  (setq helm-autoresize-mode t
        helm-display-header-line nil
        helm-header-line-space-before-prompt nil
        helm-autoresize-max-height 30   ;Always take up 30% the screen
        helm-autoresize-min-height 30
        helm-split-window-inside-p t    ;Shows helm window in current buffer
        helm-swoop-split-with-multiple-windows helm-split-window-inside-p
        helm-mode-line-string nil
        helm-use-frame-when-more-than-two-windows nil
        )
  ;; ;Helm minibuffer config
  ;;TODO(mallchad) Need to reduce the size of the space after helm source
  ;; Don't use helm's own displaying mode line function
  (set-face-attribute 'helm-source-header nil
                      :height 1.1
                      :foreground "dark cyan"
                      )
  ;; Keybinds
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; Swap Action and Completion Buttons
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  ;; TODO(mallchad) need to find more elegant keybinds
  ;;Query
  (defhydra hydra-query (:color blue)
    "query for"
    ("s" helm-swoop-without-pre-input "String")
    ("m" helm-mini "Mini")
    ("b" helm-bookmarks "Bookmarks")
    ("p" helm-projectile "Projectile")
    ("f" helm-flycheck "Flycheck")
    ("r" helm-rg "Ripgrep")
    ("C-p" helm-projectile-rg "Projectile rg")
    )
  (global-set-key (kbd "C-q") 'hydra-query/body)
  )
(req-package highlight-parentheses
  :hook
  (cemacs-init-setup . global-highlight-parentheses-mode)
  :config
  (setq hl-paren-background-colors '("gray")
        hl-paren-colors '("black")
        hl-paren-highlight-adjacent nil
        )
  )
(req-package hl-todo
  :config
  (global-hl-todo-mode)
  )
(req-package hungry-delete
  :config
  (global-hungry-delete-mode)
  ;; The default hungry-delete behaviour deletes all whitespace backwards
  ;; This is annoying since the point of using it was to rmeove extraneous
  ;; whitespace, not to remove all of it
  ;; The new behaviour leaves 1 space for words so its more comfortable and
  ;; similar to other editors
  (set hungry-delete-join-reluctantly t)
  )
(req-package hydra
  :config
  (defhydra hydra-slayer (:color blue)
    "kill shortcuts"
    ("x" slay-function "function(x)" :color red)
    ("l" kill-whole-line "whole line" :color red)
    ("b" slay-whole-buffer "whole buffer")
    )
  (global-set-key (kbd "C-s") 'hydra-slayer/body)
  ;; (defhydra hydra-emacs (:color blue :hint nil))
  )
(req-package lsp-mode
  :after company
  :hook
  (c++-mode . lsp)
  (c-mode . lsp)
  (csharp-mode . lsp)

  :config
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-clients-clangd-args '("--header-insertion-decorators=0" "--background-index")
        )
  ;; Performance Tuning
  (setq
   ;; Source for performance improvements https://emacs-lsp.github.io/lsp-mode/page/performance/
   ;; Adjust gc-cons-threshold. The default setting is too low for lsp-mode's
   ;; needs due to the fact that client/server communication generates
   ;; a lot of memory/garbage.
   gc-cons-threshold 100000000
   ;; Increase the amount of data which Emacs reads from the process.
   ;; Again the emacs default is too low 4k considering that the some of the
   ;; language server responses are in 800k - 3M range.
   read-process-output-max (* 1024 1024) ;; 1mb
   )
  (define-key lsp-mode-map (kbd "M-o") #'lsp-clangd-find-other-file)
  )
(req-package lsp-ui
  :after
  lsp-mode
  flycheck
  :hook
  (lsp-ui-mode . (lambda() (lsp-ui-doc-mode -1)))
  :config
  (setq-default lsp-ui-doc-position 'top)
  )
(req-package magit
  :config
  )
(req-package lua-mode
  :after
  lsp
  :hook
  (lua-mode . lsp)
  :config
  ;; TODO(mallchad) should automatically extract emmy-lua for supported systems
  )
(req-package multiple-cursors
  :after hydra
  :config
  (global-set-key (kbd "M-m") 'mc/edit-lines)
  ;; (defhydra hydra-multicurses (global-map "C-m" :color red)
  ;;   "Multiple Cursors"
  ;;   ("q" hydra--body-exit "quit")
  ;;   ("e" mc-edit-lines "edit lines")
  ;;   ("a" mc/mark-all-like-this "mark all")
  ;;   ("r" mc/mark-all-in-region "mark in region")
  ;;   )
  )
(req-package json-mode
  ;; A minor mode to aid with json editing
  )
(req-package omnisharp
  ;; :hook
  ;; (csharp-mode . omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  )
(req-package org-noter
  :config
  )
(req-package org-super-agenda
  :config
  )
(req-package origami
  :require lsp-origami
  :config
  ;; TODO(mallchad) need to setup keybinds for this package
  )
(req-package projectile
  :require
  flycheck-projectile
  :hook
  (cemacs-init-setup . projectile-mode)
  :config
  )
(req-package rainbow-blocks
  :config
  (set-face-attribute 'rainbow-blocks-depth-1-face nil :foreground "white")
  (set-face-attribute 'rainbow-blocks-depth-2-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-blocks-depth-3-face nil :foreground "deep pink")
  (set-face-attribute 'rainbow-blocks-depth-4-face nil :foreground "chartreuse")
  (set-face-attribute 'rainbow-blocks-depth-5-face nil :foreground "deep sky blue")
  (set-face-attribute 'rainbow-blocks-depth-6-face nil :foreground "yellow")
  (set-face-attribute 'rainbow-blocks-depth-7-face nil :foreground "orchid")
  (set-face-attribute 'rainbow-blocks-depth-8-face nil :foreground "spring green")
  (set-face-attribute 'rainbow-blocks-depth-9-face nil :foreground "sienna1")
  )
(req-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "white")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "deep pink")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "chartreuse")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "deep sky blue")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "yellow")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "orchid")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "spring green")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "sienna1")
  )
(req-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  )
(req-package restart-emacs
  :config
  (global-set-key (kbd "C-x M-a") 'restart-emacs)
  )
(req-package smartparens
  :hook
  (cemacs-init-setup . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  ;;Disable Emacs Lisp Quote Pairs
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil)
  ;; TODO(mallchad) need  to setup bindings for this package
  ;; TODO(mallchad) need to make angled bracket pair for cc modes
  ;; TODO(mallchad) need to evalue a potentiol workaround to make hybrid
  ;; sexps spill over lines to make it useful for cc mode development
  )
(req-package smooth-scrolling
  :hook
  (cemacs-init-setup . smooth-scrolling-mode)
  :config
  (setq scroll-conservatively 0)
  )
(req-package smart-yank
  :config
  )
(req-package sublimity
  :hook
  (cemacs-init-setup . sublimity-mode)
  :config
  (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  ;;(require 'sublimity-map) ;; experimental

  (setq sublimity-scroll-weight 7
        sublimity-scroll-drift-length 0
        ;;      sublimity-map-size 10
        ;;      sublimity-map-fraction 0.3
        ;;      sublimity-map-text-scale -7
        ;;      sublimity-map-set-delay 1
        )
  )
(req-package treemacs
  :require lsp-treemacs
  :hook
  (treemacs-mode . (lambda() (text-scale-adjust -2)))
  :config
  (setq treemacs-width 30
        treemacs-position 'right
        )
  ;; TODO(mallchad) this package needs bindings
  )
(req-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  (setq undo-tree-enable-undo-in-region nil  ; brings performance enhancement
        undo-tree-history-directory-alist backup-directory-alist
        )
  ;; TODO(mallchad) need to setup undo-tree persistent history
  ;; Unbind Included Keymaps
  (define-key undo-tree-map (kbd "C-x r u") nil)
  (define-key undo-tree-map (kbd "C-x r U") nil)
  (define-key undo-tree-map (kbd "C-x r") nil)
  )
(req-package visible-mark
  ;; A minor mode to show you where the mark is current
  )
(req-package vlf
  :config
  (require 'vlf-setup)
  )
(req-package visual-fill-column
  )
(req-package volatile-highlights
  :config
  (volatile-highlights-mode)
  )
(req-package ws-butler
  ;; Unobtrusively clean up extrenuous whitespace and convert tabs to whitespace
  :config
  (ws-butler-global-mode)
  (setq ws-butler-convert-leading-tabs-or-spaces t)
  )
(req-package which-key
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 2          ; Don't get in the way of normal usage
        which-key-sort-order 'which-key-description-order
        )
  )
(req-package zoom
  ;; A window rebalancing minor mode
  :config
  (zoom-mode)
  ;; Tweak tweak rebalancing ratios
  (setq zoom-size '(70 . 30))
  )
;;Solve Dependencies and Load in Correct Order
;; Order here doesn't matter
(req-package-finish)
(cemacs-init-setup)
(provide 'init)
;;; init.el ends here
