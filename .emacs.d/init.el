;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;; -*- lexical-binding: t; -*-

;; Hack to speed up package loading time
(setq gc-cons-threshold 100000000       ; 100MB
      ;; package-enable-at-startup nil
      )
;; Extra Load Paths
(add-to-list 'load-path '"~/.emacs.d/etc/")
;; Macros
(defmacro WITH_SYSTEM(type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body)
  )
(defmacro WHEN_WINDOWS (&rest body)
  "Evalute BODY when the OS is Windows."
  (declare (indent defun))
  `(when (eq system-type 'windows-nt)
     ,@body)
  )
(defmacro WHEN_LINUX (&rest body)
  "Evalute BODY when the OS is Linux."
  (declare (indent defun))
  `(when (eq system-type 'gnu/linux)
     ,@body)
  )

;; Enable Package Repositories
(require 'package)
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives (cons "gnu" "https://elpa.gnu.org/packages/"))
;; Melpa
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; Load Built-in
(add-to-list 'load-path (concat user-emacs-directory "src"))
(require 'cemacs-utility)
(require 'natural)

;; Configuration
(defun cemacs-cc-mode()
  "Hook function for cc derived modes."
  (interactive)
  (setq-default c-basic-offset 4
                )
  (setq tab-width 4
        )
  ;; Expand function body when using newline inside parentheses
  ;; Element 't' signifies the global hook shall be run as well
  ;; NOTE this wasn't as useful as I thought it would be and only really works well when the pair is already indented. and feels weird. and manually invoking indent or relying on aggressive-indent seems more useful. anyway.
  ;; (setq-local post-self-insert-hook '(t electric-pair-open-newline-between-pairs-psif))
  )
(add-hook 'c-mode-common-hook 'cemacs-cc-mode)
(defun cemacs-cpp-mode()
  "Hook function for `c++-mode'."
  (interactive)
  (c-add-style  "stroustrup"
                '((c-basic-offset . 4)
                  (c-comment-only-line-offset . 0)
                  (c-offsets-alist

                   ;; Correct Unreal prefix-macros
                   (func-decl-cont)

                   (statement-block-intro . +)
                   (substatement . 0)
                   (substatement-open . 0)
                   (substatement-label . 0)
                   (label . 0)
                   (brace-list-open . 0)
                   (brace-list-intro
                    first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
                   (statement-cont . 0)
                   (inline-open . 0)
                   (case-label . +)
                   (arglist-close . 0)
                   )))
  (c-set-style "stroustrup")
  )
(add-hook 'c++-mode-hook 'cemacs-cpp-mode)
(add-hook 'c-mode-hook 'cemacs-cpp-mode)
(defun cemacs-c-mode()
  "Hook function for `c-mode'."
  (interactive)
  (c-add-style  "stroustrup"
                '((c-basic-offset . 4)
                  (c-comment-only-line-offset . 0)
                  (c-offsets-alist

                   ;; Correct Unreal prefix-macros
                   (func-decl-cont)

                   (statement-block-intro . +)
                   (substatement . 0)
                   (substatement-open . 0)
                   (substatement-label . 0)
                   (label . 0)
                   (brace-list-open . 0)
                   (brace-list-intro
                    first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
                   (statement-cont . 0)
                   (inline-open . 0)
                   (case-label . +)))
                :use-style)
  )
(add-hook 'c-mode-hook 'cemacs-c-mode)
(defun cemacs-elisp-mode()
  "Hook function for `emacs-lisp-mode'."
  (interactive)
  )
(add-hook 'emacs-lisp-mode 'cemacs-elisp-mode)
(defun cemacs-prog-mode()
  "Set up buffer for programming."
  ;; Disable auto-indent, it is inconsistent with incomplete code
  (setq electric-indent-mode nil
        )
  ;; This has severe performance issues, disabling for now
  (display-line-numbers-mode -1)

  ;; Disable multi-byte characters in programming buffers, it is largely used
  ;; for trolling and nothing productive. Programming is typically done in
  ;; English ASCII anyway.
  ;; (toggle-enable-multibyte-characters -1)
  (xref-etags-mode)
  )
(add-hook 'prog-mode-hook 'cemacs-prog-mode)

(defun cemacs-python-mode ()
  "Set up buffer for python-mode"
  )
(add-hook 'python-mode-hook 'cemacs-python-mode)

(defun cemacs-markdown-mode()
  "Hook function for `markdown-mode'."
  (interactive)
  (flyspell-mode)
  )
(add-hook 'markdown-mode-hook 'cemacs-markdown-mode)
(defun cemacs-text-mode ()
  "Hook function for `text-mode'."
  (flyspell-mode)                       ; Spell checking
  (auto-fill-mode -1)                      ; break lines automatically
  )
(defvar cemacs-init-setup-hook nil
  "A normal hook that runs at the end of init setup."
  )
;; Setup Functions
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
configuration see `cemacs-init-local-frame'"
  (interactive)

  ;;Set Fonts
  (WITH_SYSTEM gnu/linux
    (add-to-list 'default-frame-alist
                 '(font . "MesloLGS NF-11:style=Regular")))
  (WITH_SYSTEM windows-nt
    (add-to-list 'default-frame-alist
                 '(font . "Consolas-11:style=Regular")))

  ;;Enable built-in modes
  (global-hl-line-mode)

  ;; Disable Window Decorations
  (if (display-graphic-p)               ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame)))
  (setq-default mode-line-format nil
                vertical-scroll-bar nil
                horizontal-scroll-bar nil
                tool-bar-mode nil
                menu-bar-mode nil
                )
  ;; Tiny fringes, needs to be abled for hl-todo
  (set-fringe-style 5)

  ;; Visualize Whitespace
  (setq whitespace-style '(trailing tabs tab-mark))
  )

(defun cemacs-init-setup()
  "Run after-initilization setup."
  (interactive)
  ;;Misc Setup
  (delete-other-windows)

  ;; Per-Buffer Defaults
  (setq-default
   indent-tabs-mode nil
   tab-width 4
   c-basic-offset 4
   c-electric-flag nil                  ; Disable useless and problematic electric
   parens-require-spaces nil            ; Don't insert space before parenthesis
   fill-column 80                       ; Change where auto-line wrap fill modes trigger
   display-line-numbers-widen t         ; BUGFIX: Assinine fix for lockups on truncated lines
   )

  (setq
   ;; Performance improvements
   inhibit-compacting-font-caches t
   jit-lock-chunk-size 6000             ; Do huge chunks at a time so you can scroll a bit
   jit-lock-defer-time 0                ; Defer fontification when pending input
   jit-lock-stealth-time 3              ; Start fontifying the whole buffer after idle
   ;; I think this is causing hard locks actually causes lockups but I don't
   ;; want to give up fast-refresh. Emacs will just freeze the screen otherwise...
   ;; Okay this aparently only froze because of post-command-hooks like centaur-tabs
   redisplay-dont-pause nil               ; Prioritize drawing responsible over input processing

   ;; Makes fontification and movement more performant at the cost of font-lock accuracy
   fast-but-imprecise-scrolling t
   redisplay-skip-fontification-on-input t
   redisplay-skip-initial-frame nil     ; Maybe improve visuals with pixel draw modes

   ;; UI Improvements
   display-line-numbers-current-absolute t ; Show absolute line number of current line
   display-line-numbers-type 'relative     ; Show relative line numbers for prefix args
   switch-to-buffer-preserve-window-point nil ; Use the buffer's 'point' not the window's 'point'

   ;; Mode Setting
   indent-tabs-mode nil                 ; use spaces for indendation
   transient-mark-mode nil
   global-hl-line-mode t

   ;; Backup
   make-backup-files nil
   backup-by-copying t
   auto-save-default nil

   ;; Execute the 'compile-command' for 'project-compile by default
   ;; Can set a new command using the universal argument
   compilation-read-command nil
   compile-command nil                  ; Don't use 'make -k' by default
   desktop-dirname cemacs-var-dir       ; Set a session save direction
   desktop-path `(,cemacs-var-dir ,user-emacs-directory "~")
   diary-file "~/userdata/org/emacs-diary.gpg"
   text-scale-mode-step 1.1             ; Allow more graduated  text scaling

   ;; Move file locks elsewhere to prevent polluting directories with danglng locks
   create-lockfiles t

   ;; Misc Config
   kill-whole-line nil                   ; don't make 'kill-line' remove empty lines
   create-lockfiles t                    ; make sure lockfiles are enabled in newer versions
   y-or-n-p-use-read-key t               ; allows y-or-n-p to read even if not in the minibuffer
   mouse-scroll-delay 0                  ; makes small mouse scrolls more responsive
   tags-add-tables t                     ; Stop bugging the user about adding tag tables

   ;; GDB Tewaks
   speedbar-use-images nil               ; This is buggy for some reason
   gdb-many-windows nil                  ; prevent modifying the users layout where possible
   gdb-delete-out-of-scope nil           ; Don't auto-delete watch expressions it wastes time
   gdb-display-io-nopopup t              ; Don't auto-raise program output
   gdb-use-colon-colon-notation t        ; Allows watching same-variable names with function::
   gdb-max-children 200                  ; Don't warn about children until huge numbers
   )
  ;; Move file locks elsewhere to prevent polluting directories with danglng locks
  (WHEN_LINUX (setq lock-file-name-transforms
                    '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t))))
  (WHEN_WINDOWS (setq lock-file-name-transforms
                      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/var/\\1" t))))

  ;; Misc Config
  (blink-cursor-mode -1)                ; don't

  ;; Bugs smarparens mode...
  ;; (delete-selection-mode 1)             ; delete activated region on typing
  (global-subword-mode 1)               ; treat delimited words seperately
  (global-whitespace-mode 1)            ; visualize tab characters

  ;; Disabled visual-line because of a large performance penalty on long lines
  ;; (global-visual-line-mode 1)           ; make some commands to operate on visual lines
  (recentf-mode 1)                      ; Save recently visited files

  (defvar cemacs-disabled-features '()
    "List of features intended to be unloaded where possible,
this of it like a blacklist. It must be written in order of
dependency with 0 dependency things listed first or it will
error. It is possible to force-unload each feature but it can
break packages")

  ;; Unload in reverse order
  (cl-loop for x-feature in cemacs-disabled-features do
           ;; Only if feature already loaded
           (when (featurep x-feature)
             (unload-feature x-feature))
           )
  ;; (fset 'electric-indent-post-self-insert-function 'ignore) ;; TODO need to steal this behaviour

  ;; -- Hooks --
  ;; Save recentf on every file open
  (add-hook 'find-file-hook 'recentf-save-list)
  (add-hook 'compilation-finish-functions 'cemacs-bury-compilation)
  (add-hook 'compilation-finish-functions 'cemacs-compilation-hook)

  (fset 'yes-or-no-p 'y-or-n-p )        ; Make all yes or no prompts consistent
  (fset 'overwrite-mode 'ignore)        ; Disable pain in the arse insert mode

  ;; Re-enable disabled functions
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; Run Functions
  (cemacs-configure-session-decorations)

  ;; Create Advice
  (defvar cemacs-after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `cemacs-after-load-theme-hook"
    (run-hooks 'cemacs-after-load-theme-hook))

  (advice-add 'read-only-mode :before 'natural-ad-read-only-mode)

  ;; Defer init setup hooks
  (run-at-time "0.1sec" nil 'run-hooks 'cemacs-init-setup-hook)

  (message "[cemacs Initialized]")
  (setq cemacs-init-complete t)
  )
;; Run early setup to prettify the session
(defun cemacs-early-init ()
  "An early setup function which must run before `cemacs-init-setup'."
  (interactive)
  ;; Variables
  (setq warning-minimum-log-level :warning ; Log warnings in a volatile buffer
        ;; Set early to prevent truncation of the recentf
        recentf-max-saved-items 1000
        )
  (cemacs-defdir 'cemacs-var-dir (concat user-emacs-directory "var/"))
  (cemacs-deffile 'cemacs-custom-file (concat cemacs-var-dir "custom.el")
                  'custom-file :local-only)
  (cemacs-defdir 'cemacs-recentf-save-file (concat cemacs-var-dir "recentf")
                 'recentf-save-file :local-only)

  (defvar cemacs-query-prefix-map (make-sparse-keymap)
    "A prefix-map for various inspection commands, like buffer regex search."
    )
  (define-prefix-command 'cemacs-query-prefix-map)

  ;; Just prettify the frame whilst waiting for loading
  (if (display-graphic-p)  ; Resolve inital frame configuration
      (cemacs-init-local-frame (selected-frame)))
  (load custom-file)
  (require 'personal)
  )
(cemacs-early-init)
;; Req Package Setup
;; Attempt to install req-package if it's not alread loaded
(when (not (require 'req-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'req-package)
  )
(require 'req-package)
(setq use-package-always-ensure t       ; Automatically fetch packages
      use-package-always-defer nil
      use-package-always-demand nil     ; Just there to test loading speed
      )
;; Built in Packages
(req-package asm-mode
  :config
  ;; Stop asm mode from overriding normal insert commands...
  (advice-add #'asm-comment :override #'self-insert-command)
  )
(use-package cc-mode
  :demand t
  :bind
  (("M-;" . cemacs-comment-dwim)
   ("C-x C-;" . cemacs-comment-line)
   )
  :config
  (defun cemacs-comment-dwim (ARG)
    "Allows for C style commenting without the doc-style if using C style comments at the time"
    (interactive "P")
    (if (bound-and-true-p c-block-comment-flag)
        (let ((comment-style 'multi-line)
              (comment-continue "   "))
          (comment-dwim ARG))
      ;; else
      (comment-dwim ARG))
    )
  (defun cemacs-comment-line (ARG)
    "Allows for C style commenting without the doc-style if using C style comments at the time"
    (interactive "P")
    (if (bound-and-true-p c-block-comment-flag)
        (let ((comment-style 'multi-line)
              (comment-continue "   "))
          (comment-line ARG))
      ;; else
      (comment-line ARG))
    ))
(req-package flyspell
  :after org
  :bind
  ;; Unbind Default keys
  (:map flyspell-mode-map
        ([(control ?\,)] .  nil)
        ([(control ?\.)] .  nil)
        ;; `org-return' is infuriating, so disable it until something better is found
        :map org-mode-map
        ("RET" .            nil))
  :config
  )

(req-package compile
  :demand t
  :config

  ;; TODO Improve logic need to improve display logic because currently its fairly random
  (defun compilation-start (command &optional mode name-function highlight-regexp)
  "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
  (or mode (setq mode 'compilation-mode))
  (let* ((name-of-mode
	  (if (eq mode t)
	      "compilation"
	    (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
	 (thisdir default-directory)
	 (thisenv compilation-environment)
	 outwin outbuf)
    (with-current-buffer
	(setq outbuf
	      (get-buffer-create
               (compilation-buffer-name name-of-mode mode name-function)))
      (let ((comp-proc (get-buffer-process (current-buffer))))
      (if comp-proc
          (if (or (not (eq (process-status comp-proc) 'run))
                  (eq (process-query-on-exit-flag comp-proc) nil)
                  (yes-or-no-p
                   (format "A %s process is running; kill it? "
                           name-of-mode)))
              (condition-case ()
                  (progn
                    (interrupt-process comp-proc)
                    (sit-for 1)
                    (delete-process comp-proc))
                (error nil))
            (error "Cannot have two processes in `%s' at once"
                   (buffer-name)))))
      ;; first transfer directory from where M-x compile was called
      (setq default-directory thisdir)
      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (let ((inhibit-read-only t)
	    (default-directory thisdir))
	;; Then evaluate a cd command if any, but don't perform it yet, else
	;; start-command would do it again through the shell: (cd "..") AND
	;; sh -c "cd ..; make"
	(cd (cond
             ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]"
                                 command))
              default-directory)
             ((not (match-end 1)) "~")
             ((eq (aref command (match-beginning 1)) ?\')
              (substring command (1+ (match-beginning 1))
                         (1- (match-end 1))))
             ((eq (aref command (match-beginning 1)) ?\")
              (replace-regexp-in-string
               "\\\\\\(.\\)" "\\1"
               (substring command (1+ (match-beginning 1))
                          (1- (match-end 1)))))
             ;; Try globbing as well (bug#15417).
             (t (let* ((substituted-dir
                        (substitute-env-vars (match-string 1 command)))
                       ;; FIXME: This also tries to expand `*' that were
                       ;; introduced by the envvar expansion!
                       (expanded-dir
                        (file-expand-wildcards substituted-dir)))
                  (if (= (length expanded-dir) 1)
                      (car expanded-dir)
                    substituted-dir)))))
	(erase-buffer)
	;; Select the desired mode.
	(if (not (eq mode t))
            (progn
              (buffer-disable-undo)
              (funcall mode))
	  (setq buffer-read-only nil)
	  (with-no-warnings (comint-mode))
	  (compilation-shell-minor-mode))
        ;; Remember the original dir, so we can use it when we recompile.
        ;; default-directory' can't be used reliably for that because it may be
        ;; affected by the special handling of "cd ...;".
        ;; NB: must be done after (funcall mode) as that resets local variables
        (setq-local compilation-directory thisdir)
        (setq-local compilation-environment thisenv)
	(if highlight-regexp
            (setq-local compilation-highlight-regexp highlight-regexp))
        (if (or compilation-auto-jump-to-first-error
		(eq compilation-scroll-output 'first-error))
            (setq-local compilation-auto-jump-to-next t))
	;; Output a mode setter, for saving and later reloading this buffer.
	(insert "-*- mode: " name-of-mode
		"; default-directory: "
                (prin1-to-string (abbreviate-file-name default-directory))
		" -*-\n"
		(format "%s started at %s\n\n"
			mode-name
			(substring (current-time-string) 0 19))
		command "\n")
        ;; Mark the end of the header so that we don't interpret
        ;; anything in it as an error.
        (put-text-property (1- (point)) (point) 'compilation-header-end t)
	(setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    ;; https://lists.gnu.org/r/emacs-devel/2007-11/msg01638.html
    (setq outwin (display-buffer outbuf '(nil (inhibit-same-window . t) (allow-no-window . t))))
    (with-current-buffer outbuf
      (let ((process-environment
	     (append
	      compilation-environment
              (and (derived-mode-p 'comint-mode)
                   (comint-term-environment))
	      (list (format "INSIDE_EMACS=%s,compile" emacs-version))
	      (copy-sequence process-environment))))
        (setq-local compilation-arguments
                    (list command mode name-function highlight-regexp))
        (setq-local revert-buffer-function 'compilation-revert-buffer)
	(and outwin
	     ;; Forcing the window-start overrides the usual redisplay
	     ;; feature of bringing point into view, so setting the
	     ;; window-start to top of the buffer risks losing the
	     ;; effect of moving point to EOB below, per
	     ;; compilation-scroll-output, if the command is long
	     ;; enough to push point outside of the window.  This
	     ;; could happen, e.g., in `rgrep'.
	     (not compilation-scroll-output)
	     (set-window-start outwin (point-min)))

	;; Position point as the user will see it.
	(let ((desired-visible-point
	       ;; Put it at the end if `compilation-scroll-output' is set.
	       (if compilation-scroll-output
		   (point-max)
		 ;; Normally put it at the top.
		 (point-min))))
	  (goto-char desired-visible-point)
	  (when (and outwin (not (eq outwin (selected-window))))
	    (set-window-point outwin desired-visible-point)))

	;; The setup function is called before compilation-set-window-height
	;; so it can set the compilation-window-height buffer locally.
	(if compilation-process-setup-function
	    (funcall compilation-process-setup-function))
	(and outwin (compilation-set-window-height outwin))
	;; Start the compilation.
	(if (fboundp 'make-process)
	    (let ((proc
		   (if (eq mode t)
                       ;; On remote hosts, the local `shell-file-name'
                       ;; might be useless.
                       (with-connection-local-variables
		        ;; comint uses `start-file-process'.
		        (get-buffer-process
			 (with-no-warnings
			   (comint-exec
			    outbuf (downcase mode-name)
			    shell-file-name
			    nil `(,shell-command-switch ,command)))))
		     (start-file-process-shell-command (downcase mode-name)
						       outbuf command))))
              ;; Make the buffer's mode line show process state.
              (setq mode-line-process
                    '((:propertize ":%s" face compilation-mode-line-run)
                      compilation-mode-line-errors))

              ;; Set the process as killable without query by default.
              ;; This allows us to start a new compilation without
              ;; getting prompted.
              (when compilation-always-kill
                (set-process-query-on-exit-flag proc nil))

              (set-process-sentinel proc #'compilation-sentinel)
              (unless (eq mode t)
                ;; Keep the comint filter, since it's needed for proper
		;; handling of the prompts.
		(set-process-filter proc #'compilation-filter))
	      ;; Use (point-max) here so that output comes in
	      ;; after the initial text,
	      ;; regardless of where the user sees point.
	      (set-marker (process-mark proc) (point-max) outbuf)
	      (when compilation-disable-input
		(condition-case nil
		    (process-send-eof proc)
		  ;; The process may have exited already.
		  (error nil)))
	      (run-hook-with-args 'compilation-start-hook proc)
              (compilation--update-in-progress-mode-line)
	      (push proc compilation-in-progress))
	  ;; No asynchronous processes available.
	  (message "Executing `%s'..." command)
	  ;; Fake mode line display as if `start-process' were run.
	  (setq mode-line-process
		'((:propertize ":run" face compilation-mode-line-run)
                  compilation-mode-line-errors))
	  (force-mode-line-update)
	  (sit-for 0)			; Force redisplay
	  (save-excursion
	    ;; Insert the output at the end, after the initial text,
	    ;; regardless of where the user sees point.
	    (goto-char (point-max))
	    (let* ((inhibit-read-only t) ; call-process needs to modify outbuf
		   (compilation-filter-start (point))
		   (status (call-process shell-file-name nil outbuf nil "-c"
					 command)))
	      (run-hooks 'compilation-filter-hook)
	      (cond ((numberp status)
		     (compilation-handle-exit
		      'exit status
		      (if (zerop status)
			  "finished\n"
			(format "exited abnormally with code %d\n" status))))
		    ((stringp status)
		     (compilation-handle-exit 'signal status
					      (concat status "\n")))
		    (t
		     (compilation-handle-exit 'bizarre status status)))))
	  (set-buffer-modified-p nil)
	  (message "Executing `%s'...done" command)))
      ;; Now finally cd to where the shell started make/grep/...
      (setq default-directory thisdir)
      ;; The following form selected outwin ever since revision 1.183,
      ;; so possibly messing up point in some other window (bug#1073).
      ;; Moved into the scope of with-current-buffer, though still with
      ;; complete disregard for the case when compilation-scroll-output
      ;; equals 'first-error (martin 2008-10-04).
      (when compilation-scroll-output
	(goto-char (point-max))))

    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)))
  )

;; Some nice clever behaviour and helpers for programming pair management
(req-package elec-pair
  :demand t
  )

(req-package gdb-mi
  :config
  ;; Fix for gdb-watch
  ;;   (defun gud-watch (&optional arg event)
  ;;     "Watch expression at point.
  ;; With arg, enter name of variable to be watched in the minibuffer."
  ;;     (interactive (list current-prefix-arg last-input-event))
  ;;     (let ((minor-mode (buffer-local-value 'gud-minor-mode gud-comint-buffer)))
  ;;       (if (eq minor-mode 'gdbmi)
  ;;           (progn
  ;;             (if event (posn-set-point (event-end event)))
  ;;             (require 'tooltip)
  ;;             (save-selected-window
  ;;               (let ((expr
  ;;                      (if arg
  ;;                          (read-string "Watch Expression: ")
  ;;                        (if (and transient-mark-mode mark-active)
  ;;                            (buffer-substring (region-beginning) (region-end))
  ;;                          (concat (if (derived-mode-p 'gdb-registers-mode) "$")
  ;;                                  (tooltip-identifier-from-point (point)))))))
  ;;                 (set 'gud-watch-expression expr)
  ;;                 (set-text-properties 0 (length expr) nil expr)
  ;;                 (gdb-input (concat "-var-create - * "  expr "")
  ;;                            (lambda () (gdb-var-create-handler gud-watch-expression))))))
  ;;         (message "gud-watch is a no-op in this mode."))))
  )

(req-package org
  :require
  ;; A cool new package which inserts links
  org-cliplink
  :hook
  (org-mode . cemacs-text-mode)
  :bind
  (("C-M-#" . cemacs-org-stamp-time)
   :map org-mode-map
   ("C-c C-x C-s" . cemacs-org-archive-and-done))
  :config
  (defvar cemacs-org-priority-list
    '(("* *" "top")
      ("ARCHIVE" "bottom")
      ))

  (setq
   org-cycle-separator-lines 0 ;; Prevent newlines from seperating headings
   org-table-tab-jumps-over-hlines nil ;; Make it easier to add newlines above footer rows
   org-image-actual-width nil          ; Allow using attributes to change inline image width
   )
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
               "~/userdata/notes")
             do (cemacs-open-files-in-directory x-folder)
             ))
  (defun cemacs-org-tagwise-comp-func (taglist-left taglist-right)
    "Decide which tag should go above, biasing ARCHIVE tags to the bottom."
    (interactive)
    (let ((comp-left (list ""))
          (comp-right (list ""))
          (less-than 'no-priority)
          (comp-left-tag-string "")
          (comp-right-tag-string "")
          (left-tag "")
          (right-tag ""))
      ;; Normalize tag strings
      (while (or (car taglist-left) (car taglist-right))
        (setq left-tag (or (pop taglist-left) ""))
        (setq right-tag (or (pop taglist-right) ""))
        (when (stringp left-tag)
          (push (downcase left-tag) comp-left))
        (when (stringp right-tag)
          (push (downcase right-tag) comp-right)))
      ;; This currently only pushes the ARCHIVE tag to the bottom
      ;; Whilst this does the job for general todo lists it is far from ideal
      ;; Here `less-than` actually counter-intuitively means the beginning of the
      ;; buffer, despite it being physically at the top, this is purely down to
      ;; character storing mechanics where in ASCII
      ;; `a == 0 + 97` and  `a == 0 + 122`
      ;; Which leads values closer to a being considered "lower" and being sorted
      ;; closer to the numerically lower buffer position 0, the very top left
      (cond ((member (downcase org-archive-tag) comp-left)
             (setq less-than nil)
             )
            ((member (downcase org-archive-tag) comp-right)
             (setq less-than t)
             )
            ;; Blindly comapre tags alphanumerically
            (:default
             (while (car comp-left)
               (setq comp-left-tag-string
                     (concat comp-left-tag-string (pop comp-left))))
             (while (car comp-right)
               (setq comp-right-tag-string
                     (concat comp-right-tag-string (pop comp-right))))
             (if (eq less-than 'no-priority)
                 (setq less-than (string-collate-lessp comp-left-tag-string
                                                       comp-right-tag-string))))
            )
      less-than)
    )
  (defun cemacs-org-sort-taglist-get ()
    "Get a list of tags for the heading under point."
    (interactive)
    (or (org-get-tags) (list ""))
    )
  (defun cemacs-org-sort-by-tag (&optional reverse)
    "Sort top level org headings by tag alphanumerically, grouping archive tags.

    With REVERSE non-nil or with a prefix argument, reverse the sorting order.
    This function makes a point of forcing the `org-archive-tag' towards the extreme
    top or bottom of the file."
    (interactive "P")
    ;; Can't function by original point, get line and match it again
    (let ((position-along-original-line (- (point) (line-beginning-position)))
          (original-line-contents
           (buffer-substring (line-beginning-position) (line-end-position))))
      (mark-whole-buffer)
      (org-global-cycle 1)                ; Hide all subtrees
      (if reverse
          (org-sort-entries nil ?f 'cemacs-org-sort-taglist-get
                            '(lambda (taglist-left taglist-right)
                               (not (cemacs-org-tagwise-comp-func taglist-left taglist-right))))
        ;; Else
        (org-sort-entries nil ?f 'cemacs-org-sort-taglist-get 'cemacs-org-tagwise-comp-func))
      ;; Try return to the line the command was invoked on
      (search-forward original-line-contents)
      (goto-char (+ position-along-original-line (line-beginning-position))))
    )
  (defun cemacs-org-stamp-date ()
    (interactive)
    (call-interactively
     (org-time-stamp cemacs-universal-argument-double 'inactive))
    )
  (defun cemacs-org-stamp-time ()
    (interactive)
    (org-time-stamp cemacs-universal-argument-double 'inactive)
    )
  (defun cemacs-org-archive-and-done ()
    "Cycle the 'TODO' state and toggle the archive tag in one function."
    (interactive)
    (org-todo)
    (org-toggle-archive-tag)
    )
  (defun cemacs-org-folded-p ()
    "Returns non-nil if point is on a folded headline or plain list item."
    (interactive)
    (and (or (org-at-heading-p)
             (org-at-item-p))
         (invisible-p (point-at-eol)))
    )
  (defun cemacs-org-mode ()
    "Hook function for `org-mode'"
    (interactive)
    (org-indent-mode)
    (setq-local org-hide-leading-stars nil)
    )
  (add-hook 'org-mode-hook 'cemacs-org-mode)

  ;; Colour org checkboxes based on state
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

  ;; Only cycle node under cursor
  (add-hook 'org-cycle-hook
            (lambda (state)
              (when (eq state 'children)
                (setq org-cycle-subtree-status 'subtree))))
  )
(use-package tab-bar
  :demand t
  :hook
  (cemacs-init-setup . tab-bar-mode)
  :config
  (setq
   tab-bar-close-button-show nil
   tab-bar-button-margin 0
   )

  (defun cemacs-tab-bar-define-faces ()
    (set-face-attribute 'tab-bar-tab nil :box 1)
    (set-face-attribute 'tab-bar-tab-inactive nil :box nil)
    )
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-tab-bar-define-faces)
  )

;; External Packages
(req-package ansi-color
  :demand t
  ;; :disabled
  :config
  (WHEN_LINUX
    (defun cemacs-colorize-compilation-buffer ()
      (ansi-color-apply-on-region compilation-filter-start (point))
      ))
  (add-hook 'compilation-filter-hook 'cemacs-colorize-compilation-buffer)
  )
(req-package async
  :demand t
  :config
  (async-bytecomp-package-mode t)
  )
(req-package color-theme-sanityinc-tomorrow
  :hook
  (cemacs-init-setup . cemacs-sanityinc-tomorrow-init)
  :init
  (defadvice color-theme-sanityinc-tomorrow (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'cemacs-after-load-theme-hook))

  :config
  (defcustom cemacs-selected-sanityinc-theme 'sanityinc-tomorrow-bright
    "A localized variable setting the preferred sanityinc theme")
  (defun cemacs-sanityinc-tomorrow-init ()
    (if (and (boundp 'cemacs-selected-sanityinc-theme)
             (symbol-value 'cemacs-selected-sanityinc-theme))
        (load-theme cemacs-selected-sanityinc-theme)
      (load-theme 'sanityinc-tomorrow-bright t))
    )

  (defun cemacs-tomorrow-define-faces ()
    (set-face-attribute 'default nil :background "gray5")
    )
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-tomorrow-define-faces)
  )
(req-package aggressive-indent
  ;; :hook
  ;; (prog-mode . aggressive-indent-mode)
  :config
  (defvar c-aggressive-indent-line-end-prevention-distance 3
    "The distance in characters away from the end of the line
  you should be before aggressively auto-indenting")
  (defun c-aggressive-indent-near-line-end ()
    (if (< (- (line-end-position) (point))
           c-aggressive-indent-line-end-prevention-distance)
        :dont-indent
      nil)
    )
  ;; Prevent aggressive indentation after some commands
  ;; This essentially makes the behaviour less "aggressive" and offers
  ;; the user some freedom to move the cursor, before reverting to normal
  ;; behaviour
  (cemacs-add-multiple-to-list
   'aggressive-indent-protected-commands
   'backward-delete-char
   'cemacs-delete-word
   'backward-kill-word
   'natural-delete-word
   'natural-delete-word-backwards
   'natural-delete-whitespace
   'natural-one-space
   'delete-char
   'backward-delete-char
   'backward-delete-char
   'tab-to-tab-stop
   'just-one-space
   'delete-horizontal-space
   'natural-tab-to-tab-stop)
  (cemacs-add-multiple-to-list 'aggressive-indent-dont-indent-if
                               `(c-aggressive-indent-near-line-end))
  ;; Reduce the chance of impacting performance by increasing idle time
  (setq aggressive-indent-sit-for-time 0.2)
  )
(req-package all-the-icons
  :config
  (when (not (boundp 'cemacs-all-the-icons-fonts-installed))
    (all-the-icons-install-fonts 'skip)
    (customize-save-variable 'cemacs-all-the-icons-fonts-installed t))
  )
(req-package avy
  :require avy-zap
  :commands
  (avy-goto-char
   avy-pop-mark
   avy-zap-to-char-dwim)
  :bind
  (("C-r" . avy-goto-char)
   ("M-z" . avy-zap-to-char-dwim))

  :init
  ;; The colours picked here are designed to maximize readabiltiy
  ;;
  ;; Glaring/agressive and eye-catching are avoided to prevent distraction.
  ;; This makes dark and desaturated colours at the best fit for this, with
  ;; the characters being bright and high contrast, since the character is the
  ;; part meant to be read.
  ;; The colours picked here are designed to maximize readabiltiy
  ;;
  ;; Glaring/agressive and eye-catching are avoided to prevent distraction.
  ;; This makes dark and desaturated colours at the best fit for this, with
  ;; the characters being bright and high contrast, since the character is the
  ;; part meant to be read.
  (defun cemacs-avy-define-faces ()
    (set-face-attribute 'avy-lead-face nil :background '"dark red" :foreground "white")
    (set-face-attribute 'avy-lead-face-0 nil :background "#1d1d62" :foreground "white")
    (set-face-attribute 'avy-lead-face-2 nil :background "#2a3418" :foreground "white")
    )
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-avy-define-faces)

  :config
  (setq avy-highlight-first t
        avy-background t
        ;; Use 2 key combos instead of only single home row
        avy-style 'words
        avy-words nil
        )
  ;; Use all keys for avy instead of just home row
  (cl-loop for char-left from ?a to ?z do
           (cl-loop for char-right from ?a to ?z do
                    (add-to-list 'avy-words
                                 (concat (string char-left) (string char-right))
                                 :append)))
  )
(req-package backup-each-save
  :commands
  (backup-each-save)

  :hook
  (after-save . backup-each-save)
  (cemacs-kill-volatile-buffer-pre . backup-each-save)
  (cemacs-init-setup . c-backup-recentf)
  :config
  ;; TODO Stop being lazy and turn this is into a custom function
  ;; Deal with problematic windows drive path rules
  (WHEN_WINDOWS
    (defun backup-each-save-compute-location (filename)
      (let* ((containing-dir
              (replace-regexp-in-string ".:/"
                                        "drive/"         ; Strip colon from drive path
                                        (file-name-directory filename)))
             (basename (file-name-nondirectory filename))
             (backup-container
              (format "%s/%s"
                      backup-each-save-mirror-location
                      containing-dir)))
        (when (not (file-exists-p backup-container))
          (make-directory backup-container t))
        (format "%s/%s-%s" backup-container basename
                (format-time-string backup-each-save-time-format)))
      ))

  (defun backup-each-save ()
    (interactive)
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
        (let ((bfn (buffer-file-name)))
          (when (and (or backup-each-save-remote-files
                         (not (file-remote-p bfn)))
                     (funcall backup-each-save-filter-function bfn)
                     (or (not backup-each-save-size-limit)
                         (<= (buffer-size) backup-each-save-size-limit)))
            (copy-file bfn (backup-each-save-compute-location bfn) t t t))))
    )

  (defun c-backup-recentf ()
    (let* ((recentf-path (file-truename recentf-save-file))
           (recentf-backup-path (backup-each-save-compute-location recentf-path)))
      (file-truename recentf-backup-path)
      (copy-file recentf-path recentf-backup-path
                 :overwrite :perserve-time :preserve-id :preserve-permission))
    )
  )

(req-package beacon
  :hook
  (cemacs-init-setup . beacon-mode)
  :config
  (setq cemacs-beacon-size 40
        cemacs-beacon-size-padding 10
        beacon-size cemacs-beacon-size
        beacon-color "gold"
        beacon-blink-when-point-moves-vertically 1    ;; blink if the line changes
        beacon-blink-when-point-moves-horizontally 20
        ;; Don't push the mark when the cursor moves a long distance
        beacon-push-mark nil
        beacon-visual-size 40
        beacon-size-padding 10
        beacon-visual-mode t

        )
  (defun beacon--int-range (a b)
    "Return a list of integers between A inclusive and B exclusive.
Only returns `beacon-size' elements, optionally truncated to the visual line with `beacon-visual-mode'."
    (setq beacon-visual-size beacon-size)
    (let* ((d (/ (- b a) beacon-visual-size))
           (out (list a))
           (point-along-line (- (point) (save-excursion (beginning-of-visual-line) (point))))
           (buffer-width (window-width))
           (point-window-end-distance (- buffer-width point-along-line)))
      (setq beacon-visual-size (- (abs point-window-end-distance)
                                  beacon-size-padding))
      (when (and (> beacon-visual-size beacon-size) (bound-and-true-p beacon-visual-mode))
        (setq beacon-visual-size beacon-size))
      (when (< beacon-visual-size 0)
        (setq beacon-visual-size 0)
        (message "Failed to calculate a non-intrusive beacon-size, Refusing to blink"))
      (dotimes (_ (1- beacon-visual-size))
        (push (+ (car out) d) out))
      (nreverse out)))

  (defun beacon--shine ()
    "Shine a beacon at point.
If `beacon-visual-mode' is `t' then the beacon will refuse to run over to the next line."
    (let ((colors (beacon--color-range))
          (loop-limit 1000))
      (if (> beacon-visual-size 0)
          (save-excursion
            (while (and colors (> loop-limit 0))
              (set 'loop-limit (1- loop-limit))
              (if (looking-at "$")
                  (progn
                    (beacon--after-string-overlay colors)
                    (setq colors nil))
                (beacon--colored-overlay (pop colors))
                (forward-char 1)))))))

  (defun beacon-blink ()
    "Blink the beacon at the position of the cursor.
Unlike `beacon-blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from lisp code."
    (interactive)
    (beacon--vanish)
    (run-hooks 'beacon-before-blink-hook)
    (beacon--shine)
    (when (timerp beacon--timer)
      (cancel-timer beacon--timer))
    (setq beacon--timer nil)
    (if (> beacon-visual-size 0)
        (setq beacon--timer
              (run-at-time beacon-blink-delay
                           (/ beacon-blink-duration 1.0 beacon-visual-size)
                           #'beacon--dec))))


  (defun beacon-blink-automated ()
    "If appropriate, blink the beacon at the position of the cursor.
Unlike `beacon-blink', the blinking is conditioned on a series of
variables: `beacon-mode', `beacon-dont-blink-commands',
`beacon-dont-blink-major-modes', and
`beacon-dont-blink-predicates'."
    ;; Record vars here in case something is blinking outside the
    ;; command loop.
    (beacon--record-vars)
    (unless (or (not beacon-mode)
                (run-hook-with-args-until-success 'beacon-dont-blink-predicates)
                (seq-find #'derived-mode-p beacon-dont-blink-major-modes)
                (memq (or this-command last-command) beacon-dont-blink-commands))
      (beacon-blink)))
  )
(req-package bind-key
  :config
  (defun cemacs-bind-vanilla-keys()
    "Set up personal keybinds after initilization."
    (interactive)
    ;; Emacs Control Bindings
    ;; Here we try to keep as many standard editor bindings as possible, to
    ;; make it less jarring if a non-emacs user needs to use it for any reason.
    ;;
    ;; Equally, we try to keep as many vanilla emacs bindings as possible,
    ;; to make unmodified distributions more accessible.
    (bind-keys
     ;; Navigation
     ("C-a" .           natural-beginning-of-line)
     ("C-e" .           natural-end-of-line)
     ("<home>" .        natural-beginning-of-line)
     ("<end>" .         natural-end-of-line)
     ("M-b" .           natural-backward-word)
     ("M-f" .           natural-forward-word)
     ("<C-left>" .      natural-backward-word)
     ("<C-right>" .     natural-forward-word)
     ("C-," .           pop-to-mark-command)
     ("M-[" .           tab-bar-switch-to-prev-tab)
     ("M-]" .           tab-bar-switch-to-next-tab)

     ;; Editing Commands
     ("<C-backspace>" . natural-delete-word-backwards)
     ("M-d" .           natural-delete-word)
     ("<C-delete>" .    natural-delete-word)
     ("M-\\" .          natural-delete-whitespace)
     ("M-SPC" .         natural-one-space)
     ("M-i" .           natural-tab-to-tab-stop)
     ("M-p" .           kill-whole-line)
     ("C-#" .           cemacs-activate-mark)

     ;; Other
     ("C-q" .           cemacs-query-prefix-map)
     ("C-x k" .         cemacs-buffer-kill-volatile)
     ("M-o" .           ff-find-other-file)
     ("C-x e" .         cemacs-find-user-init-file)
     ("C-x C-#" .       server-edit-abort)
     ("<mouse-3>" .     menu-bar-open) ; Make right click context menu
     ("C-x x g" .       cemacs-revert-buffer)

     ;; Unbind Keys
     ("<insert>" .      nil)         ; 'overwrite-mode
     ("<backspace>" .   nil)
     ;; Disable highlighting region with mouse dragging, if it becomes unstable
     ;; ("<down-mouse-1>" . nil)
     )
    )
  (ignore-errors (cemacs-bind-vanilla-keys)) ; Protected from req-package error
  )

;;; Minor-mode which hides the compilaton buffer if successful
(req-package bury-successful-compilation
  ;; Enable bury-successful-compilation mode
  :hook (cemacs-init-setup . bury-successful-compilation)
  :config
  (defun bury-successful-compilation-buffer (buffer string)
    "Bury the compilation BUFFER after a successful compile.
Argument STRING provided by compilation hooks."
    (setq bury-successful-compilation-save-windows
          (and
           (equal 'compilation-mode major-mode)
           (string-match "finished" string)
           (not (save-excursion (goto-line 5) (search-forward "warning" nil t))))
          )
    (when bury-successful-compilation-save-windows
      (ignore-errors
        (jump-to-register
         bury-successful-compilation-precompile-window-state))
      (message "Compilation successful.")))
  (defadvice compilation-start (before
                                bury-successful-compilation-save-windows
                                activate)
    "Save window configuration to
`bury-successful-compilation-precompile-window-state' unless
`bury-successful-compilation-save-windows' is nil."
    (let ((save-configuration nil)
          (visible-compilation nil)
          )
      (setq visible-compilation (dolist (x-buffer (buffer-list))
                                        (when (eq 'compilation-mode major-mode)
                                          (setq visible-compilation t)))
            save-configuration (or (not visible-compilation)
                                   bury-successful-compilation-save-windows))
      (when save-configuration
        (window-configuration-to-register
         bury-successful-compilation-precompile-window-state))
      ))
  )

;;; A robust, prettified calender framework
(req-package calfw
  :require calfw-org
  :commands
  (cfw:open-calendar-buffer)
  )

(req-package centaur-tabs
  :hook
  (cemacs-init-setup . centaur-tabs-mode)
  :bind
  (("<C-tab>" . centaur-tabs-forward)
   ("<C-S-tab>" . centaur-tabs-backward)
   ("<C-iso-lefttab>" . centaur-tabs-backward)
   ("<C-S-iso-lefttab>" . centaur-tabs-backward))
  :config
  ;;Misc Settings
  (setq centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-close-button nil
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-style "bar"
        centaur-tabs-set-bar t
        centaur-tabs-bar 'over
        centaur-tabs-modified-marker "*"
        )
  ;;Create Uniform Tabbar Appearance
  (centaur-tabs-headline-match)
  ;;Group tabs by projects
  (centaur-tabs-group-by-projectile-project)
  (defun cemacs-centaur-tabs-define-faces ()
    (set-face-attribute 'centaur-tabs-modified-marker-unselected nil :foreground "white")
    (set-face-attribute 'centaur-tabs-modified-marker-selected nil :foreground "red")
    )
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-centaur-tabs-define-faces)

  ;; Fix for the insanely slow nonsense centaur-tabs does.
  ;; No seriously, wtf is this, a very slow hook for *every command in emacs*???
  (remove-hook 'post-command-hook centaur-tabs-adjust-buffer-order-function)
  (add-hook 'window-buffer-change-functions centaur-tabs-adjust-buffer-order-function)
  (defun cemacs-ad-centaur-reorder (oldfun &rest args)
    (while-no-input (funcall oldfun))
    )
  (advice-add 'centaur-tabs-adjust-buffer-order-alphabetically :around 'cemacs-ad-centaur-reorder)
  (advice-add 'centaur-tabs-adjust-buffer-order :around 'cemacs-ad-centaur-reorder)
)

(req-package company
  :commands
  (company-mode
   global-company-mode
   cemacs-company-define-faces)
  :hook
  (cemacs-after-load-theme . cemacs-company-define-faces)
  (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("M-p" . company-select-previous)
        ("M-n" . company-select-next)
        ;; Make company less disruptive to general editing
        ("C-p" . nil)
        ("C-n" . nil)
        :map company-tng-map
        ("C-p" . nil)
        ("C-n" . nil)

        ;; Allow scrolling completion with PgUp/PgDn keys (not C-v/M-v)
        ("<next>" . company-next-page)
        ("<prev>" . company-next-page))

  :init
  (defun cemacs-company-define-faces ()
    (set-face-attribute 'company-tooltip-scrollbar-thumb nil :background "Skyblue4")
    (set-face-attribute 'company-tooltip-scrollbar-track nil :background "#2c2f33")
    (set-face-attribute 'company-tooltip nil :foreground "#cccccc"  :background "#302a1b")
    (set-face-attribute 'company-tooltip-common nil :foreground "#d93f3f")
    )

  :config
  ;; Apply company-tng patch
  (company-tng-configure-default)
  (setq company-require-match 'never

        ;; Slow down popup so input blocking is reduced
        ;; This is tuned so it shows completion hints if and only if the user hesitates
        company-idle-delay 0.2

        ;; Backend is fast, slow redisplay to avoid double rendering everything
        ;; and improve responsiveness
        company-async-redisplay-delay 2

        ;; vscode icons are meaningless don't bother with them
        company-format-margin-function 'company-text-icons-margin
        )
  (delete 'company-clang company-backends)
  )

(req-package crux
  :commands
  (crux-move-beginning-of-line
   crux-top-join-line
   crux-swap-windows
   crux-smart-open-line-above)
  :bind
  (
   ;; ("C-j" . crux-top-join-line)
   ("C-c C-o" . crux-swap-windows)
   ;; ("C-o" . crux-smart-open-line-above)
   )
  :config
  ;; Fix crux not honouring visual lines
  (setq crux-move-visually t)
  )
(req-package cmake-mode
  :defer 5
  :commands (cmake-mode)
  )
(req-package csharp-mode
  :defer 5
  :commands (csharp-mode)
  :require csproj-mode
  :hook
  (csharp-mode . cemacs-cc-mode)
  :config
  (defun cemacs-csharp-mode()
    (interactive)
    (setq tab-width 4)
    (setq-default c-basic-offset 4)
    (c-set-style "csharp")
    (c-add-style  "stroustrup"
                '((c-offsets-alist (substatement-open . 0))) :use-style)
    )
  (add-hook 'csharp-mode-hook 'cemacs-csharp-mode)
  )

(req-package cuda-mode
  )

(req-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )

(req-package desktop
  :ensure nil
  :config
  (defun cemacs-desktop-owner-advice (original &rest args)
    (let ((owner (apply original args)))
      (if (and owner (/= owner (emacs-pid)))
          (and (car (member owner (list-system-processes)))
               (let (cmd (attrlist (process-attributes owner)))
                 (if (not attrlist) owner
                   (dolist (attr attrlist)
                     (and (string= "comm" (car attr))
                          (setq cmd (car attr))))
                   (and cmd (string-match-p "[Ee]macs" cmd) owner))))
        owner)))
  ;; Shotgun advice fix for preventing killing emacs likely break desktop-mode
  (remove-hook 'kill-emacs-query-functions 'desktop-kill)
  ;; Ensure that dead system processes don't own it.
  (advice-add #'desktop-owner :around #'cemacs-desktop-owner-advice)
  )

;; desktop-plus
;; Name based desktop saving
(req-package desktop+
  :commands
  (desktop+-create
   desktop+load)

  :config
  (cemacs-defdir 'c-desktop-plus-base-dir (concat cemacs-var-dir "desktop-plus")
                 'desktop+-base-dir :local-only)
  )

(req-package diff-hl
  :hook
  (cemacs-init-setup . global-diff-hl-mode)
  :commands
  (global-diff-hl-mode)
  )

;; Needs to be downloaded seperaetly
;; (req-package explain-pause-mode
;;   :demand t
;;   :config
;;   (explain-pause-mode)
;;   )

;; Out of date package signature :(
;; (req-package emacs-gc-stats
;;   :demand t
;;   )

(req-package embark
  )

;; Keybind usage statistics
;; Helps identify cancidates keybindings for development
(req-package keyfreq
  :hook
  (cemacs-init-setup . keyfreq-mode)
  (cemacs-init-setup . keyfreq-autosave-mode)
  :config
  (cemacs-deffile 'cemacs-keyfreq-file (concat cemacs-var-dir "keyfreq")
                  'keyfreq-file :local-only)

  (defun keyfreq-table-load (table)
    "Load all values from the `keyfreq-file' and add them in the TABLE.
The table is not reset, so the values are appended to the table.

MODIFIED (cemacs)
This version has been patched to avoid clobbering the keyfreq file when the lisp
 form is invalid or corrupted for some reason"

    ;; Does `keyfreq-file' exist?
    (if (file-exists-p keyfreq-file)
        ;; Load sexp
        (let
            ((l (with-temp-buffer
                  (insert-file-contents keyfreq-file)
                  (goto-char (point-min))
                  (condition-case error (read (current-buffer))
                    ;; If reading fails just backup the file and bail
                    (t (rename-file
                        keyfreq-file
                        (concat keyfreq-file "_backup_"
                                (format-time-string  "%Y-%m-%d_%H%M_%S" (current-time))
                                ))))))
             )

          ;; Add the values in the table
          (while (and (listp l) l)
            (if (listp (car l))
                (unless (keyfreq-match-p (cdr (caar l)))
                  (puthash (caar l) (+ (gethash (caar l) table 0) (cdar l)) table)))
            (setq l (cdr l)))
          )))
  )

(req-package fancy-compilation
  :hook (init-setup . fancy-compilation-mode)
  :config
  ;; Don't supress useful info
  (setq fancy-compilation-quiet-prelude nil
        fancy-compilation-quiet-prolog nil)
  )

(req-package fireplace
  :commands
  (fireplace)
  :bind
  (("C-x 3" . cemacs-fireplace-split-window-right)
   ("C-x 2" . cemacs-fireplace-split-window-below))
  :config
  ;; Variable Config
  (setq fireplace-smoke-on t
        fireplace-sound-file-path (concat user-emacs-directory "assets/fireplace.mp3"))
  ;; Paramter hack to allow the fireplace to resize properly
  (defun fireplace--update-locals-vars (&optional nil-window)
    "Update `fireplace' local variables."
    (setq fireplace--bkgd-height (- (floor (window-height (get-buffer-window fireplace-buffer-name))) 1)
          fireplace--bkgd-width  (- (round (window-width (get-buffer-window fireplace-buffer-name))) 1)
          fireplace--flame-width (min fireplace--bkgd-height (round (/ fireplace--bkgd-width 2.5)))
          fireplace--flame-pos fireplace-flame-pos))
  (defun fireplace--disable-minor-modes ()
    "Disable minor modes that might affect rendering."
    (switch-to-buffer fireplace-buffer-name)
    ;; Use local variables to avoid messing with the actual editing enviornment
    (setq-local truncate-lines t
                cursor-type nil
                show-trailing-whitespace nil
                indicate-empty-lines nil
                transient-mark-mode nil
                hl-line-mode nil
                ;; global-hl-line mode overrides the local hl-line-mode
                ;; *for some reason* and it's still called global-hl-line-mode
                ;; *even though* you can set 'global-hl-line-mode' as a buffer-local.
                global-hl-line-mode nil
                ;; non-standard emacs packages
                beacon-mode nil
                )
    ;; Reference the fireplace buffer in-case the current buffer
    ;; isn't the fireplace, for some reason.
    (buffer-disable-undo fireplace-buffer-name)
    )

  (defun cemacs-fireplace-draw (buffer-name)
    "Draw the whole fireplace in BUFFER-NAME from FLAME-POS with FLAME-WIDTH."
    ;; Save on performance when fireplace is hidden
    (when (cemacs-buffer-visiblep fireplace-buffer-name)
      (with-current-buffer (get-buffer fireplace-buffer-name)
        (if (not (eq major-mode 'fireplace-mode))
            (fireplace-off)
          (setq buffer-read-only nil)
          (fireplace--make-grid)
          (dolist (pos fireplace--flame-pos)
            (fireplace--flame (round (* pos fireplace--bkgd-width))
                              (+ (round (* (+ 0.2 (min pos (- 1 pos))) fireplace--flame-width))
                                 (random 3))))
          (setq buffer-read-only t))))
    )
  (advice-add 'fireplace-draw :override 'cemacs-fireplace-draw)

  (defun cemacs-fireplace-visit (&optional window)
    (interactive)
    (if (windowp window)
        (select-window window)
      (set 'window (selected-window)))
    (unless (get-buffer "*fireplace*")
      (fireplace))
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
  (add-hook 'after-make-frame-functions 'cemacs-fireplace-visit)
  )

(req-package flycheck
  :demand t
  :commands
  (flycheck-mode
   global-flycheck-mode)
  :require flycheck-inline
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-inline-mode)
  (global-flycheck-mode . global-flycheck-inline-mode)

  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang))
  ;; Disable highlighting, its not consistently useful, use helm-flycheck
  (setq flycheck-highlighting-mode nil
        flycheck-inline-mode t
        flycheck-indication-mode nil)
  ;; Disable annoying documentation warnings which are too strict
  ;; instead, use 'M-x checkdoc'
  ;; (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  ;; Fix flycheck not being able to find files in the load path
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (defun flycheck-inline--contains-point (phantom &optional pt)
  "Whether the given error overlay contains the position PT otherwise `(point)'

Fixed to display the error reguardless of where you are on a line.
It is faster and alleviates no syntax highlighting"
  (let* ((pos (or pt (point)))
         (err (overlay-get phantom 'error))
         (region (flycheck-error-region-for-mode err 'line)))
    (and phantom
         ;; Must be one of our phantoms (probably unneeded).
         (overlay-get phantom 'phantom)
         ;; The underlying error must currently exist.
         err
         (memq err flycheck-current-errors)
         ;; Most importantly, point must be within the error bounds.
         region
         (>= pos (car region))
         (<= pos (cdr region)))))

  )
(req-package free-keys
  :commands
  (free-keys
   free-keys-set-prefix
   free-keys-change-buffer)
  )

(req-package glsl-mode
  :commands (glsl-mode)
  :require company-glsl
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl))
  )

(req-package god-mode
  :commands (god-mode)
  :config
  )

;; TODO Try again with lucid toolkit
(req-package good-scroll
  :hook
  (cemacs-init-setup . good-scroll-mode)
  :bind
  (("C-v" . good-scroll-up-full-screen)
  ("M-v" . good-scroll-down-full-screen))
  :config
 )
(req-package helm
  :require
  helm-flycheck
  helm-projectile
  helm-rg
  helm-swoop
  :hook
  (cemacs-init-setup . helm-mode)
  (cemacs-init-setup . cemacs-helm-mode)
  :commands
  (cemacs-helm-mode
   helm-M-x
   helm-find-files
   helm-buffers-list
   helm-execute-persistent-action
   helm-select-action
   helm-swoop-without-pre-input
   helm-mini
   helm-bookmarks
   helm-projectile
   helm-flycheck
   helm-rg
   helm-projectile-rg)

  :bind
  (:map
   cemacs-helm-map
   ("M-x" .     helm-M-x)
   ;; ("C-x b" . helm-buffers-list)

   :map cemacs-query-prefix-map
   ("C-q" .     quoted-insert)          ; Keep replaced binding easily availiable
   ;; ("s" .       helm-swoop-without-pre-input)
   ("m" .       helm-mini)
   ("b" .       helm-bookmarks)
   ("f" .       helm-flycheck)
   ("r" .       helm-rg)
   ;; Projectile and lsp specific
   ("p" .       projectile-find-file)
   ("C-p" .     helm-projectile-rg)
   ("d" .       helm-lsp-diagnostics)

   ;; Swap Action and Completion Buttons
   :map helm-map
   ("TAB" .     helm-execute-persistent-action)
   ("<tab>" .   helm-execute-persistent-action)
   ("C-z" .     helm-select-action)
   ;; unbind non-emacs conforming bindings
   ("C-w" .     nil)                    ; append word at point
   ("C-SPC" .   nil)                    ; confusing and not useful marking behavour
   ;; Use backward delete word instead of invoking some auto-expansion toggle
   :map helm-find-files-map ("<C-backspace>" . nil)
   :map helm-projectile-find-file-map ("<C-backspace>" . nil)
   :map cemacs-helm-map ([remap find-file] . helm-find-files)

   :map projectile-mode-map
   ;; ("C-x p f" . helm-projectile-find-file)     ; Hangs on larger projects
   )
  :init
  ;; Custom keybinds
  (defvar cemacs-helm-map (make-sparse-keymap)
    "Custom mapping for cemacs-helm-mode")
  (define-minor-mode cemacs-helm-mode
    "Toggably custom functionality for helm-mode"
    :global t
    :keymap cemacs-helm-map
    )

  ;; Hide the helm-mode-line its pointless and noisy
    (defadvice helm-display-mode-line (after undisplay-header activate)
    (setq mode-line-format nil))

  :config
  ;;Helm minibuffer config
  (setq helm-autoresize-mode t
        helm-display-header-line t
        helm-header-line-space-before-prompt nil
        helm-autoresize-max-height 30   ;Always take up 30% the screen
        helm-autoresize-min-height 30
        helm-split-window-inside-p t    ;Shows helm window in current buffer
        helm-swoop-split-with-multiple-windows helm-split-window-inside-p
        helm-mode-line-string nil
        helm-use-frame-when-more-than-two-windows nil
        helm-find-files-ignore-thing-at-point t ; Nice idea but grabs input at the worst times
        helm-candidate-number-limit 150

        ;; Disable fuzzy matching for stuffs, it doesn't play nice with exact
        ;; match being ordered to the bottom
        helm-recentf-fuzzy-match nil
        helm-buffers-fuzzy-matching nil
        helm-locate-fuzzy-match nil
        helm-M-x-fuzzy-match nil
        helm-semantic-fuzzy-match nil
        helm-lisp-fuzzy-completion nil
        helm-session-fuzzy-match nil
        helm-etags-fuzzy-match nil
        )

  ;; ;Helm minibuffer config
  ;;TODO(mallchad) Need to reduce the size of the space after each helm source
  ;; Don't use helm's own displaying mode line function
  (defun cemacs-helm-define-faces ()
    (let ((comment-fg-color (face-attribute 'font-lock-comment-face :foreground))
          (default-bg-color (face-attribute 'default :background)))
      (set-face-attribute 'helm-source-header nil :height 1.1 :foreground "dark cyan")
      (set-face-attribute 'helm-header nil
                          :foreground comment-fg-color
                          :background default-bg-color
                          :box nil))
    )
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-helm-define-faces)

  (defvar cemacs-helm-rg-limit 1000
    "Specific helm-candidate limit for just helm-rg")
  (defun cemacs-ad-helm-rg (advised-func &rest args)
    "Set the limit of candidates to scroll through much higher"
    (let ((helm-candidate-number-limit cemacs-helm-rg-limit))
      (apply advised-func args))
    )
  (advice-add 'helm-rg :around 'cemacs-ad-helm-rg)
  )
(req-package highlight-parentheses
  :hook
  (cemacs-init-setup . global-highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors nil
        highlight-parentheses-background-colors '(nil)
        )
  (set-face-attribute 'highlight-parentheses-highlight nil
                      :box '(:line-width (-2 . -2)
                                         :color "Deeppink1"))
  )
(req-package hl-todo
  :demand t
  :hook
  (cemacs-init-setup . global-hl-todo-mode)
  )

(req-package hydra
  :demand nil
  :config
    )

;; Framework for fast, minimalistic searching
;; It appers to be more simple and faster than helm
;; helm-projectile can be unusable with large-projects, unlike ivy
(req-package ivy
  :bind
  (:map
   cemacs-query-prefix-map
   ("s" .       swiper)
   :map ivy-minibuffer-map
   ;; Make ivy use tab for candidate insertion
   ("TAB" .     ivy-insert-current)
   ("<tab>" .   ivy-insert-current)
   )
  :config
  ;; Increase minimum lines bring the candidate to the screen center
  (setq ivy-height 20
        ivy-fixed-height-minibuffer t
        ;; Disable ~ instantly sending you home
        ivy-magic-tilde nil
        )
  )

;; A minor mode to aid with json editing
(req-package json-mode
  :defer 5
  :commands
  (json-mode)
  :config
  )

(req-package lsp-mode
  :commands (lsp)
  :after
  company
  flycheck
  :require
  lsp-ui
  ;; :hook
  ;; (c++-mode . lsp)
  ;; (c-mode . lsp)
  ;; (csharp-mode . lsp)
  :bind
  (:map lsp-mode-map
        ("M-#" . lsp-ui-doc-glance)
        ;; ("M-o" . lsp-clangd-find-other-file)

        ;; Visual Studio Like Bindings
        ("<f12>" . lsp-find-definition)
        :map lsp-signature-mode-map
        ;; Unbind lsp-signature-previous
        ("M-p" . nil)
        )
  :config
  ;; Fixes
  (defalias 'yas-expand-snippet 'ignore)        ; Prevent company-capf from erroring
  ;; Configuration
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-enable-indentation nil
        ;; Disable auto-formatting which is conflusing and conflcits with indenters
        lsp-enable-on-type-formatting nil
        ;; Allow indexing in the background
        lsp-clients-clangd-args '("--header-insertion-decorators=0" "--background-index")
        ;; Disable warning of too many files being watched
        ;; TODO Make it look for projectile file
        lsp-file-watch-threshold nil
        ;; Don't bug the user to restart just restart
        lsp-restart 'auto-restart
        ;; Don't enable lsp-ui default, its slow and really noisy
        lsp-auto-configure nil
        lsp-auto-guess-root t
        )
  (setq
   ;; Mode the doc position to the top of the frame
   lsp-ui-doc-position 'top
   ;; Don't show docs on hover, instead use a command/binding
   lsp-ui-doc-enable nil
   ;; Disable lsp info heading
   lsp-headerline-breadcrumb-enable nil
   ;; Disable attention grabbing, screen moving, modeline signature documentation
   lsp-signature-render-documentation nil

   ;; Disable sideline, flycheck-inline is more readable and less intrusive
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-diagnostics nil
   )
  ;; Performance Tuning
  (setq
   ;; Source for performance improvements https://emacs-lsp.github.io/lsp-mode/page/performance/
   ;; Adjust gc-cons-threshold. The default setting is too low for lsp-mode's
   ;; needs due to the fact that client/server communication generates
   ;; a lot of memory/garbage.
   ;; gc-cons-threshold 100000000

   ;; Increase the amount of data which Emacs reads from the process.
   ;; Again the emacs default is too low 4k considering that the some of the
   ;; language server responses are in 800k - 3M range.
   read-process-output-max (* 1024 1024) ;; 1mb
   )

  (defvar cemacs-lsp-enable nil
    "Enable or disable enablig of lsp mode on a new buffer")
  (defun cemacs-ad-lsp (oldfun &rest args)
    (when (bound-and-true-p cemacs-lsp-enable)
      (apply 'oldfun args)))
  (advice-add 'lsp :around 'cemacs-ad-lsp)
  )
;; (req-package lsp-ui
;;   :config
;;   )
(req-package magit
  :commands
  (magit-status
   magit-stage
   magit-stage-file
   magit-unstage-file)
  :config
  (setq magit-diff-refine-hunk 'all         ; Show in-hunk differences
        magit-save-repository-buffers nil   ; Don't try to save before commit
        )
  )
;; Allows for quick insertion of licenses from github
(req-package license-templates
  )
(req-package lua-mode
  :hook
  (lua-mode . lsp)
  :commands
  (lua-mode
   cemacs-nelua-mode)

  :init
  ;; For Nelua
  (add-to-list 'auto-mode-alist '("\\.nelua\\'" . cemacs-nelua-mode))

  :config
  (defun cemacs-nelua-mode (&optional arg)
    "Makes sure only to enable lsp in valid lua files"
    (lua-mode))
  (if (file-exists-p "/usr/bin/lua-language-server")
      (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"))
  )
(req-package multiple-cursors
  :commands
  (mc/edit-lines
   mc/mark-all-in-region-regexp)
  :after hydra
  :bind
  ((;; Need to find a new home for this
    "M-m" . mc/edit-lines)
   :map mc/keymap
   ;; Don't exit on newline
   ("<return>" . nil)
   ("RET" . nil)
   )
  :config
  ;; Mark defualt behaviour for commands

  (cemacs-add-multiple-to-list 'mc/cmds-to-run-for-all
                               #'cemacs-delete-word
                               #'cemacs-delete-word-backwards
                               #'natural-delete-word-backward
                               #'natural-delete-word-backwards
                               #'natural-forward-word
                               #'natural-beginning-of-line
                               #'natural-end-of-line
                               #'crux-move-beginning-of-line
                               #'indent-for-tab-command
                               #'kill-region
                               #'org-delete-char
                               #'org-self-insert-command
                               #'tab-to-tab-stop
                               )

  (cemacs-add-multiple-to-list 'mc/cmds-to-run-once
                               #'helm-M-x
                               #'helm-confirm-and-exit-minibuffer
                               )
  )

(req-package omnisharp
  ;; :hook
  ;; (csharp-mode . omnisharp-mode)
  :commands
  (omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  )
;; Personal knowledge-base extensions for org-mode
(req-package org-roam
  ;; Unused
  )
(req-package org-roam-ui
  ;; Unused
  )
(req-package org-super-agenda
  :config
  (org-super-agenda-mode 1)
  )
(req-package origami
  :require lsp-origami
  :commands
  (origami-mode)
  :config
  ;; TODO(mallchad) need to setup keybinds for this package
  ;; Mainly just origami toggle node, and open/close recursively
  )

(req-package flycheck-projectile
  )

(req-package projectile
  :require
  flycheck-projectile
  :commands (projectile-mode)
  :hook
  (cemacs-init-setup . projectile-mode)
  (find-file . cemacs-projectile-file-setup)
  (after-revert . cemacs-projectile-file-setup) ; redo with buffer revert
  :bind
  (:map projectile-mode-map
        (("C-x p" . projectile-command-map)))
  :config

  ;; Variables
  (setq projectile-enable-caching t
        ;; --strip-cwd-prefix only supported in newer versions of fd
        projectile-git-fd-args "-H -0 -E .git -tf -c never"
        )

  (defvar cemacs-projectile-functions
    '(())
    )
  (defvar cemacs-projectile-locals '(())
    "Sets read-only config variables for a project.

This is for setting variables for other functions, and is not
meant to be used to store information for any amount of time.

The expected value is an associative list, where the car of
each con cells is the project in question, and the cdr is
a list of symbols and values.

For example
((unreal (tab-width 4)
         (indent-tabs-mode t)
(cemacs (fill-column 80))
)
"
    )
  (defvar cemacs-projectile-grouping '(())
    "This variable contains what groups projects are associated with.

The underlying system does not actually have a concept of what a group is,
a group is simply a generic 'project' designed to be used for other projects,
as oppose to on their own.

The expected value of the variable is an associative list,
 specificly using cons cells,
where, the expected car of each cons cell is the name of the project,
and the cdr is a list of groups to be 'bound' to the project.

For example
((UnrealTournament unreal)
(cemacs elisp)
)
"
    )
  (defvar cemacs-projectile-detect-functions '()
    "A list of functions that run heuristics to try to determine the ideal project-local settings to apply to the buffer/project. There can be multiple project settings applied to the local project and the last one in the list takes precedence. Projects can also be assigned under a group to share project settings with.")
  ;; TODO Detect projcet type base on uproj / function

  (defun cemacs-projectile-file-setup ()
    (when (and projectile-mode (projectile-project-p))

      ;; Setup project-local options
      (condition-case nil
          (let* ((current-project (projectile-project-name))
                 (current-project-root (projectile-project-name))
                 (current-project-function
                  (cdr (assoc current-project
                              cemacs-projectile-functions)))
                 (current-group-functions nil)
                 (current-project-groups (cdr (assoc current-project cemacs-projectile-grouping)))
                 (current-project-locals (cdr (assoc
                                               current-project
                                               cemacs-projectile-locals)))
                 (current-locals-copy nil)
                 (lexical-binding t)
                 )

            ;; Grab the hook functions associated with the project
            (dolist (x-group current-project-groups)
              (cemacs-push-list current-project-locals
                                (reverse (cdr (assoc x-group cemacs-projectile-locals))))
              (cemacs-push-list current-group-functions
                                (cdr (assoc x-group cemacs-projectile-functions)))
              )

            ;; Run the hook functions associated with the project
            (dolist (x-function current-group-functions)
              (when (functionp x-function)
                (funcall x-function)))

            ;; Sanitize and copy for element popping
            (set 'current-locals-copy (remove nil cemacs-projectile-locals))
            (set 'current-locals-copy current-project-locals)
            ;; Apply the project-local variables to the newley opened buffer
            (let* ((x-symbol nil)
                   (x-value nil))
              (while (> (cl-list-length current-locals-copy) 0)
                (set 'x-symbol (pop current-locals-copy))
                (set 'x-value (pop current-locals-copy))
                (when (and (symbolp x-symbol)
                           (bound-and-true-p x-value))
                  (eval `(setq-local ,x-symbol ,x-value)))))

            ;; Record what was attempted to be applied to the local buffer
            (when (bound-and-true-p cemacs-debug-enabled)
              (message "cemacs-project: Enabled following in buffer \n
project: %S \n group: %S \n functions: %S \n locals: %S"
                       current-project current-project-groups
                       current-group-functions current-project-locals))
            (setq-local
             cemacs-project-name current-project
             cemacs-project-groups current-project-groups
             cemacs-project-locals current-project-locals
             cemacs-project-functions current-group-functions)
            ))))
  )

(req-package rainbow-blocks
  :commands
  (rainbow-blocks-mode
   cemacs-rainbow-blocks-define-faces)

  :config
  (defun cemacs-rainbow-blocks-define-faces ()
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
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-rainbow-blocks-define-faces)
  )

(req-package rainbow-delimiters
  :commands
  (rainbow-delimiters-mode)
  :hook
  (prog-mode . rainbow-delimiters-mode)

  :init
  (defun cemacs-rainbow-delimiters-define-faces ()
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
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-rainbow-delimiters-define-faces)
  )

(req-package rainbow-mode
  :commands
  (rainbow-mode)
  :hook
  (emacs-lisp-mode . rainbow-mode)
  )

(req-package restart-emacs
  :commands
  (restart-emacs)
  :bind
  (("C-x M-a" . restart-emacs))
  :config
  )

(req-package rtags
  :require
  company-rtags
  company
  rtags-xref
  :hook
  (c-mode . rtags-xref-enable)
  (c++-mode . rtags-xref-enable)
  :commands
  (rtags-xref-enable
   rtags-xref)
  :config
  ;; (add-to-list 'company-backends 'company-rtags)
  (defvar cemacs-rtags-procs '()
    "List of procs to clean up")

  (cl-defun cemacs-ad-rtags-call-rc (&rest arguments
                                           &key (path (rtags-buffer-file-name))
                                           unsaved
                                           async ;; nil or a cons (process-filter . sentinel)
                                           path-filter
                                           path-filter-regex
                                           range-filter
                                           (output (list t nil)) ; not supported for async
                                           range-min
                                           range-max
                                           noerror
                                           timeout
                                           silent
                                           silent-query
                                           &allow-other-keys)
  (save-excursion
    (let ((rc (rtags-executable-find rtags-rc-binary-name))
          (tempfile))
      (dolist (x-proc cemacs-rtags-procs)
        (when (processp x-proc)
          (async-get (pop cemacs-rtags-procs))
          (when (bound-and-true-p cemacs-debug-enabled)
            (message "deleting async rc"))
          ;; (async-wait x-proc)
          ))
      (if (not rc)
          (unless noerror (rtags--error 'rtags-cannot-find-rc))
        (setq output (rtags--convert-output-buffer output))
        (setq rtags-last-request-not-connected nil)
        (setq rtags-last-request-not-indexed nil)
        (setq arguments (rtags-remove-keyword-params arguments))
        (setq arguments (rtags-remove '(lambda (arg) (not arg)) arguments))
        (setq arguments (mapcar 'rtags-untrampify arguments))
        ;; other way to ignore colors would IMHO be to configure tramp,
        ;; but: do we need colors from rc?
        (when rtags-verify-protocol-version
          (push (format "-t%d" rtags-protocol-version) arguments))
        (push "-z" arguments)
        (setq path (rtags-untrampify path))
        (when path-filter
          (push (concat "--path-filter=" (rtags-untrampify path-filter)) arguments)
          (when path-filter-regex
            (push "-Z" arguments)))
        (when (and unsaved (rtags-buffer-file-name unsaved))
          (setq tempfile (make-temp-file "/tmp/"))
          (push (format "--unsaved-file=%s:%s" (rtags-untrampify (rtags-buffer-file-name unsaved)) tempfile) arguments)
          (with-current-buffer unsaved
            (save-restriction
              (widen)
              (rtags--write-region (point-min) (point-max) tempfile))))
        (when rtags-rc-config-path
          (push (concat "--config=" (expand-file-name rtags-rc-config-path)) arguments))
        (when rtags-completions-enabled
          (push "-b" arguments))
        (when silent
          (push "--silent" arguments)
          (setq output nil))
        (when silent-query
          (push "--silent-query" arguments))
        (when range-filter
          (push (format "--range-filter=%d-%d"
                        (or range-min (rtags-offset (point-min)))
                        (or range-max (rtags-offset (point-max))))
                arguments))
        (when (or timeout rtags-timeout)
          (push (format "--timeout=%d" (or timeout rtags-timeout)) arguments))
        (when (and rtags-show-containing-function (not (member "-N" arguments)))
          (push "-o" arguments))

        (cond ((stringp path) (push (concat "--current-file=" path) arguments))
              (path nil)
              (default-directory (push (concat "--current-file=" (rtags-untrampify default-directory)) arguments))
              (t nil))

        (when (> (length rtags-socket-file) 0)
          (push (rtags--get-socket-file-switch) arguments))

        (when (> (length rtags-socket-address) 0)
          (push (rtags--get-socket-address-switch) arguments))

        (when rtags-rc-log-enabled
          (rtags-log (concat rc " " (rtags-combine-strings arguments))))
        (if async
            (when rtags-rc-log-enabled
              (rtags-log "running previous command asynchronously"))
            (let ((proc (apply #'async-start-process
                               rtags-rc-binary-name
                               rc
                               'ignore
                               arguments)))
              (push proc cemacs-rtags-procs)
              (set-process-query-on-exit-flag proc nil)
              (when (listp async)
                  (when (car async)
                (set-process-filter proc (car async)))
                (when (cdr async)
                  (set-process-sentinel proc (cdr async))))
              t)
          (let ((result (apply #'process-file rc nil output nil arguments)))
            (goto-char (point-min))
            (save-excursion
              (cond ((equal result rtags-exit-code-success)
                     (when rtags-autostart-diagnostics
                       (rtags-diagnostics)))
                    ((equal result rtags-exit-code-connection-failure)
                     (when output
                       (erase-buffer))
                     (setq rtags-last-request-not-connected t)
                     (unless noerror
                       (rtags--error 'rtags-rdm-not-running)))
                    ((equal result rtags-exit-code-protocol-failure)
                     (when output
                       (erase-buffer))
                     (unless noerror
                       (rtags--error 'rtags-protocol-mismatch)))
                    ((equal result rtags-exit-code-not-indexed)
                     (unless noerror
                       (rtags--message 'rtags-file-not-indexed (or path "buffer")))
                     (erase-buffer)
                     (setq rtags-last-request-not-indexed t))
                    ((equal result "Aborted")
                     (rtags--error 'rtags-program-exited-abnormal rtags-rc-binary-name result))
                    (t))) ;; other error
            (and (> (point-max) (point-min)) (equal result rtags-exit-code-success)))))))
  )

(defun cemacs-ad-rtags-update-buffer-list ()
    "Send the list of indexable buffers to the rtags server, rdm,
so it knows what files may be queried which helps with responsiveness.
"
    (interactive)
    ;; (message "rtags-update-buffer-list")
    (when rtags-enabled
      (let* ((visible (rtags-visible-buffers))
             (list)
             (arg))
        (mapc (lambda (buf)
                (cond ((memq buf visible)
                       (push (cons buf t) list))
                      ((funcall rtags-is-indexable buf)
                       (push (cons buf nil) list))
                      (t))) (buffer-list))
        (setq arg (concat "--set-buffers=" (mapconcat (lambda (arg)
                                                        (concat (if (cdr arg) "+" "-") (rtags-buffer-file-name (car arg)))) list ";")))
        (when rtags-rc-log-enabled
          (rtags-log (concat "--set-buffers files: " arg)))
        (when (not (string= rtags-previous-buffer-list arg))
          (setq rtags-previous-buffer-list arg)
          (rtags-call-rc :noerror t
                         :silent-query t
                         :async t
                         :output nil
                         :silent t
                         :path t
                         arg))))
    )
  (advice-add 'rtags-update-buffer-list :override 'cemacs-ad-rtags-update-buffer-list)
  (advice-add 'rtags-call-rc :override 'cemacs-ad-rtags-call-rc)
  )

(req-package rust-mode
  )

(req-package smartparens
  :after aggressive-indent
  :commands
  (smartparens-global-mode
   smartparens-mode
   cemacs-smartparens-enforcer-mode
   cemacs-smartparens-global-enforcer-mode
   sp-kill-whole-line
   c-sp-natural-delete-word
   c-sp-natural-delete-word-backwards
   sp-kill-region)
  :hook
  (cemacs-init-setup . smartparens-global-mode)
  (smartparens-mode . cemacs-smartparens-mode)
  ;; (prog-mode . cemacs-smartparens-enforcer-mode)
  ;; Fix for *scratch* loading before this is hooked
  ;; (lisp-interaction-mode . cemacs-smartparens-enforcer-mode)
  :bind
  (:map smartparens-mode-map
        ;; Pair management bindings which are required for strict-mode
        ("M-<backspace>" .              sp-backward-unwrap-sexp)
        ;; ("M-e" .                     sp-forward-slurp-sexp)
        ;; ("M-a" .                     sp-backward-slurp-sexp)
        ;; ("M-[" .                     sp-forward-barf-sexp)
        ;; ("M-]" .                     sp-backward-barf-sexp)

        ;; General Pair Manamagent and Navigation
        ;; ("C-M-k" .                      sp-kill-sexp)       ; Allow killing by pair
        ("C-M-f" .                      sp-forward-sexp)
        ("C-M-b" .                      sp-backward-sexp)
        ("M-n" .                        sp-kill-hybrid-sexp))
  :init
  (defvar cemacs-smartparens-enforcer-mode-map nil
    "Keymap used for `cemacs-smartparens-enforcer-mode'.")
  :config
  ;; Defualt Configuration
  (require 'smartparens-config)

  ;; Keybinds
  ;; Enforce smartparens-strict-mode with keybinds
  (defvar cemacs-smartparens-enforcer-mode-map nil
    "Keymap used for `cemacs-smartparens-enforcer-mode'.")
  (setq cemacs-smartparens-enforcer-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [remap kill-word] 'sp-kill-word)
          (define-key map [remap kill-line] 'sp-kill-hybrid-sexp)
          (define-key map [remap backward-kill-word] 'sp-backward-kill-word)
          (define-key map [remap kill-region] 'sp-kill-region)
          (define-key map [remap delete-region] 'sp-delete-region)
          (define-key map [remap kill-whole-line] 'sp-kill-whole-line)
          (define-key map (kbd "<C-backspace>") #'c-sp-natural-delete-word-backwards)
          (define-key map (kbd "M-d") #'c-sp-natural-delete-word)
          map)
        )

  ;; Variables
  (setq-default sp-autoinsert-pair nil          ; More trouble than it's worth
                sp-autoskip-closing-pair nil
                ;; Don't insert escape charsacters
                sp-escape-quotes-after-insert nil
                sp-escape-wrapped-region nil
                )

  ;; Mode Hook
  (defun cemacs-smartparens-mode ()
    ;; Disable automatic escape char insertion
    (setq-local sp-escape-char "")
    )

  ;; smartparens Custom Adapted Logic
  (defun c-sp-natural-delete-word (&optional arg)
    "Modified version of `natural-delete-word' for smartparens"
    (interactive)
    (let ((original-point (point)))
      ;; Following two characters are whitespace/blank or end of line
      (cond ((= (line-end-position) (point))
             (cemacs-forward-whitespace :traverse-newlines)
             (sp-delete-region original-point (point)))
            ;; Following two characters are whitespace/blank
            ((and (string-match "[[:blank:]]" (string (char-after)))
                  (string-match "[[:blank:]]" (string
                                               (char-after (+ (point) 1)))))
             ;; traverse all whitespace upto line beginning
             (cemacs-forward-whitespace)
             (sp-delete-region original-point (point)))
            (:default (sp-kill-word 1)))
      (constrain-to-field nil (point)))
    )
  (defun c-sp-natural-delete-word-backwards ()
    "Modified version of `natural-delete-word-backwards' for smartparens"
    (interactive)
    (let ((original-point (point))
          (original-line (line-number-at-pos (point)))
          )
      ;; Delete whitespace hungrily if at line beginning across lines
      (cond ((= (line-beginning-position) (point))
             (cemacs-backward-whitespace :traverse-newlines)
             (sp-delete-region original-point (point))
             )
            ;; Previous two characters are whitespace/blank
            ((and (string-match "[[:blank:]]" (string (char-before)))
                  (string-match "[[:blank:]]" (string (char-before
                                                       (- (point) 1))
                                                      )))
             ;; traverse all whitespace upto line beginning
             (cemacs-backward-whitespace)
             (sp-delete-region original-point (point))
             )
            (:default
             (sp-backward-kill-word 1)
             (if (not (= original-line (line-number-at-pos (point))))
                 ;; Try not to move the cursor too far
                 (end-of-line))
             )
            )
      (constrain-to-field nil original-point)
      (point))
    )
  ;; Keybinds
  ;; Enforce smartparens-strict-mode with keybinds

  (cemacs-add-multiple-to-list 'aggressive-indent-protected-commands
                               #'c-sp-natural-delete-word
                               #'c-sp-natural-delete-word-backwards
                               #'sp-delete-char
                               #'sp-backward-delete-char
                               #'sp-kill-word
                               #'sp-backward-kill-word
                               #'sp-kill-symbol
                               #'sp-backward-kill-symbol
                               #'sp-delete-symbol
                               #'sp-backward-delete-symbol
                               #'sp-delete-region
                               #'delete-region)
  ;; Disable Emacs Lisp Quote Pairs
  (sp-local-pair sp-lisp-modes  "'" 'nil :actions 'nil)
  (sp-local-pair sp-lisp-modes  "`" 'nil :actions 'nil)

  ;; Pair angled brackets in c-modes
  ;; (sp-local-pair sp-c-modes "<" ">")    ; Breaks bitwise operators :/

  (define-minor-mode cemacs-smartparens-enforcer-mode
    "Toggle smartparens mode but enforce balancing of sexps more.

  This mode tries harder to balance pairs, but isn't as aggressive as
   smartparens-strict-mode.
  This is designed to help prevent delimiter based syntax errors
  This has the downside of being quite aggressive with interfering with editing,
  but, this is probably worth it, for the early 'alarm' for syntax errors.

  This mode, unlike `smartparens-strict-mode' does not affect all commands,
  particularly `delete-char' still can delete pairs as normal, so that can
  be used as an effective alternative to advanced sexp editing.
  The current content of
  `cemacs-smartparens-enforcer-mode-map' is:

   \\{cemacs-smartparens-enforcer-mode-map}"
    :keymap cemacs-smartparens-enforcer-mode-map
    )
  (define-globalized-minor-mode cemacs-smartparens-global-enforcer-mode
    cemacs-smartparens-enforcer-mode
    ignore
    )
  )
(req-package smooth-scrolling
  :hook
  (cemacs-init-setup . smooth-scrolling-mode)
  :config
  (setq scroll-conservatively 0)
  )
(req-package smart-yank
  :commands
  (smart-yank-mode
   smart-yank-pop
   )
  :config
  )
;; (req-package sublimity
;;   :commands
;;   (sublimity-mode)
;;   ;; :hook
;;   ;; (cemacs-init-setup . sublimity-mode)
;;   ;; :config
;;   ;; (require 'sublimity-scroll)
;;   ;; (require 'sublimity-attractive)
;;   ;;(require 'sublimity-map) ;; experimental
;;   ;; (setq sublimity-scroll-weight 4
;;   ;; sublimity-scroll-drift-length 4
;;   ;;      sublimity-map-size 10
;;   ;;      sublimity-map-fraction 0.3
;;   ;;      sublimity-map-text-scale -7
;;   ;;      sublimity-map-set-delay 1
;;   ;; )
;;   )

;; A utility for searching through results relative to point in buffer
;; Happens to have more untuitive defaults and coloring than helm-swoop
(req-package swiper
  :config
  )

(req-package tree-sitter
  :require tree-sitter-langs
  :hook
  (prog-mode . tree-sitter-hl-mode)
  (cemacs-init-setup . global-tree-sitter-mode)
  )

;; Machine learning powered completion framework, free tier
;; (req-package company-tabnine
;; :config
;; (add-to-list 'company-backends 'company-tabnine)
;; )

(req-package undo-tree
  :bind
  (("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo)

   ;; Unbind Included Keymaps
   :map undo-tree-map
   ("C-x r u" . nil)
   ("C-x r U" . nil)
   ("C-x r" .   nil))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-enable-undo-in-region nil  ; brings performance enhancement
        undo-tree-history-dir (concat cemacs-var-dir "undo-tree-history")
        undo-tree-history-directory-alist `(( ".*" . ,undo-tree-history-dir))
        undo-tree-auto-save-history nil ; Improved performance when off
        )
  )
(req-package visible-mark
  ;; A minor mode to show you where the mark is current
  :commands
  (visible-mark-mode
   global-visible-mark-mode)
  :hook
  (cemacs-init-setup . global-visible-mark-mode)

  :config
  (setq visible-mark-max 2)
  (defface cemacs-visible-mark-common () "")
  (defface cemacs-visible-mark-1 () "")
  (defface cemacs-visible-mark-2 () "")
  (defface cemacs-visible-mark-3 () "")
  (defface cemacs-visible-mark-4 () "")

  (defun cemacs-visible-mark-define-faces ()
    (set-face-attribute 'visible-mark-active nil :background "red")
    (set-face-attribute 'cemacs-visible-mark-common nil :box '(:line-width (-2 . -2)))
    (set-face-attribute 'cemacs-visible-mark-1 nil :background "deep pink")
    (set-face-attribute 'cemacs-visible-mark-2 nil :inherit 'cemacs-visible-mark-common)
    (set-face-attribute 'cemacs-visible-mark-3 nil :inherit 'cemacs-visible-mark-common)
    )
  (add-hook 'cemacs-after-load-theme-hook 'cemacs-visible-mark-define-faces)
  (setq visible-mark-faces '(cemacs-visible-mark-1 cemacs-visible-mark-2))
  )
(req-package vlf
  :config
  (require 'vlf-setup)
  )
(req-package visual-fill-column
  :config
  (setq visual-fill-column-width 80)
  )
(req-package volatile-highlights
  :config
  (volatile-highlights-mode)
  )
(req-package ws-butler
  ;; Unobtrusively clean up extrenuous whitespace and convert tabs to whitespace
  :config
  (ws-butler-global-mode)
  ;; Somebody though setting to 't' was a good idea, defeats the point and is
  ;; aggressively follows 'indent-tabs-mode'.
  (setq ws-butler-convert-leading-tabs-or-spaces nil
        )
  (add-hook 'org-mode-hook #'(lambda ()
                               (setq-local ws-butler-mode nil)))
  )
(req-package which-key
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 2          ; Don't get in the way of normal usage
        which-key-sort-order 'which-key-description-order
        )
  )
(req-package yasnippet
  :require
  auto-yasnippet
  )
(req-package auto-yasnippet
  :config
  )
(req-package zoom
  ;; A window rebalancing minor mode
  :config
  (zoom-mode)
  ;; Tweak tweak rebalancing ratios
  (setq zoom-size '(70 . 30))
  )

;; Finalize Initialization
;; Solve Dependencies and Load in Correct Order
;; Order here doesn't matter
(req-package-finish)
(cemacs-init-setup)

(provide 'init)
;;; init.el ends here
