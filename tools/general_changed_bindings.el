;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;; This script is reliant on use-package being loaded.
;; It is also reliant on the 'bind-keys' package to obtain the definitions.
;; This file is considered a script because it only exists for documentation reasons,
;; the documentation is a part of the project and irrelevant to usage of the config.

(require 'cl-lib)
(setq use-package-always-defer t)
(load-file "~/.emacs.d/init.el")

(let* ((binding-buffer-name "*Personal Keybindings*")
       (user-init-file (file-truename "~/.emacs.d/init.el"))
       (project-root "../build/")
       (generated-output-file (concat project-root "doc/personal_bindings"))
       (use-package-always-defer t))

  (make-empty-file generated-output-file :make-parents)
  (load-file user-init-file)
  (describe-personal-keybindings)
  (cl-assert (get-buffer binding-buffer-name) "Could not find the binding file")
  (with-current-buffer "*Personal Keybindings*"
    (write-region nil nil generated-output-file))
  )
