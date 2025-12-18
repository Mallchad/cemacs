;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;;; personal.el --- A file dedicated to highly opinionated, situation dependent, or personal-to-user configuation

;; The values for this variables was defined here since spltting the places for
;; setting the value required re-evaluating in two plces.
;; At least here you can evaluate the file,
;; or use the provided `cemacs-personal-reload-config'.

(defvar personal-projectile-grouping
  '(("test_project" "unreal")
    ("UnrealEngine" "unreal")
    ("experiments_rendering" "xmake")
    ("tachyon_engine" "xmake")
    ("battleships" "cmake")
    ))
(defvar personal-projectile-locals
  '(("UnrealEngine"
     indent-tabs-mode t)
    ("UnrealEngine4"
     indent-tabs-mode t
     )
    ("xmake"
     projectile-project-compilation-cmd "xmake build")
    ("cmake"
     projectile-project-compilation-cmd "xmake build")
    ("raddebugger"
     tab-width 2
     c-basic-offset 2)
    ))

(defun personal-init-hook ()
  (cemacs-add-multiple-splicing 'cemacs-projectile-locals
                                personal-projectile-locals)
  (cemacs-add-multiple-splicing 'cemacs-projectile-grouping
                                personal-projectile-grouping)
)
(add-hook 'personal-init-hook 'cemacs-init-setup-hook)

;; Manual load code
(when (bound-and-true-p cemacs-init-complete)
  (personal-init-hook)
  )

(provide 'personal)
;;; cemacs-utility.el ends here
