;; MIT License
;; Copyright (c) 2021 Mallchad
;; This source is provided with no limitations or warrenty whatsoever.

;;; personal.el --- A file dedicated to highly opinionated, situation dependent, or personal-to-user configuation

;; The values for this variables was defined here since spltting the places for
;; setting the value required re-evaluating in two plces.
;; At least here you can evaluate the file,
;; or use the provided `cemacs-personal-reload-config'.

(cemacs-add-multiple-splicing 'cemacs-projectile-grouping
                              '(("test_project" "unreal")
                                ("UnrealEngine" "unreal")
                                ))
(cemacs-add-multiple-splicing 'cemacs-projectile-project-locals
                              '(("UnrealEngine" .
                                 (indent-tabs-mode t))
                                ("UnrealEngine4" .
                                 (indent-tabs-mode t)
                                 )))

(provide 'personal.el)
;;; cemacs-utility.el ends here
