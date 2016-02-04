;; Personal custom configuration for Emacs
;; This is run before Prelude, and is intended to be used with Prelude.

;; Set custom font as default global font
(add-to-list 'default-frame-alist '(font . "Literation Mono Powerline-10"))
(set-face-attribute 'default nil :font "Literation Mono Powerline-10")

;; This sets the default Emacs theme
(setq prelude-theme 'monokai)

; Set permanant display of line numbers
(global-nlinum-mode t)
