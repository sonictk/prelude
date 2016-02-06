;;; package --- preload-config
;;; Commentary:

;; Personal custom configuration for Emacs
;; This is run before Prelude, and is intended to be used with Prelude.

;;; Code:
;; Set custom font as default global font
(add-to-list 'default-frame-alist '(font . "Literation Mono Powerline-10"))
(set-face-attribute 'default nil :font "Literation Mono Powerline-10")

;; This sets the default Emacs theme
(setq prelude-theme 'monokai)

; Set permanent display of line numbers
(global-nlinum-mode t)

; Save desktop and buffers/positions/modes between Emacs sessions
(desktop-save-mode 1)
