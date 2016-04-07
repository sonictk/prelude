;;; package --- preload-config
;;; Commentary:

;; Personal custom configuration for Emacs
;; This is run before Prelude, and is intended to be used with Prelude.

;; Fix for bug on Macbook Pro Fedora where Emacs hangs when loading
;; See https://github.com/bbatsov/prelude/issues/896 for details.
(setq tramp-ssh-controlmaster-options)

;;; Code:
; Set default fonts and sizes
(add-to-list 'initial-frame-alist '(font . "Literation Mono Powerline-10"))
(add-to-list 'default-frame-alist '(font . "Literation Mono Powerline-10"))
(set-face-attribute 'default nil :font "Literation Mono Powerline-10")
; Check if running on Macbook based off hostname and set the font size accordingly
(if (string-equal system-name "sonictk-mbp.local") 
    ;; Set custom font as default global font
    (add-to-list 'default-frame-alist '(font . "Literation Mono Powerline-12"))
    (add-to-list 'initial-frame-alist '(font . "Literation Mono Powerline-12"))
    (set-face-attribute 'default nil :font "Literation Mono Powerline-12")
)
; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

; Define alias for Python in order to prevent mule from throwing warnings
(define-coding-system-alias 'UTF-8 'utf-8)
