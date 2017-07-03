;;; package --- preload-config
;;; Commentary:

;; Personal custom configuration for Emacs
;; This is run before Prelude, and is intended to be used with Prelude.

;; Fix for bug on Macbook Pro Fedora where Emacs hangs when loading
;; See https://github.com/bbatsov/prelude/issues/896 for details.
(setq tramp-ssh-controlmaster-options)

;;; Code:
;; Set custom font as default global font
;(add-to-list 'default-frame-alist '(font . "Literation Mono Powerline-10"))
;(set-face-attribute 'default nil :font "Literation Mono Powerline-10")
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Liberation Mono" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
    (set-face-attribute 'default nil :font "Liberation Mono-11.5")))
 ((string-equal system-type "darwin") ; Mac OS X
  (when (member "Liberation Mono" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
    (set-face-attribute 'default nil :font "Liberation Mono-11.5")))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Liberation Mono" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
    (set-face-attribute 'default nil :font "Liberation Mono-11.5"))))

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

; Define alias for Python in order to prevent mule from throwing warnings
(define-coding-system-alias 'UTF-8 'utf-8)
