; Turn off the toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)

; Blinking cursor
(blink-cursor-mode 1)

; Set cursor display attributes
(setq-default cursor-type 'bar)

; Save the window layout on exit
(desktop-save-mode 1)

; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Show matching parentheses
(show-smartparens-mode 1)

; Enable multiple cursors
(require 'multiple-cursors)

; Show whitespace
(require 'whitespace)
(setq whitespace-style (quote
   (face spaces tabs space-mark tab-mark)))

; Disables the notification sound when scrolling past EOF, among other things
(setq visible-bell 1)

; Auto reload-buffers when files are changed on disk
(global-auto-revert-mode t)

; Enable Emacs Development Environment mode
(global-ede-mode t)

; Set permanent display of line numbers
; TODO: This is causing bugs all over the GUI, figure out a better solution
; (global-nlinum-mode t)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()   "Show line numbers temporarily, while prompting for the line number input"   (interactive)   (unwind-protect
      (progn
        (nlinum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (nlinum-mode -1)))

; Save desktop and buffers/positions/modes between Emacs sessions
(desktop-save-mode 1)

; Scroll just one line when hitting bottom of window
(setq scroll-conservatively 10000)

; Disable linum-mode for speedbar
(add-hook 'speedbar-mode-hook (lambda () (nlinum-mode -1)))

; Disable word wrapping by default
(set-default 'truncate-lines t)

; Disable prelude auto cleaning up whitespace on file save
(setq prelude-clean-whitespace-on-save nil)
