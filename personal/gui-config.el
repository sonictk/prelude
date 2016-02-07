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
(setq-default show-trailing-whitespace t)

; Disables the notification sound when scrolling past EOF, among other things
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

; Auto reload-buffers when files are changed on disk
(global-auto-revert-mode t)

; Enable Emacs Development Environment mode
(global-ede-mode t)
