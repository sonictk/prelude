; Blinking cursor
(blink-cursor-mode 1)

; Set cursor display attributes
(setq-default cursor-type 'bar)

; Save the window layout on exit
(desktop-save-mode 1)

; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
