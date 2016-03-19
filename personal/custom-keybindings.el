; Remap the command and alt keys on OSX
; Also allows for fn-delete to be right-delete
(if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
    (global-set-key [kp-delete] 'delete-char)
)

; Set key bindings for kill word and backward kill word that are overridden by Prelude
(global-set-key (kbd "C-<backspace>") 'backward-kill-word)
    (global-set-key (kbd "C-<delete>") 'kill-word)

; Set key bindings for in-place scrolling of window
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "M-p") 'scroll-down-line)

; Bindings for `multiple-cursors`
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

; Unbind MMB
(global-unset-key (kbd "<mouse-2>"))
(global-set-key (kbd "<mouse-2>") 'mc/add-cursor-on-click)

; Unbind suspending the frame and bind it to undo instead
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

; Bindings for smooth horizontal scrolling
(global-unset-key (kbd "C-x >"))
(global-unset-key (kbd "C-x <"))
(global-set-key (kbd "C-x >") '(lambda ()(interactive)(scroll-left 15)))
(global-set-key (kbd "C-x <") '(lambda ()(interactive)(scroll-right 15)))

; Bindings for commenting
(global-unset-key (kbd "C-c C-c"))
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

; Bindings for opening recent files
(global-set-key (kbd "C-x C-S-f") 'recentf-open-files)

; Binding for newline
(global-set-key (kbd "<C-return>") 'prelude-smart-open-line)
