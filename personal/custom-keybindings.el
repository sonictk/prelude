; Remap the command and alt keys on OSX
; Also allows for fn-delete to be right-delete
(if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
    (global-set-key [kp-delete] 'delete-char)
)

; Set key binding to toggle subword mode
(global-set-key (kbd "C-c C-w") 'subword-mode)

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

; Bind additional yank command hotkey
(global-set-key (kbd "C-S-v") 'yank)

; Set new keybinding for browsing the kill ring
(global-set-key (kbd "C-c y") 'browse-kill-ring)

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
(defun comment-or-uncomment-region-or-line()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(global-unset-key (kbd "C-c C-c"))
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

; Bindings for opening recent files
(global-set-key (kbd "C-x C-S-f") 'recentf-open-files)

; Binding for newline
(global-set-key (kbd "<C-return>") 'prelude-smart-open-line)

; Binding for duplicating lines
(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)

; Bindings for mousewheel horizontal scrolling
(global-set-key (kbd "<S-wheel-down>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-double-wheel-down>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-triple-wheel-down>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<wheel-right>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<wheel-left>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-wheel-up>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-double-wheel-up>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-triple-wheel-up>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<S-mouse-4>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<S-mouse-5>") '(lambda nil (interactive) (scroll-left 15)))
(global-set-key (kbd "<mouse-6>") '(lambda nil (interactive) (scroll-right 15)))
(global-set-key (kbd "<mouse-7>") '(lambda nil (interactive) (scroll-left 15)))

; Bindings for text scale adjustment via scrollwheel
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-double-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-triple-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-double-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-triple-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

; Set ECB mode to use LMB instead of MMB for selection
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

; Set binding for code navigation
(global-set-key (kbd "<C-f12>") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x <f12>") 'helm-projectile-ag)
(global-set-key (kbd "C-x C-S-f") 'helm-do-grep-ag)

; Bindings for code completion
(add-hook 'python-mode-hook (lambda ()
    (global-set-key (kbd "<C-tab>") 'company-complete)
))

; Override the default keybindings for buffer mark navigation
(setq back-button-smartrep-prefix              "C-c")
(setq back-button-global-keystrokes          '("C-c <C-SPC>"))
(setq back-button-global-backward-keystrokes '("C-c <C-left>"))
(setq back-button-global-forward-keystrokes  '("C-c <C-right>"))
(setq back-button-local-keystrokes           '("C-c <SPC>"))
(setq back-button-local-backward-keystrokes  '("C-c <left>"))
(setq back-button-local-forward-keystrokes   '("C-c <right>"))
(setq back-button-local-backward-keystrokes  '("C-c C-b"))
(setq back-button-local-forward-keystrokes   '("C-c C-f"))

; Account for both kinds of mice and OSes at work/home
(global-set-key (kbd "<mouse-8>") 'back-button-global-backward)
(global-set-key (kbd "<mouse-9>") 'back-button-global-forward)
(global-set-key (kbd "M-<mouse-8>") 'back-button-local-forward)
(global-set-key (kbd "M-<mouse-9>") 'back-button-local-forward)

; Blizzard RHEL 7 sends mouse4/5 for the mouse scroll wheel for some reason
(global-set-key (kbd "<mouse-4>") '(lambda nil (interactive) (scroll-down 5)))
(global-set-key (kbd "<mouse-5>") '(lambda nil (interactive) (scroll-up 5)))

; TODO: Keybindings for inserting doxygen documentation in C/C++/Java mode

; Additional keybinds for moving lines up/down on the home row
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-n") 'move-text-down)
