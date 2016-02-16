; Install all user-required packages first
(prelude-require-packages '(monokai-theme multiple-cursors minimap whitespace nlinum auto-complete-clang ecb))

;; This sets the default Emacs theme
(require 'monokai-theme)
(setq prelude-theme 'monokai)

; Turn off the toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)

; Blinking cursor
(blink-cursor-mode 1)

; Set cursor display attributes
(setq-default cursor-type 'bar)

; Save the window layout on exit
(desktop-save-mode 1)
(setq desktop-save 'ask-if-new)

; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Show matching parentheses
(show-smartparens-mode 1)

; Enable multiple cursors
(require 'multiple-cursors)

; Show whitespace
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style (quote
   (face spaces tabs space-mark tab-mark)))
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

; Disables the notification sound when scrolling past EOF, among other things
(setq visible-bell 1)

; Auto reload-buffers when files are changed on disk
(global-auto-revert-mode t)

; Enable Emacs Development Environment mode
(global-ede-mode t)

; Set permanent display of line numbers
; TODO: This is causing bugs all over the GUI, figure out a better solution
; (global-nlinum-mode t)
; For now only display the line numbers when goto line is activated
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()   "Show line numbers temporarily, while prompting for the line number input"   (interactive)   (unwind-protect
      (progn
        (nlinum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (nlinum-mode -1)))

; Disable auto-saving buffers
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)
(setq prelude-auto-save nil)

; Scroll just one line when hitting bottom of window
(setq scroll-conservatively 10000)

; Disable linum-mode for speedbar
(add-hook 'speedbar-mode-hook (lambda () (nlinum-mode -1)))

; Disable word wrapping by default
(set-default 'truncate-lines t)

; Disable prelude auto cleaning up whitespace on file save
(setq prelude-clean-whitespace-on-save nil)

; Set UI colors
(set-face-background 'hl-line "#1a3a3a")

; Set display margins
(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 4)
  (setq right-margin-width 4)
)

(add-hook 'text-mode-hook 'my-set-margins)

; Set tab width to 4
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)

; C++ Autocomplete for clang
(require 'auto-complete-clang)
(require 'auto-complete)
(define-key c++-mode-map (kbd "C-S-SPC") 'ac-complete-clang)

; Enable autocomplete mode only for C++, Python, and other scripting languages that would make sense
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'sql-mode)

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
