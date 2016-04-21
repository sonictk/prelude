; Install all user-required packages first
(prelude-require-packages '(auto-complete auto-complete-clang back-button jedi-core company-jedi company-irony-c-headers company-lua company-qml company-shell company-web company c-eldoc irony-eldoc helm-company web-completion-data csharp-mode dtrt-indent goto-last-change glsl-mode multiple-cursors omnisharp whitespace nlinum fill-column-indicator irony company-irony ecb epc helm-gtags pylint py-autopep8 project-explorer shader-mode yascroll))

;; This sets the default Emacs theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)
(setq prelude-theme 'monokai)

; Turn off the toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)

; Blinking cursor
(blink-cursor-mode 1)

; Disable ECB startup tips of the day
(setq ecb-tip-of-the-day nil)

; Set cursor display attributes
(setq-default cursor-type 'box)
(set-cursor-color "YellowGreen")

; Save the window layout on exit
(desktop-save-mode 1)
(setq desktop-save 'ask-if-new)

; Show matching parentheses
(show-smartparens-mode 1)

; Allow for navigating between buffers
(require 'back-button)
(back-button-mode 1)

; Enable multiple cursors
(require 'multiple-cursors)

; Show whitespace
(require 'whitespace)
(global-whitespace-mode t)
(setq show-trailing-whitespace t)
(setq whitespace-style (quote
   (face spaces tabs space-mark tab-mark)))
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [187 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    )
)

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

; Define custom functions
; Allow for going to specific line number
(defun goto-line-with-feedback ()   "Show line numbers temporarily, while prompting for the line number input"   (interactive)   (unwind-protect
      (progn
        (nlinum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (nlinum-mode -1)))

; Function to kill all other buffers apart from the current one
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
        (delq (current-buffer) 
            (remove-if-not 'buffer-file-name (buffer-list))
        )
    )
)

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

; C++ Autocomplete for clang
(require 'auto-complete-clang)
(require 'auto-complete)
(define-key c++-mode-map (kbd "C-S-SPC") 'ac-complete-clang)

; Use irony autocomplete for C languages
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(setq w32-pipe-read-delay 0)

; Use company-mode with irony
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

; Use tab-completion with no delay
(setq company-idle-delay 0)
(define-key c-mode-map [(control tab)] 'company-complete)
(define-key c++-mode-map [(control tab)] 'company-complete)

; Enable completion of C/C++ headers
; (require 'company-c-headers)
; (add-to-list 'company-backends 'company-c-headers)

; Set autocomplete header search paths based on OS type
; NOTE: Can find the include paths with the shell command ``echo "" | g++ -v -x c++ -E -``
;(cond
; ((string-equal system-type "windows-nt") ; any flavor of Windows
;    (setq ac-clang-flags
;          (mapcar (lambda (item)(concat "-I" item))
;                  (split-string
;    "
;    C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/include
;    C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/../../../../include
;    C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/include-fixed
;    C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/../../../../x86_64-w64-mingw32/include
;    C:/msys64/mingw64/lib/gcc/../../include/c++/5.3.0
;    C:/msys64/mingw64/lib/gcc/../../include/c++/5.3.0/x86_64-w64-mingw32
;    C:/msys64/mingw64/lib/gcc/../../include/c++/5.3.0/backward/usr/include
;    "
;    )))
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/include")
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/../../../../include")
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/include-fixed")
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/bin/../lib/gcc/x86_64-w64-mingw32/5.3.0/../../../../x86_64-w64-mingw32/include")
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/lib/gcc/../../include/c++/5.3.0")
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/lib/gcc/../../include/c++/5.3.0/x86_64-w64-mingw32")
;    (add-to-list 'company-c-headers-path-system "C:/msys64/mingw64/lib/gcc/../../include/c++/5.3.0/backward/usr/include")
;  )
 ;((string-equal system-type "gnu/linux")
 ; )
 ;((string-equal system-type "darwin") ; Mac
 ;)
;)

; Highlight doxygen comments
(defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

; Enable autocomplete mode only for C++, Python, and other scripting languages that would make sense
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'sql-mode)

; Enable generating Sphinx-compatible docstrings automatically for Python with C-c C-d
(add-hook 'python-mode-hook (
        lambda ()
        (sphinx-doc-mode t)
    )
)

; Enable automatic settings for compiling projects for various different languages
(add-hook 'c++-mode-hook
    (lambda ()
        (unless (file-exists-p "Makefile")
            (set (make-local-variable 'compile-command)
                (let* ((file (file-name-nondirectory buffer-    file-name))
                    (executable (convert-filename-to-executable file)))
                        (concat "g++ -g -Wall -o "
                            (file-name-sans-extension file)
                             " "
                             file
                             " && "
                             executable
                        )
                )
            )
        )
    )
)

(add-hook 'c-mode-hook
      (lambda ()
        (unless (file-exists-p "Makefile")
          (set (make-local-variable 'compile-command)
               (let* ((file (file-name-nondirectory buffer-file-name))
                      (executable (convert-filename-to-executable file)))
                 (concat "gcc -g -ansi -Wall -Wpedantic -Wextra -Wc++-compat -Wconversion -o "
                         (file-name-sans-extension file)
                         " "
                         file
                         " && "
                         executable))))))

(add-hook 'python-mode-hook
          (lambda ()
              (set (make-local-variable 'compile-command)
                   (concat "python " buffer-file-name))))

(add-hook 'perl-mode-hook
          (lambda ()
              (set (make-local-variable 'compile-command)
                   (concat "python " buffer-file-name))))

; Setup GDB debugger to display multi view for debugging by default
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

; Enable vertical rule for Python/C/C++ source files
(add-hook 'python-mode-hook (lambda ()
    (fci-mode t)
))

(add-hook 'c-mode-common-hook (lambda ()
    (fci-mode t)
))
(setq fci-rule-column 80)
(setq fci-rule-use-dashes t)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; scroll 3 lines at a time when using mwheel
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

; Disable guru-mode prompts and tips
(setq prelude-guru nil)
(setq guru-global-mode nil)

; Check if running on Macbook based off hostname and set the font size accordingly
(if (string-equal system-name "sonictk-mbp.local") 
    ;; Set custom font as default global font
    (add-to-list 'default-frame-alist '(font . "Literation Mono Powerline-12"))
    (set-face-attribute 'default nil :font "Literation Mono Powerline-12")
)

; Disable tip of the day
(setq ecb-tip-of-the-day nil)

; Global scrollbar mode
(global-yascroll-bar-mode 1)

; Highlight TODOs and other interesting code tags
(defun font-lock-comment-annotations ()
    "Highlight a bunch of well known comment annotations.
  This functions should be added to the hooks of major modes for programming."
    (font-lock-add-keywords
         nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|todo\\|optimize\\|hack\\|refactor\\):"
                          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

; Setup Jedi Python autocompletion
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(add-hook 'python-mode-hook 'jedi:ac-setup)

; Run Python inferior process automatically upon invoking Python mode to avoid eldoc errors
; TODO: This is causing eldoc to completely explode and hang
; (defun my-run-python ()
;     (run-python (python-shell-parse-command)))
; (add-hook 'python-mode-hook 'my-run-python)

; Set Python PDB debugger default command to use ipdb instead
(setq gud-pdb-command-name "python -m pdb")

; Disable subword mode globally by default
(global-subword-mode 0)

; Disable flycheck mode globally by default
(global-flycheck-mode -1)

; Restore session after ediff session
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

; Setup helm gtags for C++ code navigation
(setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode
(require 'helm-gtags)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

; Set tab width to 4 by default and use spaces by default
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)

; Automatically use tabs for C++ code
(add-hook 'c-mode-common-hook 
  (lambda()(dtrt-indent-mode t))
  (lambda()(indent-tabs-mode t))
)

; Automatically setup omnisharp when editing C# solution
; (add-hook 'csharp-mode-hook 'omnisharp-mode)

; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq python-shell-prompt-detect-failure-warning nil)

; Disable eldoc since it's causing hangs on Python code, only enable for C/C++
(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

; Set flag so that will not be prompted to kill running process on closing Emacs every single time
(add-hook 'comint-exec-hook 
      (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

; Allow for communication between emacs and Maya
(add-hook
 'mel-mode-hook
 (lambda ()
   (require 'etom)
   (setq etom-default-host "localhost")
   (setq etom-default-port 2222)
   (local-set-key (kbd "C-c C-r") 'etom-send-region)
   (local-set-key (kbd "C-c C-c") 'etom-send-buffer)
   (local-set-key (kbd "C-c C-l") 'etom-send-buffer)
   (local-set-key (kbd "C-c C-z") 'etom-show-buffer)))

; For Python
(add-hook
 'python-mode-hook
 (lambda ()
   (require 'etom)
   (setq etom-default-host "localhost")
   (setq etom-default-port 2222)
   (local-set-key (kbd "C-c C-r") 'etom-send-region-py)
   (local-set-key (kbd "C-c C-c") 'etom-send-buffer-py)
   (local-set-key (kbd "C-c C-l") 'etom-send-buffer-py)
   (local-set-key (kbd "C-c C-z") 'etom-show-buffer)))

; Add MEL mode syntax highlighting
(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)
