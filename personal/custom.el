(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(minimap-always-recenter t)
 '(minimap-hide-fringes t)
 '(minimap-minimum-width 10)
 '(minimap-mode t)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(scroll-bar-mode nil)
 '(sp-base-key-bindings nil)

 ; Bindings for word navigation to override what smartparens.el sets by default
 '(sp-override-key-bindings 
    '(("C-<right>")
    ("C-<left>")
    ("C-(" . sp-forward-slurp-sexp)
    ("C-)" . sp-forward-barf-sexp))
 )
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-font-face ((t (:height 10 :family "Literation Mono for Powerline"))))
 '(whitespace-tab ((((class color) (min-colors 257)) (:background unspecified :foreground "gray30" :inverse-video unspecified :weight bold)) (((class color) (min-colors 89)) (:background unspecified :foreground "gray30" :inverse-video unspecified :weight bold)))))
