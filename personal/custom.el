(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(ac-candidate-limit 50)
 '(ac-trigger-key "C-S-SPC")
 '(ac-use-fuzzy t)
 '(ecb-options-version "2.40")
 '(global-auto-complete-mode t)
 '(pe/follow-current t)
 '(pe/omit-gitignore t)
 '(scroll-bar-mode nil)
 '(sp-base-key-bindings nil)
 '(sp-override-key-bindings
   (quote
    (("C-<right>")
     ("C-<left>")
     ("C-(" . sp-forward-slurp-sexp)
     ("C-)" . sp-forward-barf-sexp))))
 '(which-key-idle-delay 3.0)
 '(which-key-mode t)
 '(which-key-popup-type (quote minibuffer))
 '(yascroll:delay-to-hide nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((t (:foreground "gray30" :slant italic))))
 '(whitespace-tab ((t (:background "#272822" :foreground "gray30")))))
