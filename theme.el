;;---------------SETTINGS----------------;;
;; enable/disable bars.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; set space between lines.
;; (setq-default line-spacing 3)
;; set font.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono Book 8"))
;; emacs transparency.
;; (set-frame-parameter (selected-frame) 'alpha '(75 100))
;; (add-to-list 'default-frame-alist '(alpha 05 100))

;;----------------THEMING----------------;;
(use-package dracula-theme
  :config
  ;; (load-theme 'dracula t)
  )

(use-package doom-themes
  :config
  (load-theme 'doom-acario-light t)
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  ;; then run 'M-x all-the-icons-install-fonts'
  )

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup)
  )

(linum-mode +1)


;;------------CURSOR-BEHAVIOR------------;;
;; highlight line for vim modes.
;; (global-hl-line-mode 1)
;; (add-hook 'evil-emacs-state-entry-hook
;; 	(lambda() (set-face-background 'hl-line "#553")))
;; (add-hook 'evil-normal-state-entry-hook
;; 	(lambda() (set-face-background 'hl-line "#241")))
;; (add-hook 'evil-insert-state-entry-hook
 ;; 	(lambda() (set-face-background 'hl-line "#115")))
;; (add-hook 'evil-operator-state-entry-hook
;; 	(lambda() (set-face-background 'hl-line "#520")))
;; (add-hook 'evil-visual-state-entry-hook
;; 	(lambda() (set-face-background 'hl-line nil)))

;; (set-face-background 'default nil)
