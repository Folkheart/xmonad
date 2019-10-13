;;---------------SETTINGS----------------;;
;; essential for packages and use-package.
(load "~/init.el")
;; custom theme.
(load "~/theme.el")
;; default directory on Windows.
;; (setq-default default-directory "c:/TRABAJO/")

;;--------CONFLICTING-DEPENDENCIES-------;;
;for helm.
(use-package async
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  )

;;--------------MAJOR-MODES--------------;;
(use-package evil
  :config
  (evil-mode 1)
  )

(use-package helm
  :defer t
  )

(use-package projectile
  :defer t
  :config
  ;; (setq projectile-project-search-path '("C:/TRABAJO"))
  (setq projectile-project-search-path '("~/"))
  (projectile-mode 1)
  )

(use-package magit
  :defer t
  )

;;------------SECONDARY-MODES------------;;
(use-package evil-commentary
  :after (evil)
  :config
  (evil-commentary-mode)
  )

(use-package helm-swoop
  :defer t
  )

(use-package helm-projectile
  :commands (helm-projectile
  	     helm-projectile-find-file
  	     helm-projectile-recentf
  	     helm-projectile-switch-project
  	     helm-projectile-switch-to-buffer)
  )

(use-package which-key
  :config
  (which-key-mode 1)
  )

(use-package general
  :config
  (general-override-mode 1)
  (general-def 'override
    "M-x" 'counsel-M-x
    "C-s" 'helm-occur
    "M-s" 'helm-swoop
    "C-u" 'evil-scroll-up                      ;missing key on evil-mode
    "M-j" 'evil-window-next
    "M-k" 'evil-window-prev
    "M-RET" 'projectile-run-shell
    ;; "M-J" 'next-buffer
    ;; "M-K" 'previous-buffer
   )
  (general-create-definer my-leader
    :prefix "SPC"
    )

  (my-leader 'normal
	     ""   '(nil                                        :wk "EXIT")
	     "x"  '(helm-M-x                                   :wk "helm-M-x")

	     "RET"'(mode-line-other-buffer                     :wk "switch buffer")
	     ;; "m"  '(helm-bookmarks                             :wk "bookmarks")
	     "m"  '(counsel-bookmark                             :wk "bookmarks")
	     ;; "j"  '(next-buffer                                :wk "next buffer")
	     "j"  '(evil-next-buffer                                :wk "next buffer")
	     ;; "k"  '(previous-buffer                            :wk "previous buffer")
	     "k"  '(evil-prev-buffer                            :wk "previous buffer")

	     "it" '(emacs-init-time                            :wk "init time")

	     "d"  '(:ignore t                                  :wk "dired")
	     "dj" '(dired-jump                                 :wk "jump to file")
	     "do" '(dired-jump-other-window                    :wk "jump other window")

	     "D"  '(:ignore t                                  :wk "desktop")
	     "DS" '(desktop-save                               :wk "save desktop")
	     "DL" '(desktop-read                               :wk "load desktop")
	     ;; "Dk" '(eyebrowse-close-window-config              :wk "kill workspace")
	     ;; "Dc" '(eyebrowse-create-window-config             :wk "create workspace")

	     "e"  '(:ignore t                                  :wk "edit tools")
	     "ew" '(whitespace-mode                            :wk "whitespace mode")
	     "ec" '(whitespace-cleanup                         :wk "clean whitespace")

	     "f"  '(:ignore t                                  :wk "file")
	     ;; "ff" '(helm-find-files                            :wk "find file")
	     "ff" '(counsel-find-file                            :wk "find file")
	     "fr" '(helm-recentf                               :wk "recent files")

	     "w"  '(:ignore t                                  :wk "window managent")
	     "wh" '(split-window-horizontally                  :wk "split horizontally")
	     "wv" '(split-window-vertically                    :wk "split vertically")
	     "ww" '(other-window                               :wk "other window")
	     "wd" '(evil-window-delete                         :wk "delete window")
	     "wD" '(delete-other-windows                       :wk "delete other windows")

	     "p"  '(:ignore t                                  :wk "project")
	     ;; "pp" '(helm-projectile                            :wk "projectile")
	     "pp" '(counsel-projectile                            :wk "projectile")
	     "ps" '(helm-projectile-switch-project             :wk "switch project")
	     "pr" '(helm-projectile-recentf                    :wk "recent files")
	     "pb" '(helm-projectile-switch-to-buffer           :wk "buffers")
	     "pf" '(helm-projectile-find-file                  :wk "find files")
	     "pd" '(projectile-discover-projects-in-directory  :wk "discover in directory")

	     "b"  '(:ignore t                                  :wk "buffer")
	     "be" '(eval-buffer                                :wk "eval buffer")
	     ;; "bb" '(helm-buffers-list                          :wk "buffers list")
	     "bb" '(counsel-switch-buffer                          :wk "buffers list")
	     "bk" '(kill-this-buffer                           :wk "kill buffer")
	     "br" '(revert-buffer                              :wk "revert buffer")
	     "bs" '(save-buffer                                :wk "save buffer")

	     "g"  '(:ignore t                                  :wk "git")
	     "gs" '(magit-status                               :wk "status")
	     )
  )

(use-package nasm-mode
  :hook (asm-mode . nasm-mode)
  )

(use-package haskell-mode)


(use-package ivy
  :config
  (ivy-mode 1)
  )
(use-package swiper)
(use-package counsel)
(use-package counsel-projectile)
(use-package dired-narrow)
;; (use-package evil-snipe
;;   :config
;;   (evil-snipe-mode 1)                                ; 's' snipe search
;;   (evil-snipe-override-mode 1)                       ; activates extra features
;;   (setq evil-snipe-enable-highlight nil)             ; no highlight
;;   (setq evil-snipe-enable-incremental-highlight nil) ; no incremental highlight
;;   (setq evil-snipe-repeat-keys nil)                  ; disable 'f', 't' and 's' repeat search
;;   )

;; (defun emacs-terminal ()
;;       (interactive)
;;       (ansi-term "bash" "localhost")
;;       )

;; (use-package eyebrowse
;;   ;; :ensure t
;;   :config
;;   (eyebrowse-setup-opinionated-keys)
;;   ;; (general-def
;;   ;;   "M-1" 'eyebrowse-switch-to-window-config-1
;;   ;;   "M-2" 'eyebrowse-switch-to-window-config-2
;;   ;;   "M-3" 'eyebrowse-switch-to-window-config-3
;;   ;;   "M-4" 'eyebrowse-switch-to-window-config-4
;;   ;;   "M-5" 'eyebrowse-switch-to-window-config-5
;;   ;;   "M-6" 'eyebrowse-switch-to-window-config-6
;;   ;;   "M-7" 'eyebrowse-switch-to-window-config-7
;;   ;;   "M-8" 'eyebrowse-switch-to-window-config-8
;;   ;;   "M-9" 'eyebrowse-switch-to-window-config-9
;;   ;;   "M-0" 'eyebrowse-switch-to-window-config-0
;;   ;;   )
;;   (eyebrowse-mode t)
;;   )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("7f74a3b9a1f5e3d31358b48b8f8a1154aab2534fae82c9e918fb389fca776788" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "d6f04b6c269500d8a38f3fabadc1caa3c8fdf46e7e63ee15605af75a09d5441e" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "2878517f049b28342d7a360fd3f4b227086c4be8f8409f32e0f234d129cee925" "0d087b2853473609d9efd2e9fbeac088e89f36718c4a4c89c568dd1b628eae41" "001c2ff8afde9c3e707a2eb3e810a0a36fb2b466e96377ac95968e7f8930a7c5" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "c95043bcca81b664f7b394e88f888065aa80ba48b4f3a02ede30590399035a49" "332e009a832c4d18d92b3a9440671873187ca5b73c2a42fbd4fc67ecf0379b8c" "70ed3a0f434c63206a23012d9cdfbe6c6d4bb4685ad64154f37f3c15c10f3b90" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "f951343d4bbe5a90dba0f058de8317ca58a6822faa65d8463b0e751a07ec887c" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "b462d00de785490a0b6861807a360f5c1e05b48a159a99786145de7e3cce3afe" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "f589e634c9ff738341823a5a58fc200341b440611aaa8e0189df85b44533692b" "0713580a6845e8075113a70275b3421333cfe7079e48228c52300606fa5ce73b" "f30aded97e67a487d30f38a1ac48eddb49fdb06ac01ebeaff39439997cbdd869" "a2286409934b11f2f3b7d89b1eaebb965fd63bc1e0be1c159c02e396afb893c8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(evil-snipe-mode t)
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(midnight-mode t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(objed-cursor-color "#ff665c")
 '(package-selected-packages
   (quote
    (dired-narrow counsel-projectile all-the-icons-ivy counsel swiper ivy solarized-theme theme-solarized emacs-color-theme-solarized evil-snipe tabbar TabBarMode haskell-mode helm-swoop general which-key helm-projectile evil-commentary magit projectile helm evil async dracula-theme use-package)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
