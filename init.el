;; DOT EMACS - Thomas Luquet
;; ---------------------------------------------------
;; Vous trouverez ici mon .emacs.d/init.el
;; Il est un peu en bordel mais commenté, n'hésitez pas à me dire vos suggestions/ conseils


(setq user-full-name "Thomas Luquet")

;; PACKAGE Source
;; ---------------------------------------------------
;; Il y a peut etre trop de list-package dans ma list...

(require 'package)
(setq package-archives '(
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")
			 )
      )

(package-initialize)
(setq url-http-attempt-keepalives nil)


(require 'org)
(require 'ox)


;; Tricks divers
;; ---------------------------------------------------
;; Plein de trucs divers et nécessaire

;; Indentation d'une région
(global-set-key (kbd "C-x C-a") 'indent-region)

;; Pour avoir les parenthese coloré automatiquement
(show-paren-mode 1)

;; Pour pouvoir commenter une region
(global-set-key (kbd "C-C C-C") 'comment-dwim)

;; Pour avoir le numéro de la ligne à gauche
(global-linum-mode 1);; active le mode
(setq linum-format "%2d| ") ;; 2> cole à gauche puis | puis space


;; Pas de menubar en haut ni de scroll bar
(menu-bar-mode -1) ;; Enleve la bar du haut qui est useless
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; Enlever la scrollbar
  (scroll-bar-mode -1))

;; Permet de changer le répertoire de sauvegarde automatique (évite d'avoir des fichier qui trenne partout)
(require 'saveplace)
(setq-default save-place t)

;; Permet de changer le dossier de backup
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saved_places"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; Expend
(global-set-key (kbd "M-/") 'hippie-expand)

;; Buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)



;; COMPANY
;; -------------------------------------------------------
;; Nécessaire pour avoir un popup qui propose de la completion (pour le code et l'orthographe)

(require 'popup)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode) ;; La on dit que c'est pour tout


;; Don't enable company-mode in below major modes : pas dans le shell, ni erc ...
(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))

;; "text-mode" is a major mode for editing files of text in a human language"
;; most major modes for non-programmers inherit from text-mode
(defun text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  ;; company-ispell is the plugin to complete words
  (add-to-list 'company-backends 'company-ispell) )

(add-hook 'text-mode-hook 'text-mode-hook-setup)

(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))


;; Org Mode
;; ---------------------------------------------------
(require 'org)
(setq org-log-done t) ;; Sait pas à quoi ca sert
(setq org-startup-truncated nil) ;; Permet de faire de retours à la ligne


;; Permet de faire comme si une tache était +barré+ dans le terminal
;; Cela ne fonctionne malheureusement pas dans emacs -nw
(require 'cl)   ; for delete*
(setq org-emphasis-alist
      (cons '("+" '(:strike-through t :foreground "red"))
	    (delete* "+" org-emphasis-alist :key 'car :test 'equal)))

(setq org-todo-keywords
      '((sequence "TODO" "DOOING" "DONE")))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DREAM(d)" "SUN(s)" "|" "DONE(d)")
              (sequence  "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DREAM" :foreground "yellow" :weight bold)
	      ("SUN" :foreground "green" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))



;; Correcteur orthographique / dictinnaire
;; ---------------------------------------------------
(require 'flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell) ;; Ajoute automatiquement le flysper aux fichier org
(setq ispell-dictionary "french")

;; Alsa Volume manager
;; ----------------------------------
;; Permet d'afficher et de changer le volume du son du PC (très pratique)
(require 'volume)


;; Button Line
;; ----------------------------------
;; tas de conneries qui s'affiche dans la bar du bas


;; Batterie dans la buffer line
(display-battery-mode t) ;; Sert a afficher la batterie (utile pour les PC portable)

;; Nyan-mode : Permet de savoir ou tu es dans ta page (Assez utile finalement)
(require 'nyan-mode)
(nyan-mode t)

;; Affiche l'heure dans la barre du bas
;; Set le buffer du de la date et du temps
(setq display-time-24hr-format t display-time-day-and-date t display-time-interval 50 display-time-default-load-average nil display-time-mail-string "")
(display-time-mode t) ;; affiche le temps

;; Clean White Space
;; ----------------------------------
;; Montre les Whites space inutile en fin de ligne
;; TODO : show uniquement dans le code ET dans les .org
(setq-default show-trailing-whitespace t)
(setq-default show-leading-whitespace t) ;; espace en fin de ligne
(setq-default indicate-empty-lines t)

;; Efface automatiquement les espaces de fin de ligne a chaque save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org-export stysheet
;; ----------------------------------
;; Style pour l'export org-mode
;; TODO : le reset pour owncloud
;;(setq org-odt-styles-file "/home/zobi8225/Dropbox/style.ott")


;; THEME
;; ----------------------------------
(load-theme 'material t)

;; Pas écrire dans le prompt du mini buffer
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))



;; Golden resize des windows
;; -----------------------------------------------------------------
;; Permet d'ajuster la taille des windows, mais ca fou un peu la gerbde...
;; (require 'golden-ratio)
;; (golden-ratio-mode 1)

;; UTF8 Partout : Parce que c'est le turfu
;; -----------------------------------------------------------------
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Terminal (alt x -> ansi-term)
;; -----------------------------------------------------------------
;; Config pour avoir un terminal (ansi-term) qui soit bien
;; http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/

(setq multi-term-program "/bin/zsh") ;; Force l'utilisation de zsh dans le terme

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
	(split-window-sensibly (selected-window))
	(other-window 1)
	(ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
(global-set-key (kbd "C-c t") 'visit-term-buffer)
;; Permet d'éviter qu il montre les white space dans le term mais semble pas hyper bien marcher....

;; Permet de ne pas voir les white space dans le term
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
))


;; Open URL dans emacs
;; ------------------------------------------------------------
;; Use firefox to open urls
(setq browse-url-browser-function 'browse-url-firefox)


;; RAINBOW MODE
;; ------------------------------------------------------------
;; So Many Color ! #0000ff #ffffff #ff0000 (Et vive la france !)
(require 'rainbow-mode)
;; Pour le mettre en global :
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)


;; Helm
;; ------------------------------------------------------------
(require 'helm-config)
(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; Bizarie censer faire que le tab équivaut à un enter

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
;;(global-set-key (kbd "C-x m b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-s") 'helm-spotify)

;; Pour company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
;; Pour le ispell (Correcteur orthographique)
(require 'helm-ispell) ;; Pas sur de comprendre comment ca marche / a quoi ca sert

;; Pour flyspell -> Check si ca marche vraiment
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)



;; Rainbow délimiters
;; ------------------------------------------------------------
;; Met les parenthèses en "rainbows", très utile pour ne plus se perdre dans les parenthès

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit (default font-lock-function-name-face) :foreground "green" :underline nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "pink"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FFFF00"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "pink"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan")))))


;; Counter Strike :Global Offensive
;; ------------------------------------------------------------
;; Parce que emacs à un mode pour le fichier autoexe de counf de CS:GO
(require 'csgo-conf-mode)

;; Web-mode
;; ------------------------------------------------------------
;; Mode que je trouve mieux pour le JS que js2-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)


;; Markdown mode
;; ------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; [TEST] Elpy
;; ------------------------------------------------------------
;; Truc qui gère tout python, mais demande pas mal d'autres trucs à installer
;; Checker : https://github.com/jorgenschaefer/elpy
;;(package-initialize)
(elpy-enable)
(defalias 'workon 'pyvenv-workon)

(global-set-key (kbd "C-c C-s") 'elpy-rgrep-symbol)

;; [TEST] Floobits
;; ------------------------------------------------------------
;; Permet de taffer à plusieurs sur un projet
;; Apparement, il ne nécessite pas de d'appel dans le fichier de conf...

;; ox-reveal
;; ------------------------------------------------------------
;; Permet de faire de beau powerpoint à partir des .org
;; Attention, dans le .org, il faut checker le PATH du js
(require 'ox-reveal)


;;Move text
;; ------------------------------------------------------------
;; Permet de bouger des lignes et des regions de lignes avec Meta  (alt) + Maj + fleche haut ou bas
(move-text-default-bindings)


;; Multiple cursor
;; ------------------------------------------------------------
;; Emacs porn : http://emacsrocks.com/e13.html
;; Faire marcher le truc en emacs -nw

(require 'multiple-cursors)

;; (global-set-key (kbd "C-S-e") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-SPC") 'set-rectangular-region-anchor)

;; Neotree
;; ------------------------------------------------------------
;; Permet d'avoir une arboressance/menu des dossiers dans lesquel on est
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; [Test] Tramp
;; ------------------------------------------------------------
;; Permet de se connecter en ssh depuis emacs
(require 'tramp)
(setq tramp-default-method "ssh")
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)
(define-key global-map (kbd "C-c s") 'helm-tramp)

;; Changer de buffer ou de windows facilement <F6>
;; ------------------------------------------------------------
;; Pas hyper utile
(defun other-window-or-switch-buffer ()
  "Call `other-window' if more than one window is visible, switch
to next buffer otherwise."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer nil)
    (other-window 1)))
(global-set-key (kbd "<f6>") #'other-window-or-switch-buffer)

;; [Test] Projectile
;; ------------------------------------------------------------
;;
(projectile-mode)

;; [Test] Personal macro
;; ------------------------------------------------------------
;; TODO : Creat a function creator in python
;; Creat #Given , #When #Then for test in python
(fset 'givenwhenthen
   "# Given\C-m# When\C-m# Then")
(global-set-key (kbd "C-c g") 'givenwhenthen)

;; Semble ne pas marcher
(fset 'pythonUnitTest
   "@pytest.mark.unit_test\C-mdef test_(self):\C-m#Given\C-m# When\C-m# Then\C-massert 2 == 2\C-m pass")
(global-set-key (kbd "C-c u") 'givenwhenthen)


;; [TEST] Playerctl
;; ------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/me")
(require 'playerctl)
(define-key global-map (kbd "C-c C-SPC") 'playerctl-play-pause-song)
(define-key global-map (kbd "C-c C-n") 'playerctl-next-song)


;; Yasnippet
;; ------------------------------------------------------------
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;; [TEST] org-jira
;; ------------------------------------------------------------
(setq jiralib-url "https://sieaf-transverse.atlassian.net")

;; [END]
;; ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#262626"))
 '(custom-safe-themes
   (quote
    ("5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(fci-rule-color "#3a3a3a")
 '(hl-sexp-background-color "#121212")
 '(package-selected-packages
   (quote
    (playerctl org-jira package-lint ox-minutes helm-projectile projectile bonjourmadame helm-tramp lua-mode neotree pyenv-mode move-text camcorder which-key web-mode volume use-package rainbow-mode rainbow-delimiters ox-reveal ox-html5slide nyan-mode multiple-cursors material-theme markdown-preview-mode markdown-preview-eww magit json-mode js2-mode htmlize helm-ispell helm-gitignore helm-flycheck helm-firefox golden-ratio flyspell-popup flyspell-correct-popup floobits flappymacs erc-colorize elpy dired-rainbow csgo-conf-mode coffee-mode babel)))
 '(pyvenv-mode t)
 '(pyvenv-tracking-ask-before-change nil)
 '(pyvenv-virtualenvwrapper-python "/usr/bin/python")
 '(pyvenv-workon t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
