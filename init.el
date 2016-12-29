;; Dot emacs de ouam - zobi8225


;; PACKAGE Source
;; ---------------------------------------------------
;; Il y a peut etre trop de list-package dans ma list...

(require 'package)
(setq package-archives '(
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 )
      )

(package-initialize)
(setq url-http-attempt-keepalives nil)


(require 'org)
(require 'ox)


;; COMPANY
;; -------------------------------------------------------
;; Nécessaire pour avoir un popup qui propose de la completion

(require 'popup)
(require 'company)
;; La on dit que c'est pour tout
(add-hook 'after-init-hook 'global-company-mode)


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


;; ---------------------------------------------------



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




;; Correcteur orthographique / dictinnaire
;; ---------------------------------------------------
(require 'flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell) ;; Ajoute automatiquement le flysper aux fichier org
(setq ispell-dictionary "french")
;; ---------------------------------------------------



;; Tricks divers
;; ---------------------------------------------------
;; Indentation d'une région
(global-set-key (kbd "C-x C-a") 'indent-region)

;; Pour avoir les parenthese coloré automatiquement
(show-paren-mode 1)

;; Pour pouvoir commenter une region
(global-set-key (kbd "C-C C-C") 'comment-dwim)

;; Pour avoir le numéro de la ligne à gauche
(global-linum-mode 1);; active le mode
(setq linum-format "%2d| ") ;; 2> cole à gauche puis | puis space


;; Better default
(menu-bar-mode -1) ;; Enleve la bar du haut qui est useless
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; Enlever la scrollbar
  (scroll-bar-mode -1))

;; Affiche le volume
(require 'volume)


;; Permet de changer le répertoire de sauvegarde automatique
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

;; Batterie dans la buffer line
;;(display-battery-mode t) ;; Sert a afficher la batterie (utile pour les PC portable)

;; Nyan-mode : Permet de savoir ou tu es dans ta page (Assez utile finalement)
(require 'nyan-mode)
(nyan-mode t)

;; Affiche l'heure dans la barre du bas
;; Set le buffer du de la date et du temps
(setq display-time-24hr-format t display-time-day-and-date t display-time-interval 50 display-time-default-load-average nil display-time-mail-string "")
(display-time-mode t) ;; affiche le temps

;; Montre les Whites space inutile en fin de ligne
;; TODO : show uniquement dans le code ET dans les .org
;; CORRECT FOR FIRE PLACE
(setq-default show-trailing-whitespace t)
;; (setq-default show-leading-whitespace t) ;; espace en fin de ligne
;; (setq-default indicate-empty-lines t)

;; Efface automatiquement les espaces de fin de ligne a chaque save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Style pour l'export org-mode
;; A l'aire de plus marcher
(setq org-odt-styles-file "/home/zobi8225/Dropbox/style.ott")


;; THEME
;; ----------------------------------
;; Enlève le welcome popup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("7997e0765add4bfcdecb5ac3ee7f64bbb03018fb1ac5597c64ccca8c88b1262f" "4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "91faf348ce7c8aa9ec8e2b3885394263da98ace3defb23f07e0ba0a76d427d46" default)))
 '(delete-trailing-lines t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (js2-mode json-mode web-mode csgo-conf-mode dired-rainbow rainbow-delimiters volume use-package rainbow-mode nyan-mode helm-ispell helm-gitignore helm-flycheck helm-firefox flyspell-popup flyspell-correct-popup company))))

;; Pas écrire dans le prompt du mini buffer
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Pour Googlé facilement une connerie
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Search Google: "))))))
(global-set-key (kbd "C-x g") 'google)



;;; Youtube : (useless)
(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))
(global-set-key (kbd "C-x y") 'youtube)

;; Augmente ou diminue rapidement la taille des fenetre (C-X 2/3)
;; Ne marche pas dans emacs -nw :-(
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)


;; UTF8 Partout : Parce que c'est le turfu
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Terminal at your fingerprint
;; -----------------------------------------------------------------
;; Config pour avoir un terminal (ansi-term) qui soit bien
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


;; Open URL dans emacs
;; ------------------------------------------------------------
;; Use firefox to open urls
(setq browse-url-browser-function 'browse-url-firefox)
;; Use qutebrowser to open urls
;; (setq browse-url-generic-program "qupzilla")


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
 ;; On peut aussi mettre des tailles
 ;;'(rainbow-delimiters-depth-1-face ((t (:foreground "red" :height 2.0))))
 ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "orange" :height 1.8))))

 '(rainbow-delimiters-depth-1-face ((t (:foreground "red" ))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green" ))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue" ))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "black" ))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan"))))

 )


;; Counter Strike :Global Offensive
;; ------------------------------------------------------------
;; Parce que emacs à un mode pour le fichier autoexe de counf de CS:GO
(require 'csgo-conf-mode)

;; web-mode
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
(add-hook 'web-mode-hook  'my-web-mode-hook)



;; INSTALLÉ AUTOMATIQUEMENT quand on choisie son thème
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
