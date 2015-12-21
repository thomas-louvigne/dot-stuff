;; Dot emacs de ouam - zobi8225




;; PACKAGE Source
;; ---------------------------------------------------
;; Il y a peut etre trop de list-package dans ma list...

(require 'package)
(setq package-archives '(
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")
			 )
      )

(package-initialize)
(setq url-http-attempt-keepalives nil)


;; COMPANY
;; -------------------------------------------------------
;; Nécessaire pour avoir un popup qui propose de la completion

(require 'popup)
(require 'company)
;; La on dit que c'est pour tout
(add-hook 'after-init-hook 'global-company-mode)

;; Company pour emoji :-)
(require 'company-emoji)
(add-to-list 'company-backends 'company-emoji)

;; Company pour le HTML
;; Jamais testé
(require 'company-web)

;; COMPANY for Python
;; On utilise JEDI pour la completion : Attention, il demande une installation un peu bizzare quand on le lence
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)


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

  ;; => Désactivé
  ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
  ;;  but I prefer hard code the dictionary path. That's more portable.
  ;; (setq company-ispell-dictionary (file-truename "~/.emacs.d/misc/english-words.txt")))

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
;; Test de config de Org : ne sais pas a quoi ca sert
(require 'org)
;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)



;; Correcteur orthographique / dictinnaire
;; ---------------------------------------------------
(require 'flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell) ;; Ajoute automatiquement le flysper aux fichier org
(setq ispell-dictionary "french")
;; ---------------------------------------------------



;; Tricks
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
(display-battery-mode t) ;; Sert a afficher la batterie (utile pour les PC portable)

;; Nyan-mode : Permet de savoir ou tu es dans ta page (Assez utile finalement)
(nyan-mode t)

;; Affiche l'heure dans la barre du bas
;; Set le buffer du de la date et du temps
(setq display-time-24hr-format t display-time-day-and-date t display-time-interval 50 display-time-default-load-average nil display-time-mail-string "")
(display-time-mode t) ;; affiche le temps

;; Montre les Whites space inutile en fin de ligne
;; TODO : show uniquement dans le code ET dans les .org
(setq-default show-trailing-whitespace t)
;; (setq-default show-leading-whitespace t) ;; espace en fin de ligne
(setq-default indicate-empty-lines t)

;; Efface automatiquement les espaces de fin de ligne a chaque save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Style pour l'export org-mode
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
 '(custom-enabled-themes (quote (calmer-forest)))
 '(custom-safe-themes
   (quote
    ("4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "91faf348ce7c8aa9ec8e2b3885394263da98ace3defb23f07e0ba0a76d427d46" default)))
 '(delete-trailing-lines t)
 '(inhibit-startup-screen t)
 '(send-mail-function (quote smtpmail-send-it)))

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

;; Permet de faire comme si une tache était +barré+ dans le terminal
(require 'cl)   ; for delete*
(setq org-emphasis-alist
      (cons '("+" '(:strike-through t :foreground "red"))
	    (delete* "+" org-emphasis-alist :key 'car :test 'equal)))


;; EMOJI -> Ne marche pas dans le emacs -nw , mais marche bien sinon :smile:
(add-hook 'after-init-hook #'global-emojify-mode)

;; EMAIL : MU4E
;;-------------------------------------------------------
(require 'mu4e)

;; Default
(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "zobi8225@gmail.com"
 user-full-name  "Thomas Luquet"
 message-signature
 (concat
  "-- \n"
  "Thomas Luquet\n"
  "thomas@luquet.net\n")
 )


;; Permet (théoriquement) d'envoyer des mails avec org-mode
(require 'org-mu4e)


;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.


(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; View
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)


;; NOTIFICATION
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display) ;; Dans la ligne

;; Mail dir extention
(require 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)

;; Open URL dans emacs
;; ------------------------------------------------------------
;; Use firefox to open urls
(setq browse-url-browser-function 'browse-url-firefox)
;; Use qutebrowser to open urls
;; (setq browse-url-generic-program "qupzilla")

(global-set-key (kbd "C-c C-o") 'link-hint-open-link-at-point)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Python : ELPY
;; ------------------------------------------------------------
;; Permet d'afficher la norme dans python
(package-initialize)
(elpy-enable)

;; SMEX -- TEST ---
;; ------------------------------------------------------------
;; A pas compris l'utilité
;; Est sensé retenir les fonctions les plus utilisé dans emacs
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.



;; LANG TOOLS
;; ------------------------------------------------------------
;; Permet une correction de la grammaire francaise
(require 'langtool)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
(setq langtool-mother-tongue "fr")

;; Censé faire un popup (Mais à pas l'aire de marcher...)
(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))



;; IDO VERTICAL MODE
;; ------------------------------------------------------------
;; Permet de proposer les trucs d'un buffer dans le sens vertical
;; Exemple : C-x C-f
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; FIREPLACE
;; ------------------------------------------------------------
;; Marche pas dans ma config, ce qui est très triste.
(require 'fireplace)


;; RAINBOW MODE
;; ------------------------------------------------------------
;; So Many Color ! #0000ff #ffffff #ff0000 (Et vive la france !)
(require 'rainbow-mode)
(rainbow-mode 1)


;; Google Contacts
;; ------------------------------------------------------------
;; Gère les contacts google
;; (require 'google-contacts)
