;;; dot-emacs --- Thomas Luquet
;;; Commentary:
;; Vous trouverez ici mon .emacs.d/init.el
;; Il est un peu en bordel mais assé commenté
;; N'hésitez pas à me dire vos suggestions/ conseils
;;
;;
;;; Code:
(setq user-full-name "Thomas Luquet")

;; PACKAGE Source
;; ---------------------------------------------------

(require 'package)
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 )
      )


(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


(package-initialize)
(setq url-http-attempt-keepalives nil)
(setq use-package-always-ensure t)


;; Tricks divers
;; ---------------------------------------------------
;; Plein de trucs divers et nécessaire

;; Indentation d'une région
(global-set-key (kbd "C-x C-a") 'indent-region)

;; Pour avoir les parentheses qui s'allument des deux cotés
(show-paren-mode 1)

;; Pour avoir le numéro de la ligne à gauche
;; (global-linum-mode 1);; active le mode
;; (setq linum-format "%2d| ") ;; 2> cole à gauche puis | puis space

;; Pas de menubar en haut ni de scroll bar
(menu-bar-mode 0) ;; Enleve la bar du haut qui est useless
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; Enlever la scrollbar
  (scroll-bar-mode -1))

;; Permet de changer le répertoire de sauvegarde automatique
;; (évite nottament d'avoir des fichiers ~ qui trennent partout)
;; Mais ca marche pas :/
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq backup-by-copying t)
  (setq backup-directory-alist '(("." . "~/.emacs.d/saved_places"))) ;; Mais c'est pas certain que ca merche
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  )

(setq backup-directory-alist `(("." . "~/.emacs.d/saved_places")))


;; Commande de Completion
(global-set-key (kbd "M-/") 'hippie-expand)


;; Search dans le fichier - Remplacé par swiper-helm
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)



;;;; Navigate in file
;;--------------------
;; changed beacause ELPY bind M-> & M-<
(global-set-key (kbd "<S-up>") 'beginning-of-buffer)
(global-set-key (kbd "<S-down>") 'end-of-buffer)
(global-set-key (kbd "C-d") 'kill-whole-line) ;; fait un couper de toute la ligne, remplace du kill-char totalement uselss

;; Mouse tricks
;;--------------------
;; Permet de faire des copiers/coller dans et en dehors d'emacs
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'x-clipboard-yank)
(setq x-select-enable-clipboard t)


;; Move text
;; ------------------------------------------------------------
;; Permet de bouger des lignes sélectionné (regions) avec  M-S ^ /
;; Alt + Maj + fleche haut ou bas
(move-text-default-bindings)

;; Multiple cursor
;; ------------------------------------------------------------
;; Emacs porn : http://emacsrocks.com/e13.html
(use-package multiple-cursors
  :bind
  (:map global-map
        ("M-SPC" . set-rectangular-region-anchor)
        ([f9] . mc/mark-previous-word-like-this)
        ([f10] . mc/mark-next-word-like-this)
        ([f11] . mc/mark-all-word-like-this)
        ))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click) ;; marche pas dans emacs -nw termin

;; Exand region
;; ------------------------------------------------------------
(use-package expand-region
  :bind (
         ("M-w" . er/expand-region)
         )
  )


;; Navigate in fileS
;;-------------------

;; Changer de buffer facilement <F5> / <f6 , <f7>>
;; ------------------------------------------------------------
(defun other-window-or-switch-buffer ()
  "Call `other-window' if more than one window is visible, switch to next buffer otherwise."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer nil)
    (other-window 1)))

(global-set-key (kbd "<f5>") #'split-window-right)
(global-set-key (kbd "<f6>") #'other-window-or-switch-buffer)
(global-set-key (kbd "<f7>") #'delete-window)

;; [TEST] Désactive l'indentation avec des tabs
;; -------------------------------------------------------
(setq-default indent-tabs-mode nil)

;; COMPANY
;; -------------------------------------------------------
;; Peremet de suggérer des réponses lorsqu'on écrit des trucs

;; Vielle config :
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode) ;; Là on dit que c'est pour tout
;; ;; Don't enable company-mode in below major modes : pas dans le shell, ni erc ...
;; (setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode python-mode))

;;;; company
;; Modular text completion framework
(use-package company
  :init
  (setq company-idle-delay nil)
  (setq company-tooltip-limit 20)
  (setq company-minimum-prefix-length 2)

  :config
  (global-company-mode 1)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (add-to-list 'company-begin-commands 'outshine-self-insert-command)
  (setq company-backends (remove 'company-ropemacs company-backends))


  (defun my-company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))

;;  Usage based completion sorting
  (use-package company-statistics
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my-company-elisp-setup)
    :config (company-statistics-mode)))


;;;; company-quickhelp
;; Popup documentation for completion candidates
(use-package company-quickhelp
  :init
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1)
  :config (company-quickhelp-mode 1))

;;;; company-web
;; Company version of ac-html, complete for web,html,emmet,jade,slim modes
(use-package company-web
  :config
  (defun my-company-web ()
    (set (make-local-variable 'company-backends) '(company-web-html))
    (company-mode t))
  :hook (web-mode . my-company-web))



;; Org Mode
;; ---------------------------------------------------
(use-package org
  :custom
  (org-log-done t)
  (org-startup-truncated nil) ;; Permet de faire de retours à la ligne
  (org-export-with-sub-superscripts nil) ;; Évites les erreures d'export quand on export les caractères __
  (org-todo-keywords
       '((sequence "TODO" "DOING" "DONE")))

  )

;; Correcteur orthographique / dictinnaire
;; ---------------------------------------------------
;; avec flyspell , On-the-fly spell checker
(use-package flyspell
  :diminish
  :init (setq ispell-dictionary "french")
  :hook (org-mode . turn-on-flyspell)
  :bind
  (
        ("M-3" . ispell-word)
        )
  )


;; Devrait passer à Grammacollect

;; [TEST] Test grammalecte
;; ------------------------------------------------------------
;; Y a encore plein de trucs a travailler
;; (load-file "/home/tlu/.emacs.d/me/flycheck-grammalecte/flycheck-grammalecte.el")


;; Bottom Line
;; ----------------------------------
;; tas de conneries qui s'affiche dans la bar du bas

;; Batterie dans la buffer line
;;(display-battery-mode t) ;; Sert a afficher la batterie (utile pour les PC portable)

;; Permet de voir les lignes et les columns du curseur
(column-number-mode t)
(line-number-mode t) ;; Affiche ta ligne et ta colonne

;; Nyan-mode : Permet de savoir ou tu es dans ta page (Assez utile finalement)
(use-package nyan-mode)
(nyan-mode t)

;; Affiche l'heure dans la barre du bas
;; Set le buffer du de la date et du temps
(display-time-mode t) ;; affiche le temps

;; Permet Pas écrire dans le prompt du mini buffer
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))



;; Clean White Space (Trailing whitespace)
;; ----------------------------------
;; Montre les Whites space inutile en fin de ligne
;; TODO : show uniquement dans le code ET dans les .org
(setq-default show-trailing-whitespace t)
(setq-default show-leading-whitespace t) ;; espace en fin de ligne
(setq-default indicate-empty-lines t)

;; Efface automatiquement les espaces de fin de ligne à chaque sauve
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org-export stysheet
;; ----------------------------------
;; Style pour l'export org-mode
;; TODO : le reset pour owncloud
;;(setq org-odt-styles-file "/home/zobi8225/Dropbox/style.ott")


;; THEME
;; ----------------------------------
(load-theme 'zenburn t)

;; configuration Font et Police 10
;; -----------------------------------------------------------------
(set-frame-font "DejaVu Sans Mono-12")

;; UTF8 Partout : Parce que c'est le turfu
;; -----------------------------------------------------------------
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Permet de ne pas voir les white space dans le term
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            ))


;; ;; Treemacs
;; ;; ------------------------------------------------------------
;; ;; permet d'avoir un menu en arbre
;; ;; TODO : avoir un truc qui met a jour des que je change de buffer
;; ;; Une ouverture automatique au launch de emacs
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (setq treemacs-change-root-without-asking nil
;;           treemacs-collapse-dirs              (if (executable-find "python") 3 0)
;;           treemacs-file-event-delay           5000
;;           treemacs-follow-after-init          t
;;           treemacs-goto-tag-strategy          'refetch-index
;;           treemacs-indentation                2
;;           treemacs-indentation-string         " "
;;           treemacs-is-never-other-window      nil
;;           treemacs-never-persist              nil
;;           treemacs-no-png-images              nil
;;           treemacs-recenter-after-file-follow nil
;;           treemacs-recenter-after-tag-follow  nil
;;           treemacs-show-hidden-files          t
;;           treemacs-silent-filewatch           nil
;;           treemacs-silent-refresh             nil
;;           treemacs-sorting                    'alphabetic-desc
;;           treemacs-tag-follow-cleanup         t
;;           treemacs-tag-follow-delay           1
;;           treemacs-recenter-after-file-follow t
;;           treemacs-width                      45)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null (executable-find "python3"))))
;;       (`(t . t)
;;        (treemacs-git-mode 'extended))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ([f8]         . treemacs-toggle)
;;         ("M-0"        . treemacs-select-window)
;;         ("C-c 1"      . treemacs-delete-other-windows)
;;         )
;;   )
;; (use-package treemacs-projectile
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq treemacs-header-function #'treemacs-projectile-create-header))

;; Open URL dans emacs
;; ------------------------------------------------------------
;; Use firefox to open urls
(setq browse-url-browser-function 'browse-url-firefox)


;; Rainbow délimiters
;; ------------------------------------------------------------
;; Met les parenthèses en "rainbows", très utile pour ne plus se perdre dans les parenthès

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "pink"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#FFFF00"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "pink"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "cyan")))))


;; Counter Strike : Global Offensive
;; ------------------------------------------------------------
;; Parce que emacs à un mode pour le fichier autoexe de counf de CS:GO
(use-package csgo-conf-mode)

;; Web-mode
;; ------------------------------------------------------------
;; Pour tout ce qui est fichier HTML, (Attention, pas JS !)
(use-package web-mode)
;; use web-mode for html
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; turn on flychecking globally
;; d-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking

;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))

;; use eslint with web-mode for jsx files
;;(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode,  adjust indentation."
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 1)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)



;; [TEST] JS2-mode
;; ------------------------------------------------------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
;; Vérifier ce que cela fait
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


(add-hook 'js2-mode-hook (lambda ()
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda () (electric-indent-local-mode -1))) ;; ?

;; Hook pour passer le linter a chaque save
(eval-after-load 'js2-mode
	   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;; [TEST] refactor JS
;; ------------------------------------------------------------
;; permet une suite de binding pour refacor du code js
(js2r-add-keybindings-with-prefix "C-c C-m")

;; ESLINT
;; ------------------------------------------------------------
;; Linter configurable

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Le package n'étant pas terrible j'ai pris ca ici :
;; https://github.com/rustyconover/eslint-fix/blob/483709dbad2100160df770a87c35e00248bb8f68/eslint-fix.el
(defun eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (progn (call-process eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
           (revert-buffer t t t))
    ))

;; Markdown mode
;; ------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ox-reveal
;; ------------------------------------------------------------
;; Permet de faire de beau powerpoint à partir de .org et de la lib reavea.js
;; Attention, dans l'entete du .org, il faut checker le PATH du js
(use-package ox-reveal)


;; HELM
;; ------------------------------------------------------------
;; Permet d'améliorer le M-x et pas mal d'autre choses

(use-package helm
  :commands helm-mini

  :init
  (setq helm-idle-delay 0.1)
  (setq helm-input-idle-delay 0.1)
  (setq helm-M-x-always-save-history t)
  (setq helm-buffer-details-flag nil)
  (setq helm-mode-handle-completion-in-region nil) ;don't use helm for `completion-at-point'
  ;;(setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (setq helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm") (rx "*scratch")  (rx "*log") (rx "*Ibuffer") (rx "*vc") (rx "*Help")  (rx "*Minibuf") (rx "*Messages*") (rx "*Annotate")  (rx "*temp")  (rx "*server") ))
  (setq helm-autoresize-mode t)

  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . #'helm-find-files)
  ("C-s" . #'swiper)
;;  ("C-x C-b" . helm-mini)

  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory))
  (add-to-list 'helm-boring-buffer-regexp-list ":.*")

  ;; permet des fake "go to definition" nottament pour le mode JS

  (require 'helm-xref)
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs)

  ;; cette option à été enlevé : (setq xref-show-xrefs-function helm-xref-show-xrefs)

  (use-package helm-ag) ;; Permet de faire des recherches dans le code. Couplé avec projectile, c'est juste Excelent !

  ;; Yet Another `describe-bindings' with `helm'.
  (use-package helm-descbinds
    :config (helm-descbinds-mode))

  ;; Helm integration for Projectile
  (use-package helm-projectile)

  ;; Helm UI wrapper for system package managers.
  (use-package helm-system-packages)
)

;; Projectile
;; ------------------------------------------------------------
;; Permet de naviguer au sein d'un projet
(projectile-mode)
(helm-projectile-on)

(use-package projectile
  :init (progn
          (projectile-mode)
          (setq projectile-enable-caching t)
          (setq projectile-ignored-directories  '("node" "_output" "node_module" "pkg" "bin"))
          (setq projectile-ignored-files '(".DS_Store" ".gitmodules" ".gitignore") )
          )
  :bind (
         ("<f1>" . helm-projectile-switch-project) ;; Change le projet de travail
	 ("<f2>" . helm-projectile)  ;; Cherche un fichier
         ("<f3>" . helm-projectile-ag) ;; Sorte de grep
         )
:ensure t)


;; Playerctl
;; ------------------------------------------------------------
;; Permet de changer de musique, notament spotify, depuis emacs
;; Nécessiste playerctl
;; Développé par moi :-)

(use-package playerctl
  :bind(
        ("C-c C-SPC" . playerctl-play-pause-song)
        ("C-c C-n" . playerctl-next-song)
        ("C-c C-p" . playerctl-previous-song)
        )
)

;; Git Gutter
;; ------------------------------------------------------------
;; Permet de montrer ce qui a changé dans git dans le linum (la bar de gauche)
(use-package git-gutter
    :bind(
        ("C-n" . git-gutter:next-diff)
        ("C-p" . git-gutter:previous-diff)
        )
    )
(global-git-gutter-mode t)

;; Flycheck
;; ------------------------------------------------------------
(use-package flycheck
  :diminish
  :hook (after-init-hook . #'global-flycheck-mode))


;; Pomodori
;; ------------------------------------------------------------
(global-set-key (kbd "<f12>") #'pomidor)
(setq alert-default-style 'libnotify)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

;; [TEST] Elpy
;; ------------------------------------------------------------
;; Toujours pas satisfait...
(use-package elpy)
(package-initialize)
(elpy-enable)

(setq elpy-rpc-backend "jedi")
(pyvenv-activate "/home/tlu/working/sief/sief-back/venv/")

;; [TEST] terminal interpreter
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--simple-prompt --pprint")


;; Compilation (et test avec elpy) dans une frame en bas
;; ------------------------------------------------------------

(defun down-compilation-frame-hook ()
  "Permet d'avoir la fenetre de compilation ou de test vers le bas de l'écran."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'down-compilation-frame-hook)

;; Pour avoir de la couleur dans le shell quand on compille
;; ------------------------------------------------------------
(define-derived-mode ansi-compilation-mode compilation-mode "ansi compilation"
  "Compilation mode that understands ansi colors."
  (require 'ansi-color)
  (toggle-read-only 0)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun colorize-compilation (one two)
  "ansi colorize the compilation buffer."
  (ansi-compilation-mode)
 )
(setq compilation-finish-function 'colorize-compilation)

;; [TEST] yapf
;; ------------------------------------------------------------
(add-hook 'python-mode-hook 'yapf-mode)


;; Diminish
;; ----------------------------------
;; Permet de retirer les mods de la botom line (qui prennent de la place pour rien)
;;(diminished-modes t)
(diminish 'anaconda-eldoc-mode)
(diminish 'edebug-mode)
(diminish 'flycheck-mode)
(diminish 'helm-mode)
(diminish 'rainbow-delimiters-mode)
(diminish 'python-mode)
(diminish 'eldoc-mode)
(diminish 'company-mode)
(diminish 'git-gutter)
(diminish 'python)
(diminish 'elpy-mode)
(diminish 'projectile-mode)
(diminish 'git-gutter-mode)
(diminish 'flymake-mode)
(diminish 'ispell-minor-mode)
(diminish 'server-buffer-clients)
(diminish 'js2-mode)
(diminish 'company-mode)
(diminish 'venv-mode)
(diminish 'yapf-mode)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(calendar-date-display-form
   (quote
    ((if dayname
         (concat dayname ", "))
     day "-" monthname "-" year)))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-window-height 20)
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" default)))
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(elpy-test-pytest-runner-command (quote ("pytest" "-p no:sugar -vvv
")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(fci-rule-color "#383838")
 '(font-use-system-font t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-ignore-buffer-patterns (quote ("\\.html\\'" "\\.htm\\'")))
 '(helm-ag-ignore-directory (quote ("html_cov" "node_module" ".tmp" "dist")))
 '(helm-ag-insert-at-point (quote symbol))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-export-with-sub-superscripts nil)
 '(org-log-done t)
 '(org-startup-truncated nil)
 '(org-todo-keywords (quote ((sequence "TODO" "DOING" "DONE"))))
 '(package-selected-packages
   (quote
    (dockerfile-mode swagger-to-org adoc-mode typescript-mode docker-compose-mode docker company-statistics flame swiper-helm yapfify playerctl angular-snippets expand-region company-quickhelp helm-system-packages helm-descbinds fancy-narrow helm-xref tern-context-coloring company-tern xref-js2 js2-refactor add-node-modules-path pyvenv python elpy exec-path-from-shell htmlize diminish helm diff-hl magithub pomidor imenu-list markdown-mode+ flymake-json flycheck flymake-cursor git-gutter package-lint ox-minutes projectile lua-mode pyenv-mode move-text web-mode use-package rainbow-delimiters ox-reveal nyan-mode multiple-cursors ac-html-angular+ markdown-preview-mode markdown-preview-eww magit json-mode flyspell-popup flyspell-correct-popup dired-rainbow csgo-conf-mode zenburn-theme helm-ag pomodoro helm-projectile)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pyvenv-mode t)
 '(select-enable-clipboard t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(x-select-enable-clipboard-manager t)
 '(x-select-request-type (quote UTF8_STRING))
 '(xterm-mouse-mode t))



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; [END]
