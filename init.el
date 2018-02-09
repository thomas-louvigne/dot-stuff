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

(package-initialize)
(setq url-http-attempt-keepalives nil)
(setq use-package-always-ensure t)

(require 'org)
(require 'ox)

;; Tricks divers
;; ---------------------------------------------------
;; Plein de trucs divers et nécessaire

;; Indentation d'une région
(global-set-key (kbd "C-x C-a") 'indent-region)

;; Pour avoir les parenthese coloré automatiquement
(show-paren-mode 1)

;; Pour avoir le numéro de la ligne à gauche
;; (global-linum-mode 1);; active le mode
;; (setq linum-format "%2d| ") ;; 2> cole à gauche puis | puis space

;; Pas de menubar en haut ni de scroll bar
(menu-bar-mode -1) ;; Enleve la bar du haut qui est useless
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; Enlever la scrollbar
  (scroll-bar-mode -1))

;; Permet de changer le répertoire de sauvegarde automatique (évite d'avoir des fichiers ~  qui trennent partout)
(require 'saveplace)
(setq-default save-place t)

;; Permet de configurer le dossier de backup des fichiers
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

;; Navigate
;; changed beacause ELPY bind M-> & M-<
(global-set-key (kbd "<S-up>") 'beginning-of-buffer)
(global-set-key (kbd "<S-down>") 'end-of-buffer)
(global-set-key (kbd "C-d") 'kill-whole-line) ;; remplacement du kill-char totalement uselss


;; Multiple cursor
;; ------------------------------------------------------------
;; Emacs porn : http://emacsrocks.com/e13.html

(require 'multiple-cursors)
(global-set-key (kbd "M-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "<f10>") 'mc/mark-next-like-this)
(global-set-key (kbd "<f9>") 'mc/mark-previous-like-this)
(global-set-key (kbd "<f11>") 'mc/mark-all-like-this)

;; [TEST] Désactive l'indentation avec des tabs
;; -------------------------------------------------------
(setq-default indent-tabs-mode nil)

;; COMPANY
;; -------------------------------------------------------
;;(require 'popup)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode) ;; Là on dit que c'est pour tout

;; Don't enable company-mode in below major modes : pas dans le shell, ni erc ...
(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode python-mode))

;; Org Mode
;; ---------------------------------------------------
(require 'org)
(setq org-log-done t) ;; Sait pas à quoi ca sert
(setq org-startup-truncated nil) ;; Permet de faire de retours à la ligne
(setq org-export-with-sub-superscripts nil) ;; Évites les erreures qd on export les caractères __
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "DONE")))


;; [TEST] Correcteur orthographique / dictinnaire
;; ---------------------------------------------------
;; EST EN TRAIN DE PASSER à Grammacollect
;;(require 'flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell) ;; Ajoute automatiquement le flysper aux fichier org
(setq ispell-dictionary "french")

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
(line-number-mode t)

;; Nyan-mode : Permet de savoir ou tu es dans ta page (Assez utile finalement)
;;(require 'nyan-mode)
(nyan-mode t)

;; Affiche l'heure dans la barre du bas
;; Set le buffer du de la date et du temps
(display-time-mode t) ;; affiche le temps

;; Utilise diminish.el <3
;; Permet de retirer les minor-mod inutile de la botom line
;;(diminished-modes t)
(diminish 'anaconda-eldoc-mode)
(diminish 'edebug-mode)
(diminish 'flycheck-mode)
(diminish 'helm-mode)
(diminish 'rainbow-delimiters-mode)
(diminish 'yas-mode)
(diminish 'yas-minor-mode)
(diminish 'python-mode)
(diminish 'eldoc-mode)
(diminish 'company-mode)
(diminish 'git-gutter)
(diminish 'python)
(diminish 'elpy-mode)
(diminish 'projectile-mode)
(diminish 'git-gutter-mode)
(diminish 'flymake-mode)
;;(diminish 'server-clients-mode)


;; Permet Pas écrire dans le prompt du mini buffer
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Clean White Space (Trailing whitespace)
;; ----------------------------------
;; Montre les Whites space inutile en fin de ligne
;; TODO : show uniquement dans le code ET dans les .org
(setq-default show-trailing-whitespace t)
(setq-default show-leading-whitespace t) ;; espace en fin de ligne
(setq-default indicate-empty-lines t)

;; Efface automatiquement les espaces de fin de ligne a chaque sauve
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org-export stysheet
;; ----------------------------------
;; Style pour l'export org-mode
;; TODO : le reset pour owncloud
;;(setq org-odt-styles-file "/home/zobi8225/Dropbox/style.ott")


;; THEME
;; ----------------------------------
;; (setq sml/no-confirm-load-theme t)
(load-theme 'zenburn t)


;; UTF8 Partout : Parce que c'est le turfu
;; -----------------------------------------------------------------
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; [TEST] force la font a 10
;; -----------------------------------------------------------------

(set-frame-font "DejaVu Sans Mono-10")

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

;; Couleur pour le terminal lors d'une compile (ou d'un test)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)


;; Permet de ne pas voir les white space dans le term
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
))

;; Imenu-list
;; ------------------------------------------------------------
;; Permet d'avoir un menu avec les class / methodes du buffer
;; Merde un peu qd on lance emacs en deamon
;; (imenu-list-minor-mode t)
;; (global-set-key (kbd "<f8>") #'imenu-list-smart-toggle)

;; [TEST] Treemacs
;; ------------------------------------------------------------
;; permet d'avoir un menu en arbre
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-change-root-without-asking nil
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-never-persist              nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      45)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ))
(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))





;; Open URL dans emacs
;; ------------------------------------------------------------
;; Use firefox to open urls
(setq browse-url-browser-function 'browse-url-firefox)


;; Rainbow délimiters
;; ------------------------------------------------------------
;; Met les parenthèses en "rainbows", très utile pour ne plus se perdre dans les parenthès

;;(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

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


;; Counter Strike :Global Offensive
;; ------------------------------------------------------------
;; Parce que emacs à un mode pour le fichier autoexe de counf de CS:GO
(require 'csgo-conf-mode)

;; Web-mode
;; ------------------------------------------------------------
(require 'web-mode)
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; use web-mode for html
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; adjust indents for web-mode to 1 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode,  adjust indentation."
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 1)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 1)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)




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
;; a vérifier
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


;;(add-to-list 'load-path "~/.emacs.d/elpa/tern-20170925.1333/")
;;(autoload 'tern-mode "tern.el" nil t)

;; [Marche pas] Company term (better completion)
;; (add-to-list 'company-backends 'company-tern)
;; (add-hook 'js2-mode-hook (lambda ()
;;                            (tern-mode)
;;                            (company-mode)))

(add-hook 'js2-mode-hook (lambda ()
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)

;; Hook pour passer le linter a chaque save
(eval-after-load 'js2-mode
	   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;; Markdown mode
;; ------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ox-reveal
;; ------------------------------------------------------------
;; Permet de faire de beau powerpoint à partir des .org
;; Attention, dans le .org, il faut checker le PATH du js
(require 'ox-reveal)



;; [Test] Move text
;; ------------------------------------------------------------
;; Permet de bouger des lignes sélectionné (regions) avec  M-S ^ /
;; Alt + Maj + fleche haut ou bas
(move-text-default-bindings)

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


;; Helm
;; ------------------------------------------------------------
;; Permet d'améliorer le M-x et pas mal d'autre chose
(require 'helm-mode)
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-s") #'helm-occur)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)
(helm-mode 1)

(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

;; Helm-ag
;; ------------------------------------------------------------
;; AG => Sorte de grep
;; Avec projectile, permet de trouver l'occurence d'un mot dans un projet


;; Projectile
;; ------------------------------------------------------------
;;
(projectile-mode)
(helm-projectile-on)

(use-package projectile
  :init (progn
          (projectile-global-mode)
          (setq projectile-enable-caching t)
          (setq projectile-ignored-directories  '("node" "_output"))
          (setq projectile-ignored-files '(".DS_Store" ".gitmodules" ".gitignore" "pkg" "bin") )
          )
  :bind (
         ("<f1>" . helm-projectile-switch-project) ;; Change le projet de travail
	 ("<f2>" . helm-projectile)  ;; Cherche un fichier
         ("<f3>" . helm-projectile-ag) ;; Sorte de grep
	 ("<f4>" . helm-projectile-switch-to-buffer) ;; Switch entre les buffer
         )
:ensure t)

;; AC-mode
;; ------------------------------------------------------------
;;(ac-config-default)
;;(ac-set-trigger-key "TAB")


;; Playerctl
;; ------------------------------------------------------------
;; Permet de changer de musique, notament spotify, depuis emacs
;; Nécessiste playerctl
;; Développé par moi :-)
(require 'playerctl)
(define-key global-map (kbd "C-c C-SPC") 'playerctl-play-pause-song)
(define-key global-map (kbd "C-c C-n") 'playerctl-next-song)
(define-key global-map (kbd "C-c C-p") 'playerctl-previous-song)

;; [Test] Yasnippet
;; ------------------------------------------------------------
;; Permet de générer automatiquement du code
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;; (require 'yasnippet)
(yas-global-mode 1)

;; Git Gutter
;; ------------------------------------------------------------
;; Permet de montrer ce qui a changé dans git dans le linum (la bar de gauche)
(global-git-gutter-mode t)

;; Flycheck
;; ------------------------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Pomodori
;; ------------------------------------------------------------
(global-set-key (kbd "<f12>") #'pomidor)
(setq alert-default-style 'libnotify)
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

;; [TEST] Elpy
;; ------------------------------------------------------------
(package-initialize)
(elpy-enable)

(setq elpy-rpc-backend "jedi")
(pyvenv-activate "/home/tlu/working/sief/sief-back/venv/")

;; [TEST] Devrait choisir entre company et yasnipiet
;; (defun company-yasnippet-or-completion ()
;;   "Solve company yasnippet conflicts."
;;   (interactive)
;;   (let ((yas-fallback-behavior
;;          (apply 'company-complete-common nil)))
;;     (yas-expand)))

;; (add-hook 'company-mode-hook
;;           (lambda ()
;;             (substitute-key-definition
;;              'company-complete-common
;;              'company-yasnippet-or-completion
;;              company-active-map)))


;; [TEST] terminal interpreter
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--simple-prompt --pprint")


;; [TEST] Compilation (et test avec elpy) dans une frame en bas
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





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-test-pytest-runner-command (quote ("pytest" "-p no:sugar
")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(font-use-system-font t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-ignore-buffer-patterns (quote ("\\.html\\'" "\\.htm\\'")))
 '(helm-ag-ignore-directory (quote ("html_cov" "node_module" ".tmp" "dist")))
 '(helm-ag-insert-at-point (quote symbol))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (helm-xref tern-context-coloring company-tern xref-js2 js2-refactor add-node-modules-path pyvenv python treemacs-projectile treemacs elpy exec-path-from-shell htmlize diminish helm diff-hl magithub pomidor imenu-list markdown-mode+ flymake-json flycheck flymake-cursor git-gutter playerctl package-lint ox-minutes projectile lua-mode pyenv-mode move-text web-mode use-package rainbow-delimiters ox-reveal nyan-mode multiple-cursors ac-html-angular+ markdown-preview-mode markdown-preview-eww magit json-mode flyspell-popup flyspell-correct-popup dired-rainbow csgo-conf-mode zenburn-theme helm-ag pomodoro helm-projectile)))
 '(pyvenv-mode t))



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; [END]
