(package-initialize)

(require 'use-package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))

;; Remove cluttered toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(hl-line-mode 1)
(setq-default visible-bell 0
              indent-tabs-mode nil)

;; Keeps me from fat fingering C-x b
;;(global-set-key (kbd "C-x C-b") 'counsel-switch-to-buffer-or-window)

;; Kill all line
(global-set-key (kbd "C-d") 'kill-whole-line) ;; fait un couper de toute la ligne, remplace du kill-char totalement uselss

;; Disable autosave because it's slow
(auto-save-mode -1)

;; Auto-revert
(global-auto-revert-mode t)

;; Parentheses (&friends) helper
(electric-pair-mode -1)

;; Save adding :ensure t on every use package
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; Expand region
(use-package expand-region
  :bind ("C-c =" . er/expand-region)
  )

;; A mettre sur W:bind ("M-S-w" . er/expand-region)

;; Switch windows
(global-set-key (kbd "<f6>") #'other-window)

;; Git integration
(use-package magit
  :bind ("C-x C-g" . magit-status))
(use-package github-browse-file)
(use-package gist)

(use-package helm
  :init (progn
          (setq helm-autoresize-max-height 20)
          )
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . #'helm-find-files)
  ("C-x C-b" . #'helm-buffers-list)
  ("C-s" . #'swiper-helm)
  )
(require 'helm-config)
(helm-mode 1)

;; Searching with projectile
(use-package helm-ag)
(use-package helm-projectile)

;; Manage projects with a keystroke
(use-package projectile
  :init (progn
          (setq projectile-enable-caching t)
          (setq projectile-indexing-method 'native)
          (setq projectile-ignored-directories '("_output" "node_module" "pkg"))
          (setq projectile-ignored-files '(".DS_Store" ".gitmodules" ".gitignore") )
          )
  :bind (
         ("<f1>" . helm-projectile-switch-project) ;; Change le projet de travail
         ("<f2>" . helm-projectile-find-file)  ;; Cherche un fichier
         ("<f3>" . helm-projectile-ag) ;; Sorte de grep, en mieux
         )
  :config (projectile-mode 1))

;; If nothing is marked yanks whole line
(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode 1))

;; Autocomplete Popups
(use-package company
  :config (global-company-mode 1))

;; Gitgutter
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :bind(
        ("C-n" . git-gutter:next-hunk)
        ("C-p" . git-gutter:previous-hunk)
        )
  :config (global-diff-hl-mode 1))

;; Yaml editing support
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package org-bullets
  :hook org-mode)

;; Web mode
(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :config (setq
	   web-mode-markup-indent-offset 2
	   web-mode-code-indent-offset 2))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :config (setq typescript-indent-level 2))

;; JavaScript mode
;; Better highlighting for JS files (potential support for JSX too)
(use-package js2-mode
  :interpreter ("node" . js2-mode)
  :mode ("\\.m?jsx?\\'" . js2-mode)
  :config (setq js2-basic-offset 2
                js2-indent-switch-body t
		js2-strict-missing-semi-warning nil
                js2-mode-show-strict-warnings nil))

;; Hook pour passer le linter a chaque save
(eval-after-load 'js2-mode
	   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))


(use-package lsp-mode
  :config (setq lsp-print-io t))
(use-package lsp-javascript-typescript
  :hook (typescript-mode . lsp-javascript-typescript-enable))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/tslint/bin/tslint"
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

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


(use-package flycheck
  :hook ((flycheck-mode . my/use-tslint-from-node-modules)
         (flycheck-mode . my/use-eslint-from-node-modules))
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode))

;; Maybe I can finally start using it (maybe)
(use-package ace-jump-mode
  :bind ("C-c ." . ace-jump-mode))

;; Markdown editing
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; 4daLookz
(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package yasnippet
  :config (setq yas-snippet-dirs
                '("~/.emacs.d/snippets")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'show-paren-mode)
(setq linum-format "%2d|") ;; ajout un pip de séparation
(add-hook 'prog-mode-hook 'linum-mode)
(setq-default cursor-type 'box)
(set-frame-font "Roboto Mono 12")

;; Disable backup files (# and ~ files)
(setq make-backup-files nil
      auto-save-default nil)

;; Change customize-* file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default show-leading-whitespace t)
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Smart C-a
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

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


;; flyspell Correcteur orthographique / dictinnaire
;; TODO à vérifier si ca marche
(use-package flyspell
  :diminish
  :init (setq ispell-dictionary "french")
  :hook (org-mode . turn-on-flyspell)
  )

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

;; Diminish
;; Permet de retirer les mods de la botom line (qui prennent de la place pour rien)
;;(diminished-modes t)
(use-package diminish)
(diminish 'anaconda-eldoc-mode)
(diminish 'edebug-mode)
(diminish 'flycheck-mode)
(diminish 'helm-mode)
(diminish 'rainbow-delimiters-mode)
(diminish 'python-mode)
(diminish 'eldoc-mode)
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
(diminish 'ivy-mode)


;; [TEST] Editor Config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
;; Hugo blog

(use-package ox-hugo
  :after ox)

;;; init.el ends here
