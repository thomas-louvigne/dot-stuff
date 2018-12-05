;;; package --- My DOT-file
;;; Commentary:
;; It is my EMACS conf, enjoy it
;;; Code:
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

;; Kill all line
(global-set-key (kbd "C-d") 'kill-whole-line) ;; fait un couper de toute la ligne, remplace du kill-char totalement uselss

;; Anable autosave
(setq auto-save-default nil)
(savehist-mode t)

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
  :bind ("M-d" . er/expand-region)
  )

;; Mouse trick to change windows focus or resizing with click
(xterm-mouse-mode t)

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
  ("M-q" . helm-imenu)
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

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)
(setq projectile-ignored-directories '("_output" "node_module" "pkg"))

(use-package projectile
  :init (progn
          (setq projectile-enable-caching t)
          (setq projectile-indexing-method 'native)
          (setq projectile-globally-ignored-directories '("node_module"))
          (setq projectile-ignored-directories '("_output" "node_module" "pkg"))
          (setq projectile-ignored-files '(".DS_Store" ".gitmodules" ".gitignore" "package-lock.json") )
          )
  :bind (
         ("<f1>" . helm-projectile-switch-project) ;; Change le projet de travail
         ("<f2>" . helm-projectile-find-file)  ;; Cherche un fichier
         ("<f3>" . helm-projectile-ag) ;; Sorte de grep, en mieux
         )
  :config (projectile-mode 1))



;; Let helm-ag searching at point
(setq helm-ag-insert-at-point 'symbol)

;; If nothing is marked yanks whole line
(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode 1))

;; Autocomplete Popups
(use-package company
  :config (global-company-mode 1))

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
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

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


;; (use-package flycheck
;;   :hook ((flycheck-mode . my/use-tslint-from-node-modules)
;;          (flycheck-mode . my/use-eslint-from-node-modules))
;;   :config
;;   (global-flycheck-mode)
;;   (setq-default flycheck-disabled-checkers '(javascript-jshint))
;;   (flycheck-add-mode 'javascript-eslint 'js2-mode)
;;   (flycheck-add-mode 'typescript-tslint 'typescript-mode))

;; (global-flycheck-mode)
(use-package flycheck
  :config

  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode))

;; (setq max-lisp-eval-depth 10000)

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


;; Try other stuff
(global-display-line-numbers-mode)

(setq-default cursor-type 'box)
(set-frame-font "Roboto Mono 11")

;; Disable backup files (# and ~ files)
;; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
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
(diminish 'helm)
(diminish 'rainbow-delimiters-mode)
(diminish 'python-mode)
(diminish 'eldoc-mode)
(diminish 'git-gutter)
(diminish 'git-gutter+-mode)
(diminish 'git-rebase-mode)
(diminish 'git-gutter-mode-major-mode)
(diminish 'magit-mode)
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
(diminish 'undo-tree-mode)
(diminish 'lisp-mode)
(diminish 'editorconfig-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'helm-modes-using-escaped-strings)
(diminish 'compilation-mode)
(diminish 'emacs-lisp-mode)

;; [TEST] Editor Config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Hugo blog
(use-package ox-hugo
  :after ox)

;; [TEST] Go to variable OR function
(defun bzg-find-variable-or-function-at-point ()
  (interactive)
  (or (find-variable-at-point)
      (find-function-at-point)
      (message "No variable or function at point.")))

(global-set-key (kbd "C-.") 'bzg-find-variable-or-function-at-point)

;; Dumb-jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-RET" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'helm)
  :ensure)

;; dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . sidebar-toggle))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-theme 'doom-themes) ;; j'ai pas l'impression que ca marche
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

;; Grammalecte - French grammar
;; (require 'flycheck-grammalecte)
;; (load-file "~/.emacs.d/flycheck-grammalecte/flycheck-grammalecte.elc")
;; (setq flycheck-grammalecte-report-apos nil
;;       flycheck-grammalecte-report-nbsp nil)
;; (setq flycheck-grammalecte-enabled-modes
;;       '(org-mode text-mode markdown-mode))

;; Open the file name being pointed in an other window or dired
;; reference: http://kouzuka.blogspot.com/2011/02/emacsurlfinder.html
(defun my-directory-or-file-p (path)
  "Return t if PATH is a directory, return nil if PATH is a file."
  (car (file-attributes path)))

;; (defun my-open-emacs-at-point ()
;;   "Open the file with opening EMACS."
;;   (interactive)
;;   (require 'ffap)
;;   (let ((file (or (ffap-url-at-point)
;;                   (ffap-file-at-point))))
;;     (unless (stringp file)
;;       (error"No file or URL found"))
;;     (when (file-exists-p (expand-file-name file))
;;       (setq file (expand-file-name file)))
;;     (message "Open: %s" file)

;;     (if (my-directory-or-file-p file)
;;       (dired-other-window file)
;;       (find-file-other-window file))
;;     ))

;; (global-set-key (kbd "\C-c o") 'my-open-emacs-at-point)

;; double click
(global-set-key [double-mouse-1] 'my-open-emacs-at-point)
(global-set-key [double-down-mouse-1] 'ignore) ; mouse-drag-region

;; TODO
;;--------
;; (defun find-file-or-jump-to-definition ()
;;   "Find a file or jump to a definition."
;;   (interactive)
;;   (let* ((tap (thing-at-point 'filename))
;;          (fname (when tap (file-relative-name tap default-directory))))
;;     (if (and (fname (file-exists-p fname)))
;;         (find-file fname)
;;       (js2-jump-to-definition))))

;; Switch to camelCase
(require 'string-inflection)
(global-set-key (kbd "C-c c") 'string-inflection-lower-camelcase)


;; Le you know what you have modified during this commit
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-g") 'git-gutter)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)


(defun quotes-on-symbol-at-point ()
  (interactive)
  (save-excursion
    (let
        ((bounds (bounds-of-thing-at-point 'symbol)))
      (goto-char (car bounds))
      (insert "\"")
      (goto-char (+ 1 (cdr bounds)))
      (insert "\""))))




;;; Init.el ends here
