;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; MY CODE


(require 'use-package)

;; Expand region
(use-package expand-region
  :bind ("M-s" . er/expand-region)
  :bind ("M-S-s" . er/contract-region)
  )

;; Stop trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default show-leading-whitespace t)
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Pas hyper utile
(use-package easy-hugo
  :init
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-org-header t)
  (setq easy-hugo-basedir "~/Documents/blogs/blog-louvigne/")
  (setq easy-hugo-url "http://thomas-louvigne.github.io/")
  (setq easy-hugo-sshdomain "blogdomain")
  (setq easy-hugo-root "~/Documents/blogs/blog-louvigne/public/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-postdir "content/posts")
  :bind ("C-c C-e" . easy-hugo))

;; Correcteur gramaticale
(with-eval-after-load 'flycheck
  (flycheck-grammalecte-setup))
(use-package flycheck-grammalecte
  :hook (fountain-mode . flycheck-mode)
  :init
  (setq flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp nil
        flycheck-grammalecte-report-nbsp nil)
  :config
  (add-to-list 'flycheck-grammalecte-enabled-modes 'fountain-mode)
  (grammalecte-download-grammalecte)
  (flycheck-grammalecte-setup))

;; Bette org mode
(global-org-bulletproof-mode +1)

;; END OF MY CODE
(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;; Fin des conf locales
(provide 'init-locales)

;;; init-locales.el ends here
