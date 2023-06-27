;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; MY CODE

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

;; Installation :
(with-eval-after-load 'flycheck
  (flycheck-grammalecte-setup))
;; Finally, the last expected step is to download the
;; CLI & Server upstream package. Just enter the following command:
;; M-x grammalecte-download-grammalecte


(use-package flycheck-grammalecte
  :hook (fountain-mode . flycheck-mode)
  :init
  (setq flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp t
        flycheck-grammalecte-report-nbsp nil)
  :config
  (add-to-list 'flycheck-grammalecte-enabled-modes 'fountain-mode)
  (grammalecte-download-grammalecte)
  (flycheck-grammalecte-setup))

;; Better org mode
(global-org-bulletproof-mode +1)

;; Fin des conf locales
(provide 'init-locales)

;;; init-locales.el ends here
