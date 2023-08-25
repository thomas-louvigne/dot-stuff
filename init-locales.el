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


;; Force les fichiers .txt a s'ouvre dans le text-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))

;; Correcteur gramaticale

;; Setup :
;; (with-eval-after-load 'flycheck
;;   (flycheck-grammalecte-setup))

(use-package flycheck-grammalecte
  :hook (fountain-mode . flycheck-mode)
  :init
  (setq flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp t
        flycheck-grammalecte-report-nbsp nil
        ;; Ajout trouvé sur les internets
        flycheck-grammalecte-filters-by-mode
        '(
          (org-mode "(?ims)^[ \t]*#\\+begin_src.+?#\\+end_src"
                    "(?im)^[ \t]*#\\+begin[_:].+$"
                    "(?im)^[ \t]*#\\+end[_:].+$"
                    "(?m)^[ \t]*(?:DEADLINE|SCHEDULED):.+$"
                    "(?m)^\\*+ .*[ \t]*(:[\\w:@]+:)[ \t]*$"
                    "(?m)^[ \t]*-[ \t]*" ;; tirets pour orgmode
                    "(?im)^:[a-z]+:" ;; propriétés org-roam
                    "(?im)^[ \t]*#\\+(?:caption|description|keywords|(?:sub)?title):"
                    "(?im)^[ \t]*#\\+(?!caption|description|keywords|(?:sub)?title)\\w+:.*$")
          (text-mode "(?m)^[ \t]*-[ \t]*" ;; tirets pour textmode
                     )
          (message-mode "(?m)^[ \t]*(?:[\\w_.]+>|[]>|]).*"))
        )
  :config
  (add-to-list 'flycheck-grammalecte-enabled-modes 'fountain-mode)
  (grammalecte-download-grammalecte)
  (flycheck-grammalecte-setup))

;; Better org mode
(use-package org-bulletproof
  :init
  (setq global-org-bulletproof-mode t)
  )

;; Fin des conf locales
(provide 'init-locales)

;;; init-locales.el ends here
