;; dot emacs de ouam - zobi8225

;;(add-to-list 'load-path (expand-file-name "~/.emacs.d") t)
;; Pas propre..

;; Enlève le warning
;; (defadvice display-warning
;;     (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
;;   "Ignore the warning about the `.emacs.d' directory being in `load-path'."
;;   (unless (and (eq type 'initialization)
;;                (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
;;                                 message t))
;;     ad-do-it))

;; PACKAGE
(require 'package)
(setq package-archives '(
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         )
      )
(package-initialize)
(setq url-http-attempt-keepalives nil)


;; Langue dico
(add-hook 'org-mode-hook 'turn-on-flyspell) ;; Ajoute automatiquement le flysper aux fichier org
(setq ispell-dictionary "french")


;; Affiche le volume
(require 'volume)



;; TUTO : Set keymap
;;(global-set-key (kbd "C-a") 'MAFONCTION)



;; Indent un rectangle
(global-set-key (kbd "C-x C-a") 'indent-region)

;; Pour avoir les parenthese coloré automatiquement
(show-paren-mode 1)

;; Pour pouvoir commenter une region
(global-set-key (kbd "C-C C-C") 'comment-dwim)

;; Pour avoir le numéro de la ligne à gauche
(global-linum-mode 1);; active le mode
(setq linum-format "%2d| ") ;; 2> cole à gauche puis | puis space

;; Better default
(ido-mode t) ;; A quoi ca sert ?
(setq ido-enable-flex-matching t)

(menu-bar-mode -1) ;; Enleve la bar du haut qui est useless
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; Enlever la scrollbar
  (scroll-bar-mode -1))

;; Permet de faire des trucs mais ont sait pas quoi
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
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; A quoi ca sert ?
;;(setq-default indent-tabs-mode nil)


;; Batterie dans la buffer line
(display-battery-mode t) ;; Sert a afficher la batterie (utile pour les PC portable)

;; Permet de savoir ou tu es dans ta page
(nyan-mode t) ;; Nyan mod utile pour savoir ou on en est dans la page

;; Affiche l'heure dans la barre du bas
(setq display-time-24hr-format t display-time-day-and-date t display-time-interval 50 display-time-default-load-average nil display-time-mail-string "") ;; Set le buffer du de la date et du temps
(display-time-mode t) ;; affiche le temps

;; Montre les Whites space inutile en fin de ligne
(setq-default show-trailing-whitespace t)
 (setq-default show-leading-whitespace t) ;; espace en fin de ligne
(setq-default indicate-empty-lines t)

;; Efface automatiquement les espaces a chaque save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Insert date string -
 (defun insert-date-string ()
   "Insert a nicely formated date string."
   (interactive)
   (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(global-set-key (kbd "C-d") 'insert-date-string)

;; Style pour l'export org-mode
(setq org-odt-styles-file "/home/zobi8225/Dropbox/style.ott")


;; Theme calm-forest

(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)


;; Enlève le welcome popup
(custom-set-variables
 '(inhibit-startup-screen t))
(custom-set-faces)


;; Pour le BLOG ~ (Working :D)

(defvar thomas-website-html-head
  "
        <link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css'>

")

(defvar thomas-website-html-postamble  "
        <div id='disqus_thread'></div>
        <script type='text/javascript'>
        var disqus_shortname = 'thomasluquet'; // required: replace example with your forum shortname

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
         var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
         dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
         (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
         })();
        </script>
        <noscript>Please enable JavaScript to view the <a href='http://disqus.com/?ref_noscript'>comments powered by Disqus.</a></noscript>
        <a href='http://disqus.com' class='dsq-brlink'>comments powered by <span class='logo-disqus'>Disqus</span></a>

        <div class='footer'>
        <hr>
        Blog de %a.<br>
        Last updated %C. <br>
        Built with %c.
        </div>")


(require 'ox-publish)
(require 'ox-html)
(setq org-publish-project-alist
      `(("org-html"
         :base-directory "~/Dropbox/blog/luquet/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/blog/luquet/out"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :table-of-contents: nil
         :html-head ,thomas-website-html-head
         :html-preamble nil
         :html-postamble ,thomas-website-html-postamble
         :table-of-contents t        ; Set this to "t" if you want a table of contents, set to "nil" disables TOC.
         :toc-levels 2               ; Just the default for this project.
         :section-numbers nil        ; Set this to "t" if you want headings to have numbers.
         :style-include-default nil ;Disable the default css style
         )

        ("org-static"
         :base-directory "~/Dropbox/blog/luquet"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "Dropbox/blog/luquet/out"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org" :components ("org-html" "org-static"))
        )
      )


;; Org-blog 2 twbs - MARCHE PAS
;; (setq org-publish-project-alist
;;       '("org-note"
;;         :base-directory "~/Dropbox/blog/luquet/"
;;         :publishing-directory "~/Dropbox/blog/luquet/out/"
;;         :publishing-function org-twbs-publish-to-html
;;         :with-sub-superscript nil
;;         ))

;; Pas écrire dans le prompt du mini buffer
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
