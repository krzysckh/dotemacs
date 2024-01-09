(defun rc/load-theme (theme)
  (load-theme theme t)
  (scroll-bar-mode 0)
  (column-number-mode 1)
  (show-paren-mode 1)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(defun rc/set-font (fnt size)
  (add-to-list 'default-frame-alist `(font . ,(concat fnt "-" size))))

(defun rc/load-evil ()
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (require 'evil)
  (require 'evil-collection)
  (require 'evil-numbers)
  (evil-mode 1)
  (global-set-key (kbd "C-k") 'comment-dwim)
  (global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-S-a") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd ";") 'compile)
  (evil-collection-init))

(defun rc/define-ligatures ()
  (require 'ligature)
  (global-prettify-symbols-mode t)
  ;; gen ligatures for up to 80 dash || equals symbols
  (setq dash-ligatures
        (mapcar
         (lambda (x)
           (apply #'concat (mapcar (lambda (_) "-") (number-sequence 1 x))))
         (number-sequence 2 80)))

  (setq eq-ligatures
        (mapcar
         (lambda (x)
           (apply #'concat (mapcar (lambda (_) "=") (number-sequence 1 x))))
         (number-sequence 2 80)))

  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode
   (append
    '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
      "\\\\" "://")
    dash-ligatures
    eq-ligatures)))

(defun rc/define-keybindings ()
  (evil-define-key '(normal visual) 'global (kbd "]]")
    (lambda ()
      (interactive)
      (0x0-dwim (0x0--choose-server))
      (call-process-shell-command "sh -c 'xdg-open `xclip -sel c -o`' &" nil 0)))

  ;; switch buffers at backspace in normal mode
  (define-key evil-normal-state-map (kbd "<backspace>")
    (lambda ()
      (interactive)
      (switch-to-buffer (other-buffer)))))

(defun rc/download-file (url path)
  (when (not (file-exists-p path))
    (require 'url)
    (url-copy-file url path)))

(defun rc/download-lispfiles ()
  (when (not (file-directory-p additional-lisp-path))
    (mkdir additional-lisp-path))

  (add-to-list 'load-path additional-lisp-path)
  (rc/download-file
   "https://raw.githubusercontent.com/rougier/emacs-splash/master/splash-screen.el"
   (concat additional-lisp-path "splash-screen.el")))

(setq additional-lisp-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/everforest-theme")
(cond
 ((string= system-name "chad")
  (rc/load-theme 'everforest-hard-dark)
  (rc/set-font "Lilex" "12"))
 (t
  (rc/load-theme 'acme)
  (rc/set-font "Lilex" "9")))
(rc/load-evil)
(rc/define-ligatures)
(rc/define-keybindings)

(rc/download-lispfiles)

(defun about-emacs ()
  (switch-to-buffer "*scratch*"))

(require 'package)
(setq package-install-upgrade-built-in t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-16"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(require 'lice)
(setq lice:default-license "bsd-2-clause")
(setq lice:copyright-holder "krzysckh <kpm@linux.pl>")

(require 'elfeed)
(setq elfeed-feeds
      '("http://9front.org/releases/index.rss"
        "http://harmful.cat-v.org/Blog/index.rss"
        "http://rafalkosik.com/feed/"
        "https://xkcd.com/rss.xml"
        "https://ftrv.se/posts.atom"
        "https://nullprogram.com/feed/"
        "https://9lab.org/blog/index.xml"
        "https://100r.co/links/rss.xml"
        "https://rss.slashdot.org/Slashdot/slashdotMain"
        "https://text.causal.agency/feed.atom"))

(require 'lsp)
(add-hook 'java-mode-hook #'lsp)

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(global-undo-tree-mode)
(global-ligature-mode t)
(editorconfig-mode 1)
(ws-butler-global-mode)

;(add-hook 'c-mode-hook #'display-fill-column-indicator-mode)

;; line numbers
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (display-line-numbers-mode)
            (setq display-line-numbers 'relative)))

;; highlight todos
(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (highlight-regexp "TODO:" 'diff-error)))

(require 'splash-screen)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(custom-safe-themes
   '("835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" default))
 '(evil-undo-system 'undo-tree)
 '(initial-buffer-choice t)
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("dvipng" "latex")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -D %D -T tight -o %O %f")
             :transparent-image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(package-selected-packages
   '(nodejs-repl lsp-java w3m company-quickhelp acme-theme pdf-tools elfeed 0x0 lice indent-guide howdoyou evil-numbers perl-doc ws-butler vterm-toggle vterm eglot lsp-ui lsp-mode rust-mode uxntal-mode magit evil-collection racket-mode all-the-icons undo-tree ligature editorconfig flycheck company evil)))
