;; .emacs -*- lexical-binding: t -*-

(defvar rc/emoji-font "Noto Color Emoji")

(defun rc/load-theme (theme)
  (load-theme theme t)
  (scroll-bar-mode 0)
  (column-number-mode 1)
  (show-paren-mode 1)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(defun rc/set-font (fnt size)
  "set the main font to `fnt' with size `size'. `size' and `fnt' are both strings."
  (let ((f (concat fnt ":pixelsize=" size)))
    (set-frame-font f)
    (set-face-font 'default f)

    (set-fontset-font
     t 'symbol
     (font-spec
      :family rc/emoji-font
      :size (string-to-number size)
      :weight 'normal
      :width 'normal
      :slant 'normal))))

(defun rc/load-evil ()
  (setq evil-want-minibuffer t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (require 'evil)
  (require 'evil-collection)
  (require 'evil-numbers)
  (evil-mode 1)
  (global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-S-a") 'evil-numbers/dec-at-pt)

  (evil-collection-init))

(defun rc/define-ligatures ()
  (require 'ligature)
  (global-prettify-symbols-mode t)
  ;; gen ligatures for up to 80 dash || equals symbols
  (setq long-ligatures '(?- ?= ?_ ?*))
  (setq lig (apply #'append (mapcar (lambda (x) (mapcar (lambda (ch) (make-string x ch)) long-ligatures)) (number-sequence 2 80))))

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
      "\\\\" "://" ";;;")
    lig)))

(defun rc/define-keybindings ()
  (require 'inv)
  (evil-define-key '(normal visual) 'global (kbd "]]")
    (lambda ()
      (interactive)
      (browse-url (0x0-dwim (0x0--choose-server)))))

  ;; switch buffers at backspace in normal mode
  (define-key evil-normal-state-map (kbd "<backspace>")
    (lambda ()
      (interactive)
      (switch-to-buffer (other-buffer))))
  (evil-define-key '(normal visual) 'global (kbd "|") #'shell-command-on-region)

  (evil-define-key 'normal 'global (kbd ";") #'compile)

  (global-set-key
   (kbd "C-x C-b")
   (lambda ()
     (interactive)
     (execute-extended-command "" "switch-to-buffer")))

  (evil-define-key 'normal 'global (kbd "+")
    (lambda ()
      (interactive)
      (enlarge-window-horizontally 5)))

  (evil-define-key 'normal 'global (kbd "-")
    (lambda ()
      (interactive)
      (shrink-window-horizontally 5)))

  (evil-define-key 'insert 'global (kbd "C-l")
    (lambda ()
      (interactive)
      (insert-char ?λ)))

  (evil-define-key 'normal 'global (kbd "C-c y")
    (lambda ()
      (interactive)
      (if-let ((id (inv/id-at-point)))
          (yt-handler (concat "https://youtube.com/watch?v=" id)))))

  (evil-define-key 'normal elfeed-show-mode-map (kbd "C-c p")
    (lambda ()
      (interactive)
      (if-let ((id (inv/id-at-point)))
          (inv/popup-thumbnail id))))

  (evil-define-key 'normal elfeed-show-mode-map (kbd "C-c d")
    (lambda ()
      (interactive)
      (if-let ((id (inv/id-at-point)))
          (request (concat "https://sponsor.ajay.app/api/branding?videoID=" id)
            :parser #'inv//json-read-l
            :complete (cl-function (lambda (&key data &allow-other-keys)
                                     (message "No alternative title.")
                                     (if-let ((titles (cdr (assoc 'titles data))))
                                         (message "%s" (cdr (assq 'title (car titles)))))))))))

  ;; ffs
  (evil-define-key '(insert) python-mode-map [(tab)] #'company-jedi)

  (evil-collection-define-key '(normal visual) 'elfeed-search-mode-map
    "r" 'elfeed-search-untag-all-unread)

  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key (kbd "C-c u") #'crux-view-url)
  (global-set-key (kbd "C-c t") #'crux-visit-term-buffer)
  (global-set-key (kbd "C-c I") #'crux-find-user-init-file)
  (global-set-key (kbd "C-c S") #'crux-find-shell-init-file)
  (global-set-key (kbd "C-c m") #'magit)
  (global-set-key (kbd "C-c e") #'eshell)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "C-c M-x") #'universal-argument)
  (global-set-key (kbd "C-k") #'comment-dwim)
  (global-set-key (kbd "C-x C-d") #'ido-dired)
  (global-set-key (kbd "C-c C-d") #'ido-dired)
  (global-set-key (kbd "C-x C-i") #'ielm)
  (global-set-key (kbd "C-x i")   #'ielm)
  (global-set-key (kbd "C-c i")   #'ielm)
  (global-set-key (kbd "C-x l") #'list-buffers)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c s s") #'inv/search)
  (global-set-key (kbd "C-c s c") #'inv/search-channel)
  (global-set-key (kbd "C-c s e") #'elfeed)
  )

(defun rc/download-file (url path)
  (when (not (file-exists-p path))
    (require 'url)
    (url-copy-file url path)))

(defun rc/require-lisp (url)
  (rc/download-file url (concat additional-lisp-path (car (last (split-string url "/"))))))

(defun rc/download-lispfiles ()
  (when (not (file-directory-p additional-lisp-path))
    (mkdir additional-lisp-path))

  (add-to-list 'load-path additional-lisp-path)
  (rc/require-lisp "https://raw.githubusercontent.com/Naheel-Azawy/holyc-mode.el/00291cebce101456b5ae6e2d45f5139abe463a42/holyc-mode.el")
  (rc/require-lisp "https://raw.githubusercontent.com/krzysckh/emacs-splash/master/splash-screen.el")
  )

(defun rc/networkp (&optional host)
  (if (eq system-type 'windows-nt)
      t
    (null (request-response-error-thrown (request (or host "https://kelp.krzysckh.org") :timeout 2 :sync t)))))

(setq additional-lisp-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/everforest-theme")
(cond
 ((string= system-name "ligol")
  (rc/load-theme 'everforest-hard-dark)
  (rc/set-font "Lilex" "17")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))
 (t
  (let ((h (string-to-number (format-time-string "%H"))))
    (if (or (<= h 7) (>= h 20) (getenv "EMACS_DARK_MODE"))
        (progn
         (rc/load-theme 'everforest-hard-dark)
         (set-mouse-color "#d3c6aa"))
      (progn
       (rc/load-theme 'acme)
       (set-mouse-color "#2b3339"))))
  (rc/set-font "Lilex" "15")))

(add-to-list 'load-path "~/.emacs.d/kelp/")
(require 'kelp)
(when (file-exists-p "~/txt/kelp-auth")
  (setq kelp/auth-key (f-read "~/txt/kelp-auth")))
(setq kelp/load-path "~/.emacs.d/kelp/")

(when (rc/networkp)
  (kelp/refresh)
  (mapcar #'kelp/install '(wttrin.el kto.el inv.el rcon.el yt-search.el kelp.el session-file-vars-hack.el pterodactyl.el))
  (kelp/update))

(rc/download-lispfiles)
(rc/load-evil)
(rc/define-ligatures)
(rc/define-keybindings)

(when (not (file-exists-p "~/.lice"))
  (make-directory "~/.lice"))

(rc/download-file "https://pub.krzysckh.org/bsd-3-clause-clear" "~/.lice/bsd-3-clause-clear")

(defun about-emacs ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(require 'session-file-vars-hack)
(require 'holyc-mode)
(require 'crux)
(require 'wttrin)
(require 'package)
(require 'ido-completing-read+)
(require 'smex)

(setq package-install-upgrade-built-in t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require 'eglot)
;; disable eglto in holyc-mode
(defun eglot-maybe-ensure ()
  (unless (derived-mode-p 'holyc-mode)
    (eglot-ensure)))

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-16" "--fallback-style=none"))
(add-hook 'c-mode-hook #'eglot-maybe-ensure)
(add-hook 'c++-mode-hook #'eglot-maybe-ensure)
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider :codeLensProvider :documentOnTypeFormattingProvider :inlayHintProvider))

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(require 'lice)
(add-to-list 'lice:license-directories "~/.lice")
(setq lice:default-license "bsd-3-clause-clear")
(setq lice:copyright-holder "Krzysztof Michałczyk <kpm@linux.pl>")

(require 'elfeed)

(setq browse-url-handlers (list (cons #'inv/videop #'(lambda (url &rest _) (yt-handler (concat "https://youtube.com/watch?v=" (inv/videop url)))))))

(defun yt-handler (url &rest args)
  (let ((buf (get-buffer-create "*yt-handler*")))
    (with-current-buffer buf
      (erase-buffer)
      (message "%s" (switch-to-buffer buf))
      (async-shell-command (concat "mpv '" url "'") buf buf))))

(defun elfeed-update-yt (auth)
  (interactive
   (list (read-string "piped.video Authorization: ")))
  (call-process-shell-command
   (concat "sh -c 'curl -H \"Authorization: " auth "\" https://pipedapi.kavin.rocks/subscriptions > /tmp/sub.json"
           " && subjson2elfeed.pl /tmp/sub.json")))

(setq elfeed-feeds
      '("http://9front.org/releases/index.rss"
        "http://harmful.cat-v.org/Blog/index.rss"
        "http://rafalkosik.com/feed/"
        "https://xkcd.com/rss.xml"
        "https://ftrv.se/posts.atom"
        "https://nullprogram.com/feed/"
        "https://9lab.org/blog/index.xml"
        "https://100r.co/links/rss.xml"
        "https://text.causal.agency/feed.atom"
        "https://www.spoj.com/FRAKTAL/rss/"
        ))

;; https://github.com/krzysckh/bin/blob/master/subjson2elfeed.pl
(when (file-exists-p "~/.elfeed-yt")
  (load "~/.elfeed-yt")
  (setq elfeed-feeds (append elfeed-feeds elfeed-youtube-rss-feeds)))

(require 'lsp)
(add-hook 'java-mode-hook #'lsp)
(setq lsp-headerline-breadcrumb-enable 0)

(require 'web-mode)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-sql-indent-offset 2)

;; $ npm install -g vscode-css-languageserver-bin
(add-to-list 'auto-mode-alist `("\\.css\\'" . ,(lambda () (web-mode) (lsp))))

;; (require 'simpc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(if (require 'eglot-quicklintjs nil 1)
    (progn
      (defun load-eglot-quicklintjs ()
        (interactive)
        (eglot-ensure))
      (setq js-indent-level 2)
      (add-hook 'js-mode-hook #'load-eglot-quicklintjs))
  (message "unable to load eglot-quicklintjs"))

(setq c-astyle-command "astyle --style='k&r' --indent=spaces=2 -xB")
(defun astyle (pmin pmax)
  (interactive "r")
  (shell-command-on-region
   pmin pmax
   c-astyle-command
   (current-buffer) t
   (get-buffer-create "*Astyle Errors*") t))

(require 'php-mode)
(add-hook
 'php-mode-hook
 (lambda ()
    (require 'company-php)
    (ac-php-core-eldoc-setup)
    (set (make-local-variable 'company-backends)
         '((company-ac-php-backend company-dabbrev-code)
           company-capf company-files))))

(require 'company)
(require 'company-jedi)
(require 'jedi-core)
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook #'jedi:setup)

(require 'ansi-color)
(when (string= system-name "ligol")
  (add-hook
   'compilation-filter-hook
   (lambda ()
     (toggle-read-only)
     (ansi-color-apply-on-region compilation-filter-start (point))
     (toggle-read-only))))

(require 'markdown-mode)
(setq markdown-fontify-code-blocks-natively t)

(defalias 'perl-mode 'cperl-mode)
(defalias 'php-mode 'web-mode)
(defalias 'html-mode 'web-mode)
(setq inferior-lisp-program "sbcl")
(setq confirm-kill-emacs 'y-or-n-p)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(global-undo-tree-mode)
(global-ligature-mode t)
(editorconfig-mode 1)
(ws-butler-global-mode)
(setq ido-everywhere t)
(setq compilation-scroll-output t)
(setq evil-undo-system 'undo-tree)
(setq initial-buffer-choice t)
(setq scheme-program-name "olr") ;; olr = ol -r

(ido-mode 1)
(ido-ubiquitous-mode)
;; (flx-ido-mode nil)
(global-company-mode)

(require 'rcon)
(require 'yt-search)
(when (not (require 'kto nil 1))
  (message "unable to load `kto'"))

;(add-hook 'c-mode-hook #'display-fill-column-indicator-mode)

(require 'fortune)
(setq fortune-program "9")
(setq fortune-program-options "fortune")
(setq fortune-file (expand-file-name "~/nmojeprogramy/plan9front/lib/theo"))

;; line numbers
(defun run-line-mode ()
  (display-line-numbers-mode)
  (setq display-line-numbers 'relative))
(add-hook 'prog-mode-hook #'run-line-mode)

;; highlight todos
(add-hook
 'prog-mode-hook
 (lambda ()
   (interactive)
   (highlight-regexp "TODO:" 'diff-error)))

;; TODO: do innego pliku idk.. ~/.emacs.d/lisp/util.el idk idk idk
(defun plan ()
  (interactive)

  (setq shell-command-buffer-name-async "*plan*")
  (with-current-buffer (get-buffer-create "*plan*")
    (switch-to-buffer "*plan*")
    (async-shell-command "plan")
    (read-only-mode)
    (text-scale-adjust -1)))

(defun dos2unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(defun zzz ()
  (interactive)
  (require 'zone)
  ;; difference between emacs 29.1 on my laptop, and whatever i have on my debian
  (cond
   ((listp zone-programs)  (setq zone-programs '(zone-pgm-five-oclock-swan-dive)))
   ((arrayp zone-programs) (setq zone-programs [zone-pgm-five-oclock-swan-dive])))

  (with-current-buffer (get-buffer-create "*zone*")
    (run-line-mode))
  (zone))


(defun search-in-evil-collection (mode)
  "searches evil-collection for `mode' and - if found - opens up a new buffer with the file that defines keybindings for it"
  (interactive
   (let ((cur (symbol-name major-mode)))
     (list (read-string (concat "search for mode [default: " cur "]: ") nil nil cur))))
  (let* ((files (cl-delete-if-not
                 (lambda (s) (and (string-match-p mode s) (string-match-p ".*\\.el$" s)))
                 (directory-files-recursively (car (file-expand-wildcards "~/.emacs.d/elpa/evil-collection*")) "")))
         (chosen (cond
                  ((> (length files) 1) (ido-completing-read "Select file to open: " files))
                  ((equal (length files) 0) (error "not found in evil-collection"))
                  (t (car files)))))
    (find-file chosen)))

(defun reload-file-variables ()
  (interactive)
  (normal-mode))

(defun kill-all-buffers ()
  (interactive)
  (when (y-or-n-p "kill-all-buffers?")
   (mapcar #'kill-buffer (mapcar #'buffer-name (buffer-list)))))

(defun olr ()
  (interactive)
  (comint-run "olr"))

(require 'f)
(require 'eshell)
(require 'em-alias)

(defun eshell-write-aliases-list ()
  0)

(setq shrc (if (string= system-name "ligol")
               (expand-file-name "~/.bashrc")
             (expand-file-name "~/.kshrc")))

;; load aliases to eshell from `shrc' file
(when (file-exists-p shrc)
  (dolist (l (cl-remove-if-not
              (lambda (v) v)
              (mapcar
               (lambda (s) (if (string-match "alias \\(?1:.*?\\)=\\(?3:[\"']?\\)\\(?2:.*\\)\\3" s)
                          (list
                           (match-string 1 s)
                           (concat (match-string 2 s) " $*"))
                        nil))
               (split-string (f-read-text shrc) "\n"))))
    (funcall #'eshell/alias (car l) (cadr l))))

(defun abbrevize (l)
  (apply #'vector (mapcar (lambda (s) (substring s 0 3)) l)))

(setq calendar-month-name-array ["Styczeń" "Luty" "Marzec" "Kwiecień" "Maj" "Czerwiec" "Lipiec" "Sierpień" "Wrzesień" "Październik" "Listopad" "Grudzień"])
(setq calendar-month-abbrev-array (abbrevize calendar-month-name-array))

(setq calendar-day-name-array ["Niedziela" "Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek" "Sobota"])
(setq calendar-day-abbrev-array (abbrevize calendar-day-name-array))

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
   '("e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "a53c7ff4570e23d7c5833cd342c461684aa55ddba09b7788d6ae70e7645c12b4" "67f6b0de6f60890db4c799b50c0670545c4234f179f03e757db5d95e99bac332" "835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" default))
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
   '(haskell-mode cask-mode ssh-config-mode crux js-comint company-jedi yaml-mode yaml-tomato basic-mode rainbow-mode flx-ido fennel-mode smex ido-completing-read+ go-mode janet-mode nsis-mode typescript-mode web-mode gruber-darker-theme rc-mode dockerfile-mode try keycast chordpro-mode company-php company-web ctable rustic helpful lsp-java w3m company-quickhelp acme-theme pdf-tools elfeed 0x0 lice indent-guide howdoyou evil-numbers perl-doc ws-butler vterm-toggle vterm eglot lsp-ui lsp-mode rust-mode uxntal-mode magit evil-collection racket-mode all-the-icons undo-tree ligature editorconfig flycheck company evil))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
