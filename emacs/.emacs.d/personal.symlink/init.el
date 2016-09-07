;;; init.el --- Settings: Provides settings
;;; Commentary:
;;; Code:


;; Enable desktop save mode
(desktop-save-mode 1)

;; Automatically start server
(require 'server)
(unless (server-running-p) (server-start))


;;-----------------------------------------------------------------------------
;; Load paths
;;-----------------------------------------------------------------------------

;; Add Homebrew to the load path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; MIT Scheme
(setenv "MITSCHEME_LIBRARY_PATH"
        "/Applications/MIT-Scheme.app/Contents/Resources")


;;-----------------------------------------------------------------------------
;; User
;;-----------------------------------------------------------------------------

;; User
(setq-default user-full-name    "Inge Jørgensen"
              user-mail-address "inge@elektronaut.no")


;;-----------------------------------------------------------------------------
;; Look and feel
;;-----------------------------------------------------------------------------

;; Typography
(set-face-attribute 'default        nil :family "Inconsolata" :height 130)
(set-face-attribute 'variable-pitch nil :family "Inconsolata" :height 130)
(setq-default line-spacing 2)


;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")
(load-theme 'atom-one-dark t)
(set-mouse-color "white")

;; Cursor
(setq-default cursor-type 'bar)

;; Frame config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)

;; Input config
(setq-default default-input-method "MacOSX"
              mac-option-modifier nil
              mac-right-option-modifier nil
              mac-control-modifier 'control
              mac-command-modifier 'meta
              x-select-enable-clipboard t
              mouse-wheel-scroll-amount '(0.01))

;; Disable bell when scrolling
(defun inge-bell-function ()
  "Custom bell function."
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                  keyboard-quit mwheel-scroll down up next-line previous-line
                  backward-char forward-char))
    (ding)))
(setq-default ring-bell-function 'inge-bell-function)

;; Frame title
(setq-default frame-title-format
              '("" invocation-name " - "
                (:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         "%b"))))


;;-----------------------------------------------------------------------------
;; Editing
;;-----------------------------------------------------------------------------

;; Whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables '(default-major-mode 'text-mode))


;;-----------------------------------------------------------------------------
;; Built-ins
;;-----------------------------------------------------------------------------

;; Abbreviations
(setq-default abbrev-mode t
              save-abbrevs t)
(read-abbrev-file "~/.emacs.d/abbrev_defs")

;; Calendar
(setq-default calendar-week-start-day 1)

;; Prelude
(setq-default prelude-auto-save nil)

;; Org-mode
(setq-default org-mobile-directory "~/Dropbox/Apps/MobileOrg"
              org-replace-disputed-keys t
              org-src-fontify-natively t)


;;-----------------------------------------------------------------------------
;; Packages
;;-----------------------------------------------------------------------------

(require 'use-package)

(use-package apache-mode
  :init
  (autoload 'apache-mode "apache-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
  (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
  (add-to-list 'auto-mode-alist '("apache\\.conf\\'" . apache-mode))
  (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
  (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
  (add-to-list 'auto-mode-alist
               '("sites-\\(available\\|enabled\\)/" . apache-mode)))

(use-package beacon
  :init
  (beacon-mode +1)
  (setq-default beacon-color "#3E4451"
                beacon-blink-duration 0.2
                beacon-blink-delay 0.05))

(use-package company
  :bind (("<C-tab>" . company-complete)))

(use-package company-emoji
  :init
  (add-to-list 'company-backends 'company-emoji))

(use-package company-web
  :init
  (add-to-list 'company-backends 'company-web))

(use-package delight
  :config
  (delight '((yas-minor-mode nil yasnippet)
             (abbrev-mode nil abbrev)
             (helm-mode nil helm)
             (company-mode nil company)
             (flycheck-mode nil flycheck)
             (flyspell-mode nil flyspell)
             (smartparens-mode nil smartparens)
             (prelude-mode nil prelude-mode)
             (whitespace-mode nil whitespace))))

(use-package dired
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package enh-ruby-mode
  :init
  (setq-default enh-ruby-bounce-deep-indent t
                enh-ruby-hanging-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.prawn$" . enh-ruby-mode))
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-temp-prefix ".flycheck"
                flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                   '(javascript-jshint)))
  (eval-after-load "flycheck"
    '(progn
       (flycheck-add-mode 'javascript-eslint 'web-mode))))

(use-package flyspell
  :init
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))))

(use-package helm
  :init
  (add-to-list 'projectile-globally-ignored-directories "import/site"))

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)))

(use-package magit
  :init
  (setq-default magit-use-overlays nil))

(use-package mu4e
  :init
  (setq-default mu4e-maildir          (expand-file-name "~/Mail/elektronaut")
                mu4e-drafts-folder    "/Drafts"
                mu4e-sent-folder      "/Sent Messages"
                mu4e-trash-folder     "/Trash"
                mu4e-get-mail-command "offlineimap"
                ;;mu4e-sent-messages-behavior 'delete
                mu4e-maildir-shortcuts '(("/"              . ?i)
                                         ("/Sent Messages" . ?s)
                                         ("/Trash"         . ?t))))

(use-package js2-mode
  :init
  (setq-default js-basic-offset 2))

(use-package jsx-mode
  :init
  (setq-default jsx-indent-level 2))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-n"       . mc/mark-next-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package project-explorer
  :bind (("C-c p x" . project-explorer-open)))

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package rbenv
  :init
  (global-rbenv-mode))

(use-package scss-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss.erb" . scss-mode)))

(use-package viking-mode
  :init
  (eval-after-load "viking-mode"
    '(progn (viking-global-mode))))

(use-package web-mode
  :init
  (setq-default web-mode-code-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-script-padding 2
                web-mode-style-padding 2
                web-mode-content-types-alist '(("jsx" . "\\.es6\\'")))
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode)))

(use-package wgrep-ag
  :bind (("C-c p s S" . projectile-ag)))

(use-package yasnippet
  :init
  (yas-global-mode 1))


;;-----------------------------------------------------------------------------
;; Key bindings
;;-----------------------------------------------------------------------------

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "C-M-,") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)
(global-set-key (kbd "C-x w") 'helm-spaces)
(global-set-key (kbd "C-x C-+") 'auto-window-layout)


(provide 'init)
;;; init.el ends here
