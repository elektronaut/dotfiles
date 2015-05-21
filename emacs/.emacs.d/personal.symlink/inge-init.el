;;; inge-init.el --- Settings: Provides settings
;;; Commentary:
;;; Code:

;; Typography
(custom-set-faces '(default ((t (:height 130 :family "Consolas"))))
                  '(variable-pitch ((t (:height 130 :family "Consolas")))))
(setq-default line-spacing 2)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(disable-theme 'zenburn)
(load-theme 'kensho t)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Abbreviations
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)

;; Enable desktop save mode
(desktop-save-mode 1)

;; Automatically start server
(server-start)

;; Prelude
(setq prelude-auto-save nil)

;; Input config
(setq default-input-method "MacOSX")
(setq mac-option-modifier nil
      mac-right-option-modifier nil
      mac-control-modifier 'control
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
(setq mouse-wheel-scroll-amount '(0.01))

;; Disable bell when scrolling
(defun inge-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line previous-line
                                backward-char forward-char))
    (ding)))
(setq ring-bell-function 'inge-bell-function)

;; Org-mode
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Flyspell
(require 'flyspell)
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Window config
;(when window-system (set-frame-size (selected-frame) 190 48))
;(add-to-list 'default-frame-alist '(width . 190))
;(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)

;; Yasnippet
(yas-global-mode 1)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; Join lines
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Ruby
(setq enh-ruby-bounce-deep-indent t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)

;; Rbenv
(require 'rbenv)
(global-rbenv-mode)

;; Web mode
(defun inge/web-mode-hook ()
  "Hooks for web mode."
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'inge/web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))

;; MIT Scheme
(setenv "MITSCHEME_LIBRARY_PATH"  "/Applications/MIT-Scheme.app/Contents/Resources")

;; Ag
(require 'wgrep-ag)
(global-set-key (kbd "C-c p s S") 'projectile-ag)

;; Company
(global-set-key (kbd "<C-tab>") 'company-complete)

;; Project explorer
(global-set-key (kbd "C-c p x") 'project-explorer-open)

;; Calendar
(setq calendar-week-start-day 1)

;; Apache
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("apache\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; Web mode

(defun inge-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2))
(add-hook 'web-mode-hook  'inge-web-mode-hook)

;; SCSS mode
(add-to-list 'auto-mode-alist '("\\.scss.erb" . scss-mode))

;; Toggle comment on region or line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-M-,") 'comment-or-uncomment-region-or-line)

;; Kill buffers
(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun kill-everything ()
  "Kill all buffers and windows."
  (interactive)
  (kill-all-buffers)
  (delete-other-windows))

(defun region-to-hexcol ()
  (interactive)
  (let
      ((start (region-beginning))
       (end (region-end))
       (text))

    (setq text (buffer-substring-no-properties start end))

    (when (string-match "^[[:digit:]]+$" text)
      (setq text (format "%02x" (string-to-number text)))
      (delete-region start end)
      (insert text))))

(defun rgb-to-hex ()
  (interactive)

  (let
      ((start (region-beginning))
       (end (region-end)))

    (goto-char start)
    (set-mark start)
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (skip-chars-forward ", ")
    (set-mark (point))
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (skip-chars-forward ", ")
    (set-mark (point))
    (skip-chars-forward "0-9")
    (region-to-hexcol)

    (setq end (point))
    (goto-char start)

    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "[, ]" nil t) (replace-match "" nil t)))))

(provide 'inge-init)
;;; inge-init.el ends here
