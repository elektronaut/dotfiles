;;; inge-init.el --- Settings: Provides settings
;;; Commentary:
;;; Code:

;; Typography
(custom-set-faces '(default ((t (:height 130 :family "Consolas")))))
(setq-default line-spacing 2)

;; Theme
(disable-theme 'zenburn)
(load-theme 'sanityinc-tomorrow-night t)

;; Whitespace
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Abbreviations
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)

;; Enable desktop save mode
(desktop-save-mode 1)

;; Prelude
(setq prelude-auto-save nil)

;; Input config
(setq default-input-method "MacOSX")
(setq mac-option-modifier nil
      mac-right-option-modifier nil
      mac-control-modifier 'control
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Flyspell
(require 'flyspell)
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Window config
(when window-system (set-frame-size (selected-frame) 190 48))
(add-to-list 'default-frame-alist '(width . 190))
(add-to-list 'default-frame-alist '(height . 48))
(scroll-bar-mode -1)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; Join lines
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Rbenv
(require 'rbenv)
(global-rbenv-mode)

;; Ag
(require 'wgrep-ag)
(defun projectile-helm-ag ()
  "Search project with ag and display results in helm."
  (interactive)
  (helm-ag (projectile-project-root)))
(global-set-key (kbd "C-c p s S") 'projectile-helm-ag)

;; Project explorer
(global-set-key (kbd "C-c p x") 'project-explorer-open)

;; Calendar
(setq calendar-week-start-day 1)

;; Kill buffers
(defun kill-all-buffers ()
  "Kill all buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers ()
  "Kill all other buffers"
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))


(provide 'inge-init)
;;; inge-init.el ends here
