;;; modeline-el -- Mode line
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Version: 1.0

;;; Commentary:
;;  Based on amitp-mode-line.el by Amit J Patel <amitp@cs.stanford.edu>
;;  http://amitp.blogspot.sg/2011/08/emacs-custom-mode-line.html

;;; Licence: GPL

;;; Code:

(defvar mode-line-selected-window nil)

(defun mode-line-set-selected-window (&rest _)
  "Set selected window."
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq mode-line-selected-window window))))

(add-hook 'window-configuration-change-hook #'mode-line-set-selected-window)
(add-hook 'focus-in-hook #'mode-line-set-selected-window)
(advice-add 'select-window :after 'mode-line-set-selected-window)
(advice-add 'select-frame  :after 'mode-line-set-selected-window)

(defun mode-line-short-buffer-name ()
  "Return only the file name if `buffer-name' is a path."
  (if buffer-file-name
      (car (last (split-string (buffer-name) "/")))
    buffer-name))

(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ;; ; read-only or modified status
   ;; (:eval
   ;;  (cond (buffer-read-only
   ;;         (propertize " RO " 'face 'mode-line-read-only-face))
   ;;        ((buffer-modified-p)
   ;;         (propertize " ** " 'face 'mode-line-modified-face))
   ;;        (t "    ")))
   ;; "  "

   ; Path
   (:eval (propertize (if buffer-file-name (shorten-directory default-directory 10))
                      'face (if (eq (selected-window) mode-line-selected-window)
                                'mode-line-folder-face)))
   ; Filename
   (:eval (propertize
           (mode-line-short-buffer-name)
           'face (cond ((buffer-modified-p)
                        'mode-line-filename-modified-face)
                       ((eq (selected-window) mode-line-selected-window)
                        (if buffer-read-only
                            'mode-line-filename-readonly-face
                          'mode-line-filename-face)))))

   ; narrow [default -- keep?]
   " %n "

   ; Project name
   (:eval (if (projectile-project-name)
              (propertize (format "%s" (projectile-project-name))
                          'face (if (eq (selected-window) mode-line-selected-window) 'mode-line-project-face))))

   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ; nyan-mode uses nyan cat as an alternative to %p
   (:eval (when nyan-mode (list (nyan-create))))
   " %p "
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-filename-modified-face)
(make-face 'mode-line-filename-readonly-face)
(make-face 'mode-line-project-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

;; Atom Dark colors
;; (set-face-attribute 'mode-line nil
;;     :foreground "#ABB2BF" :background "#121417"
;;     :inverse-video nil
;;     :box '(:line-width 6 :color "#121417" :style nil))
;; (set-face-attribute 'mode-line-inactive nil
;;     :foreground "#ABB2BF" :background "#3E4451"
;;     :inverse-video nil
;;     :box '(:line-width 6 :color "#3E4451" :style nil))
;; (set-face-attribute 'mode-line-read-only-face nil
;;     :inherit 'mode-line-face
;;     :foreground "#528BFF"
;;     :box '(:line-width 2 :color "#528BFF"))
;; (set-face-attribute 'mode-line-modified-face nil
;;     :inherit 'mode-line-face
;;     :foreground "#BE5046"
;;     :background "#ffffff"
;;     :box '(:line-width 2 :color "#BE5046"))
;; (set-face-attribute 'mode-line-folder-face nil
;;     :inherit 'mode-line-face
;;     :foreground "#828997")
;; (set-face-attribute 'mode-line-filename-face nil
;;     :inherit 'mode-line-face
;;     :foreground "#E5C07B"
;;     :weight 'bold)
;; (set-face-attribute 'mode-line-position-face nil
;;     :inherit 'mode-line-face
;;     :family "Menlo" :height 100)
;; (set-face-attribute 'mode-line-mode-face nil
;;     :inherit 'mode-line-face
;;     :foreground "#ABB2BF")
;; (set-face-attribute 'mode-line-minor-mode-face nil
;;     :inherit 'mode-line-mode-face
;;     :foreground "#828997"
;;     :height 110)
;; (set-face-attribute 'mode-line-process-face nil
;;     :inherit 'mode-line-face
;;     :foreground "#98C379")
;; (set-face-attribute 'mode-line-80col-face nil
;;     :inherit 'mode-line-position-face
;;     :foreground "black" :background "#E5C07B")

;; Doom colors colors
(defun modeline-setup ()
  "Configure modeline colors."
  (set-face-attribute 'mode-line nil
                      :foreground "#B5BABF" :background "#20272e"
                      :inverse-video nil
                      :box '(:line-width 6 :color "#20272e" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#525E6C" :background "#20272e"
                      :inverse-video nil
                      :box '(:line-width 6 :color "#20272e" :style nil))
  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground "#00B3EF"
                      :box '(:line-width 2 :color "#00B3EF"))
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :foreground "#ff665c"
                      :background "#3D3D48"
                      :box '(:line-width 2 :color "#ff665c"))
  (set-face-attribute 'mode-line-project-face nil
                      :inherit 'mode-line-face
                      :foreground nil)
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground "#eeeeee")
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground "#00B3EF"
                      :weight 'bold)
  (set-face-attribute 'mode-line-filename-modified-face nil
                      :inherit 'mode-line-face
                      :foreground "#E69055"
                      :weight 'bold)
  (set-face-attribute 'mode-line-filename-readonly-face nil
                      :inherit 'mode-line-face
                      :foreground "#ECBE7B"
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :family "Menlo" :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground nil)
  (set-face-attribute 'mode-line-minor-mode-face nil
                      ;;:inherit 'mode-line-mode-face
                      :height 110)
  (set-face-attribute 'mode-line-process-face nil
                      :inherit 'mode-line-face
                      :foreground "#7bc275")
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground "black" :background "#ECBE7B")
  (set-face-attribute 'rbenv-active-ruby-face nil
                      :inherit 'mode-line-face
                      :foreground nil :weight 'normal)
  )
(modeline-setup)

(provide 'modeline)
;;; inge-modeline.el ends here
