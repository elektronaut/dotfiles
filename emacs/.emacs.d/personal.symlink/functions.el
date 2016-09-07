;;; functions.el --- Functions: Provides functions
;;; Commentary:
;;; Code:

(defun split-n (n)
  "Split frame in N windows."
  (let ((split-count (- n (count-windows))))
    (if (> split-count 0) (dotimes (i split-count) (split-window-right)))
    (if (< split-count 0) (dotimes (i (abs split-count))
                            (other-window -1)
                            (delete-window)
                            (other-window 1)))
    (unless (eq split-count 0) (balance-windows))))

(defun split-2 () "Split frame in 2 windows." (interactive) (split-n 2))
(defun split-3 () "Split frame in 3 windows." (interactive) (split-n 3))

(defun auto-window-layout ()
  "Automatically layout frame in 2 or 3 windows depending on size."
  (interactive)
  (if (> (frame-width) (* 3 84))
      (split-n 3)
      (split-n 2))
  (balance-windows))

(define-minor-mode big-font-mode
  "Enable big fonts."
  :global t
  :lighter " big"
  :after-hook
  (let ((adjust (if big-font-mode 20 -20)))
    (set-face-attribute 'default nil
                        :height (+ adjust
                                   (face-attribute 'default :height)))
    (set-face-attribute 'variable-pitch nil
                        :height (+ adjust
                                   (face-attribute 'variable-pitch :height)))))

;; Toggle comment on region or line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

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

(provide 'functions)
;;; functions.el ends here
