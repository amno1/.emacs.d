;;; extras.el --- Some extra extensions  -*- lexical-binding: t -*-
(require 'recentf)

;; Startup time
;;;###autoload
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time))) gcs-done))

;;;###autoload
(defun only-current-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun change-font-height (delta)
  (set-face-attribute 'default
                      (selected-frame)
                      :height (+ (face-attribute 'default :height) delta)))

;;;###autoload
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
   Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

;; from emacs-wiki @ https://www.emacswiki.org/emacs/RecentFiles

;;;###autoload
(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

;;;###autoload
(defun enlarge-window-vertically (delta)
  "Make selected window DELTA columns wider.
Interactively, if no argument is given, make selected window one
column wider."
  (interactive "p")
  (enlarge-window delta nil))

;;;###autoload
(defun shrink-window-vertically (delta)
  "Make selected window DELTA columns narrower.
Interactively, if no argument is given, make selected window one
column narrower."
  (interactive "p")
  (shrink-window delta nil))

;;;###autoload
(defun kill-window-left()
  "Kills window on the left side of current window."
  (interactive)
  (delete-window (window-in-direction 'left)))

;;;###autoload
(defun kill-window-right()
  "Kills window on the right side of current window."
  (interactive)
  (delete-window (window-in-direction 'right)))

(defun kill-window-above()
  "Kills window above current window."
  (interactive)
  (delete-window (window-in-direction 'above)))

;;;###autoload
(defun kill-window-below()
  "Kills window below current window."
  (interactive)
  (delete-window (window-in-direction 'below)))

;;;###autoload
(defun kill-buffer-other-window ()
  "Kills buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

;;;###autoload
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;;;###autoload
(defun reindent-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

;; you can modify that list, to fit your needs
;; from emacs-wiki: https://www.emacswiki.org/emacs/KillingBuffers
(setq not-to-kill-buffer-list '("*scratch*" "#emacs" "*Messages*"))

;;;###autoload
(defun kill-buffer-but-not-some ()
  (interactive)
  (if (member (buffer-name (current-buffer)) not-to-kill-buffer-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(provide 'extras)
