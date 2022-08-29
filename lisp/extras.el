;;; extras.el --- Some extra extensions  -*- lexical-binding: t -*-
(require 'recentf)

;;;###autoload
;; (defun replace-with-spaces (beg end)
;;   "Replace the region with the equivalent number of spaces."
;;   (interactive "r")
;;   (goto-char beg)
;;   (while (re-search-forward "." end t)
;;     (replace-match " ")))

(defun replace-with-spaces (beg end)
  "Replace the region with the equivalent number of spaces."
  (interactive "r")
  (delete-region beg end)
  (insert-char ?\s (- end beg)))

;; Startup time
;;;###autoload
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time))) gcs-done))

;;;###autoload
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

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

;; https://xenodium.com/emacs-clone-git-repo-from-clipboard/

(defvar git-repository-dirs (expand-file-name "~/repos/"))

(defun git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (unless (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0))
    (error "No URL in clipboard"))
  (let* ((url (current-kill 0))
         (download-dir git-repository-dirs)
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
             (message "c: %s" command)
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          ;;(delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process __state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

;; https://xenodium.com/emacs-quote-wrap-all-in-region/

(defun toggle-quote-wrap-all-in-region (beg end)
  "Toggle wrapping all items in region with double quotes."
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "no region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"" item)))
                              (split-string (buffer-substring beg end)))
                      " ")))
    (delete-region beg end)
    (insert replacement)))

(defun plist-elts (plist elt)
  "Return elements between ELT and next keyword in list PLIST."
  (let ((elts (member elt plist)) ret)
    (catch 'done ret
           (dolist (elt (cdr elts))
             (if (keywordp elt)
                 (throw 'done ret)
               (push elt ret)))
           (throw 'done ret))
    ret))

(defun plist-elt (plist property)
  "Return value of PROPERTY from irregular plist PLIST."
  (cadr (member property plist)))


;;;###autoload
(defun shell-command-on-buffer ()
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ") ))

;;;###autoload
(defun f-to-c ()
  (interactive)
  (let* ((ftemp (read-number "Enter temperature in Fahrenheit: "))
         (ctemp (* (- ftemp 32) 0.56)))
    (message "%s degrees Fahrenheit is %s degrees Celsius." ftemp (float ctemp))
    ctemp))

;;;###autoload
(defun println (listobj &optional buffer)
  "Print list one element per line."
  (let ((buff (or buffer standard-output)))
    (dolist (elt listobj)
      (print elt buff)
      (princ "\n" buff))))

;;;###autoload
(defun longest-line ()
  (let ((l 0) tmp b e)
    (save-excursion
      (goto-char 1)
      (while (not (eobp))
        (setq tmp (- (line-end-position) (line-beginning-position)))
        (if (< l tmp)
            (setq l tmp b (line-beginning-position) e (line-end-position)))
        (forward-line)))
    (cons l (cons b e))))

;;;###autoload
(defun next-buffer-other-window (&optional arg interactive)
  "In other window switch to ARGth next buffer.
Call `switch-to-next-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (next-buffer arg interactive)
    (select-window current)))

;;;###autoload
(defun previous-buffer-other-window (&optional arg interactive)
  "In other window switch to ARGth previous buffer.
Call `switch-to-prev-buffer' unless the selected window is the
minibuffer window or is dedicated to its buffer."
  (interactive "p\np")
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (previous-buffer arg interactive)
    (select-window current)))

;;;###autoload
(defun ff-other-window ()
  "Find file in other window."
  (interactive)
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (cond
     (other
      (select-window other)
      (call-interactively #'find-file)
      (select-window current))
     (t
      (call-interactively #'find-file-other-window)))))

;;;###autoload
(defun kill-buffer-other-window ()
  "Kills buffer in other window."
  (interactive)
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (kill-buffer)
    (select-window current)))

(provide 'extras)
