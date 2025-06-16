;;; extras.el --- Some extra extensions  -*- lexical-binding: t -*-
;;(require 'corfu)
(require 'recentf)
(require 'windmove)
(require 'ielm)

(defun ielm-use-current-buffer (fn &rest args)
  (let ((working-buffer (current-buffer)))
    (funcall fn args)
    (ielm-change-working-buffer working-buffer)))

(defun other-window-buffer (count &optional all-frames interactive)
  "Select another window in cyclic ordering of windows.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is
positive, skip COUNT windows forwards.  If COUNT is negative,
skip -COUNT windows backwards.  COUNT zero means do not skip any
window, so select the selected window.  In an interactive call,
COUNT is the numeric prefix argument.  Return nil.

If the `other-window' parameter of the selected window is a
function and `ignore-window-parameters' is nil, call that
function with the arguments COUNT and ALL-FRAMES.

This function does not select a window whose `no-other-window'
window parameter is non-nil.

This function uses `next-window' for finding the window to
select.  The argument ALL-FRAMES has the same meaning as in
`next-window', but the MINIBUF argument of `next-window' is
always effectively nil."
  (interactive "p\ni\np")
  (let* ((window (selected-window))
         (original-window window)
	 (function (and (not ignore-window-parameters)
			(window-parameter window 'other-window)))
	 old-window old-count)
    (if (functionp function)
	(funcall function count all-frames)
      ;; `next-window' and `previous-window' may return a window we are
      ;; not allowed to select.  Hence we need an exit strategy in case
      ;; all windows are non-selectable.
      (catch 'exit
	(while (> count 0)
	  (setq window (next-window window nil all-frames))
	  (cond
	   ((eq window old-window)
	    (when (= count old-count)
	      ;; Keep out of infinite loops.  When COUNT has not changed
	      ;; since we last looked at `window' we're probably in one.
	      (throw 'exit nil)))
	   ((window-parameter window 'no-other-window)
	    (unless old-window
	      ;; The first non-selectable window `next-window' got us:
	      ;; Remember it and the current value of COUNT.
	      (setq old-window window)
	      (setq old-count count)))
	   (t
	    (setq count (1- count)))))
	(while (< count 0)
	  (setq window (previous-window window nil all-frames))
	  (cond
	   ((eq window old-window)
	    (when (= count old-count)
	      ;; Keep out of infinite loops.  When COUNT has not changed
	      ;; since we last looked at `window' we're probably in one.
	      (throw 'exit nil)))
	   ((window-parameter window 'no-other-window)
	    (unless old-window
	      ;; The first non-selectable window `previous-window' got
	      ;; us: Remember it and the current value of COUNT.
	      (setq old-window window)
	      (setq old-count count)))
	   (t
	    (setq count (1+ count)))))
        (when (and (eq window original-window)
                   interactive
                   (not (or executing-kbd-macro noninteractive)))
          (message "No other window to select"))
        ;; return the buffer
	(window-buffer window)))))

;; This was constantly throwing into debugger in latest versions because of
;; wrong argument nil; fix: change argument from &rest -> &optional
;;;###autoload
(defun ignore-preserving-kill-region (&optional _)
  "Like `ignore', but don't overwrite `last-event' if it's `kill-region'."
  (declare (completion ignore))
  (interactive)
  (when (eq last-command 'kill-region)
    (setq this-command 'kill-region))
  nil)

;;;###autoload
(defun replace-with-spaces (beg end)
  "Replace the region with the equivalent number of spaces."
  (interactive "r")
  (delete-region beg end)
  (insert-char ?\s (- end beg)))

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
(defvar not-to-kill-buffer-list '("*scratch*" "#emacs" "*Messages*"))

;;;###autoload
(defun kill-buffer-but-not-some ()
  (interactive)
  (if (member (buffer-name (current-buffer)) not-to-kill-buffer-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

;; https://xenodium.com/emacs-clone-git-repo-from-clipboard/
;; hacked by me - no deleting, do pull instead
(defvar git-repository-dirs (expand-file-name "~/repos/"))
(defun git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (unless (string-match-p "^\\(http\\|https\\|ssh\\|git\\)://" (current-kill 0))
    (error "No Git URL in clipboard"))
  (let* ((url (current-kill 0))
         (download-dir git-repository-dirs)
         (project-dir (expand-file-name
                       (car (last (split-string url "/" t "/"))) download-dir))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (if (file-exists-p project-dir)
        (if (y-or-n-p (format "%s exists. Pull instead?" (file-name-base url)))          
            (setq command "git pull" default-directory project-dir)
          (user-error "Aborted")))
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (switch-to-buffer buffer)
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
  (cond
   ((one-window-p t)
    (call-interactively #'find-file-other-window))
   (t
    (let ((other (other-window-for-scrolling))
          (current (selected-window)))
      (select-window other)
      (call-interactively #'find-file)
      (select-window current)))))

;;;###autoload
(defun xref-find-definitions-other-pane (identifier)
  "Like `xref-find-definitions-other-window' but only create new window if there
is no `other-window' to switch to."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (cond
   ((one-window-p t)
    (xref-find-definitions-other-window identifier))
   (t
    (let ((other (other-window-for-scrolling))
          (current (selected-window)))
      (xref-find-definitions identifier)
      (select-window other)
      (select-window current)))))

(defun mp-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window)))))

;;;###autoload
(defun kill-buffer-other-window ()
  "Kills buffer in other window."
  (interactive)
  (let ((other (other-window-for-scrolling))
        (current (selected-window)))
    (select-window other)
    (kill-buffer)
    (select-window current)))

;; search in other-window by "karthink"
;; https://www.reddit.com/r/emacs/comments/x0r0pe/share_your_otherwindow_commands/
;;;###autoload
(defun isearch-forward-other-buffer (prefix)
  "Function to isearch-forward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-window-excursion
      (let ((next (if prefix -1 1)))
        (other-window next)
        (isearch-forward)
        (other-window (- next))))))

;;;###autoload
(defun isearch-backward-other-buffer (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-window-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

;; do shit to other buffers by dbqpdb
;; https://www.reddit.com/r/emacs/comments/x0r0pe/share_your_otherwindow_commands/
(defvar other-prefix-ret)

(defun other-pre-hook ()
  "Hook to move to other window before executing command."
  (setq other-prefix-ret (selected-window))
  (if (one-window-p) (funcall split-window-preferred-function))
  (other-window 1)
  (remove-hook 'pre-command-hook 'other-pre-hook))

(defun other-pre-hook-w-buffer ()
  "Hook to move to other window before executing command."
  (setq other-prefix-ret (selected-window))
  (let ((cur (current-buffer)))
    (if (one-window-p) (funcall split-window-preferred-function))
    (other-window 1)
    (set-window-buffer (selected-window) cur)
    (other-window 0)
    (remove-hook 'pre-command-hook 'other-pre-hook-w-buffer)))

(defun other-post-hook ()
  "Hook to move to other window after executing command."
  (and (not (minibufferp (current-buffer)))
       (boundp 'other-prefix-ret) other-prefix-ret
       (progn
         (select-window other-prefix-ret)
         (setq other-prefix-ret nil)
         (remove-hook 'post-command-hook 'other-post-hook)) () ))

;;;###autoload
(defun do-in-other-window ()
  "Executes next command in other window."
  (interactive)
  (setq other-prefix-ret nil)
  (add-hook 'pre-command-hook 'other-pre-hook)
  (add-hook 'post-command-hook 'other-post-hook))

;;;###autoload
(defun do-to-this-and-stay-in-other-window ()
  "Functions as a prefix to execute next command in other window."
  (interactive)
  (setq other-prefix-ret nil)
  (add-hook 'pre-command-hook 'other-pre-hook-w-buffer))

;;;###autoload
(defun do-to-this-in-other-window ()
  "Functions as a prefix to execute next command in other window."
  (interactive)
  (setq other-prefix-ret nil)
  (add-hook 'pre-command-hook 'other-pre-hook-w-buffer)
  (add-hook 'post-command-hook 'other-post-hook))

(defun send-buffer-to-side (side)
  "Send `current-buffer' to window on SIDE from the `current-window'.

If there is not a window to the left, open new one."
  (let ((wnd (or (windmove-find-other-window side)
                 (split-window nil nil side))))
    (set-window-buffer wnd (current-buffer))
      (previous-buffer)))

;;;###autoload
(defun send-to-window-up ()
  "Send `current-buffer' to window at the top from the `current-window'.

If there is not a window at the top, open new one."
  (interactive)
  (send-buffer-to-side 'above))

;;;###autoload
(defun send-to-window-down ()
  "Send `current-buffer' to window at the bottom from the `current-window'.

If there is not a window at the bottom, open new one."
  (interactive)
  (send-buffer-to-side 'below))

;;;###autoload
(defun send-to-window-left ()
  "Send `current-buffer' to window left from the `current-window'.

If there is not a window to the left, open new one."
  (interactive)
  (send-buffer-to-side 'left))

;;;###autoload
(defun send-to-window-right ()
  "Send `current-buffer' to window right from the `current-window'.

If there is not a window to the right, open new one."
  (interactive)
  (send-buffer-to-side 'right))

;;;###autoload
(defun last-buffer ()
  "Switch to last used buffer."
  (interactive)
  (switch-to-buffer nil))

;;;###autoload
(defun my-helm-next-source ()
  (interactive)
  (helm-next-source)
  (helm-next-line))

;;;###autoload
(defun erase-read-only-buffer ()
  "Interactively erase a read-only buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;###autoload
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p helm-alive-p)
              (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))

;;;###autoload
(defun import-rsa-key (&optional beg end)
  "Import RSA key from region or under the cursor."
  (interactive "r")
  (require 'package)
  (let* ((dir (expand-file-name "gnupg" package-user-dir))
         (beg (or beg (save-excursion (1+ (re-search-backward "[[:blank:]]")))))
         (end (or end (save-excursion (1- (re-search-forward "[[:blank:]]")))))
         (key (buffer-substring-no-properties beg end))
         (shell-command-switch "-c"))
    (let ((default-directory dir))
      (shell-command-on-region
       beg end (format "gpg --homedir %s --receive-keys %s" "" key)))))


(provide 'extras)
