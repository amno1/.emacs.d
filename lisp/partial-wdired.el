;;; partial-wdired.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Enable editing of file name and properties only at the point.

;;; Code:

(require 'wdired)

;;;###autoload
(defun wdired-change-to-wdired-mode ()
  "Put a Dired buffer in Writable Dired (WDired) mode.
\\<wdired-mode-map>
In WDired mode, you can edit the names of the files in the
buffer, the target of the links, and the permission bits of the
files.  After typing \\[wdired-finish-edit], Emacs modifies the files and
directories to reflect your edits.

See `wdired-mode'."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (error "Not a Dired buffer"))
  (setq-local wdired-old-content
              (buffer-substring (point-min) (point-max)))
  (setq-local wdired-old-marks
              (dired-remember-marks (point-min) (point-max)))
  (setq-local wdired-old-point (point))
  (setq-local query-replace-skip-read-only t)
  (add-function :after-while (local 'isearch-filter-predicate)
                #'wdired-isearch-filter-read-only)
  (use-local-map wdired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only nil)
  (dired-unadvertise default-directory)
  (add-hook 'kill-buffer-hook #'wdired-check-kill-buffer nil t)
  (add-hook 'before-change-functions #'wdired--before-change-fn nil t)
  (add-hook 'after-change-functions #'wdired--restore-properties nil t)
  (setq major-mode 'wdired-mode)
  (setq mode-name "Editable Dired")
  (setq revert-buffer-function 'wdired-revert)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (run-mode-hooks 'wdired-mode-hook)
  (message "%s" (substitute-command-keys
		 "Press \\[wdired-finish-edit] when finished \
or \\[wdired-abort-changes] to abort changes")))

(defun wdired--before-change-fn (beg end)
  (save-excursion
    ;; make sure to process at least entire line
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    
    (while (< beg end)
      (unless (get-text-property beg 'front-sticky)
        (put-text-property beg (1+ beg) 'front-sticky t)
        (wdired--preprocess-files beg end)
        (when wdired-allow-to-change-permissions
          (wdired--preprocess-perms beg end))
        (when (fboundp 'make-symbolic-link)
          (wdired--preprocess-symlinks beg end)))
      (forward-line)
      (setq beg (point)))
    ;; is this good enough? assumes no extra white lines from dired
    (put-text-property (1- (point-max)) (point-max) 'read-only t)))

;; Protect the buffer so only the filenames can be changed, and put
;; properties so filenames (old and new) can be easily found.
(defun wdired--preprocess-files (beg end)
  (save-excursion
    (with-silent-modifications
      (goto-char beg)
      (let ((used-F (dired-check-switches dired-actual-switches "F" "classify"))
	    filename)
	(setq filename (dired-get-filename nil t))
        (when (and filename
		   (not (member (file-name-nondirectory filename) '("." ".."))))
	  (dired-move-to-filename)
	  ;; The rear-nonsticky property below shall ensure that text preceding
	  ;; the filename can't be modified.
	  (add-text-properties
	   (1- (point)) (point) `(old-name ,filename rear-nonsticky (read-only)))
	  (put-text-property beg (point) 'read-only t)
          (dired-move-to-end-of-filename t)
	  (put-text-property (point) (1+ (point)) 'end-name t))
        (when (and used-F (looking-at "[*/@|=>]$")) (forward-char))
        (when (save-excursion
                (and (re-search-backward
                      dired-permission-flags-regexp nil t)
                     (looking-at "l")
                     (search-forward " -> " (line-end-position) t)))
          (goto-char (line-end-position)))))))

;; Put the needed properties to allow the user to change links' targets
(defun wdired--preprocess-symlinks (beg end)
  (save-excursion
    (with-silent-modifications
      (goto-char beg)
      (when (looking-at dired-re-sym)
        (re-search-forward " -> \\(.*\\)$")
	(put-text-property (1- (match-beginning 1))
			   (match-beginning 1) 'old-link
			   (match-string-no-properties 1))
        (put-text-property (match-end 1) (1+ (match-end 1)) 'end-link t)
        (unless wdired-allow-to-redirect-links
          (put-text-property (match-beginning 0)
			     (match-end 1) 'read-only t))))))

(defun wdired--preprocess-perms (beg end)
  (save-excursion
    (with-silent-modifications
        (setq-local wdired-col-perm nil)
        (goto-char beg)
	  (when (and (not (looking-at dired-re-sym))
		     (wdired-get-filename)
		     (re-search-forward dired-re-perms
                                        (line-end-position) 'eol))
	    (let ((begin (match-beginning 0))
		  (end (match-end 0)))
	      (unless wdired-col-perm
	        (setq wdired-col-perm (- (current-column) 9)))
	      (if (eq wdired-allow-to-change-permissions 'advanced)
		  (progn
		    (put-text-property begin end 'read-only nil)
		    ;; make first permission bit writable
		    (put-text-property
		     (1- begin) begin 'rear-nonsticky '(read-only)))
	        ;; avoid that keymap applies to text following permissions
	        (add-text-properties
	         (1+ begin) end
	         `(keymap ,wdired-perm-mode-map rear-nonsticky (keymap))))
	      (put-text-property end (1+ end) 'end-perm t)
	      (put-text-property
	       begin (1+ begin)
               'old-perm (match-string-no-properties 0)))))))

(defun wdired-change-to-dired-mode ()
  "Change the mode back to dired."
  (or (eq major-mode 'wdired-mode)
      (error "Not a Wdired buffer"))
  (let ((inhibit-read-only t))
    (remove-text-properties
     (point-min) (point-max)
     '(front-sticky nil rear-nonsticky nil read-only nil keymap nil)))
  (remove-function (local 'isearch-filter-predicate)
                   #'wdired-isearch-filter-read-only)
  (use-local-map dired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only t)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (dired-advertise)
  (remove-hook 'kill-buffer-hook 'wdired-check-kill-buffer t)
  (remove-hook 'before-change-functions 'wdired--before-change-fn t)
  (remove-hook 'after-change-functions 'wdired--restore-properties t)
  (setq-local revert-buffer-function 'dired-revert))

(defun wdired-abort-changes ()
  "Abort changes and return to dired mode.  "
  (interactive)
  (remove-hook 'before-change-functions 'wdired--before-change-fn t)
  (with-silent-modifications
    (erase-buffer)
    (insert wdired-old-content)
    (goto-char wdired-old-point))
  (wdired-change-to-dired-mode)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (message "Changes aborted"))

(provide 'partial-wdired)
