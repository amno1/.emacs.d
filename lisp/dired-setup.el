;;; dired-setup.el --- Setup for Dired buffer   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

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

;; 

;;; Code:

;;;; External
(require 'seq)
(require 'wdired)
(require 'dired-x)
(require 'openwith)
(require 'dired-aux)
(require 'dired-async)
;;(require 'org-view-mode)
(require 'dired-subtree)
(require 'dired-copy-paste)
;;(require 'dired+)
(require 'tmtxt-dired-async)

;;;; Help Functions
;; quick-hack - need to rewrite this
(defun my-run ()
  (interactive)
  (let ((file (expand-file-name (dired-get-file-for-visit))))
    (start-process file file file)))

(defun dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (dired-previous-line 2))

(defun dired-goto-first ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1)
  (dired-move-to-filename))

(defun dired-goto-last ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1)
  (dired-move-to-filename))

(defun dired-dirline-p ()
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at dired-re-dir)))

(defun dired-subtree-expand-tree ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename)
        (when (dired-dirline-p)
          (unless (dired-subtree--is-expanded-p)
            (dired-subtree-insert)
            (dired-prev-dirline 1))))
      (dired-next-dirline 1))))

(defun dired-subtree-expand-all ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (not (bobp))
      (ignore-errors
        (unless (dired-subtree--is-expanded-p)
          (dired-subtree-insert)))
      (dired-previous-line 1))))

(defun dired-subtree-fold-all ()
  (interactive)
  (save-excursion
    (dired-goto-last)
    (while (not (bobp))
      (dired-next-line -1)
      (ignore-errors
        (unless (dired-subtree--is-expanded-p)
          (dired-subtree-remove))))))

(defun dired-in-vertical-split ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (dired-jump))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun dired-open-current-as-sudo ()
  "open current file as sudo"
  (interactive)
  (message "!!! SUDO opening %s"
           (dired-file-name-at-point))
  (sudo-find-file (dired-file-name-at-point)))

(defun dired-disable-line-wrap ()
  "disable line wrapping in dired-mode"
  (auto-fill-mode -1))
(add-hook 'dired-mode-hook 'dired-disable-line-wrap)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
  If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Prevent dired to write to modeline.
;; Built-in version dumps entire content of dired-listing-switches to modeline
;; which pushes everything fat to the right and makes modeline literally
;; worthless. I really don't need ot see ls switches on my modeline, so I have
;; rewrote the function to not dump switches at all.
(defvar dired-display-ls-switches nil
  "Non-nil meands the Dired will display current ls-switches on modeline.")
(defun dired-sort-set-mode-line ()  
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match-p
        	    dired-sort-by-name-regexp dired-actual-switches)
        	   "Dired by name")
        	  ((string-match-p
        	    dired-sort-by-date-regexp dired-actual-switches)
        	   "Dired by date")
        	  ((eq dired-display-ls-switches t)
        	   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))

(when (version< emacs-version "28")
  (defun directory-empty-p (file-name)
    "Check if a directory contains any other files then dot-files"
    (when (file-directory-p file-name)
      (null (directory-files file-name nil
                             directory-files-no-dot-files-regexp t)))))

(defun dired-mark-empty-dirs ()
  "Interactively mark all empty directories in current Dired buffer."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (ignore-errors
          (when (directory-empty-p (dired-get-filename))
            (dired-mark 1)
            (dired-previous-line 1)))
        (dired-next-line 1)))))

(defun dired-delete-empty-dirs ()
  "Interactively mark all empty directories in current Dired buffer."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (ignore-errors
          (when (directory-empty-p (dired-get-filename))
            (delete-directory (dired-get-filename))))
        (dired-next-line 1)))))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-find-dups ()
  (unless (or (eq major-mode 'dired-mode) (eq major-mode 'wdired-mode))
              (error "Not in dired or wdired mode."))
  (goto-char (point-min))
  (let (files dups file)
    (while (not (eobp))
      (dired-next-line 1)
      (setq file (dired-get-filename 'no-dir t))
      (if (and file (seq-contains-p files file))
          (push file dups)
        (push file files)))
    dups))

;;;; Setup

(setq dired-dwim-target t
      global-auto-revert-non-file-buffers nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-listing-switches "-lA --si --time-style=long-iso --group-directories-first"
      wdired-use-vertical-movement t
      wdired-allow-to-change-permissions t
      dired-omit-files-p t
      dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
              
(dolist (ext  (list (list (openwith-make-extension-regexp
                           '("flac" "mpg" "mpeg" "mp3" "mp4"
                             "avi" "wmv" "wav" "mov" "flv" "ts" "m4b"
                             "ogm" "ogg" "mkv" "webm" "m4v" "m4a"))
                          "mpv"
                          '(file))
                    (list (openwith-make-extension-regexp
                           '("html" "htm"))
                          (getenv "BROWSER")
                          '(file))))
  (add-to-list 'openwith-associations ext))

(provide 'dired-setup)
;;; dired-setup.el ends here
