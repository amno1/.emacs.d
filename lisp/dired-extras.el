;;; dired-extras.el ---                              -*- lexical-binding: t; -*-

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

(require 'wdired)
(require 'dired-x)
(require 'openwith)
(require 'dired-aux)
(require 'dired-async)
(require 'dired-copy-paste)
;;(require 'dired+)
;;(require 'tmtxt-dired-async)
;; (require 'dired-show-readme)

(autoload 'dired-subtree-toggle "dired-subtree.el" nil t)
(autoload 'dired-openwith "openwith.el" nil t)

;; quick-hack - need to rewrite this
(defun my-run ()
  (interactive)
  (let ((file (expand-file-name (dired-get-file-for-visit))))
    (start-process file file file)))

(defun dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (dired-previous-line 2))

(defun dired-go-to-first ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1)
  (skip-chars-forward " \n\t"))

(defun dired-go-to-last ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1)
  (skip-chars-forward " \n\t"))

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

(defun dired-is-empty-p (directory-name)
  "Check if a directory contains any other files then dot-files"
  (null (directory-files directory-name nil
                         directory-files-no-dot-files-regexp t)))

(defun dired-mark-empty-dirs ()
  "Interactively mark all empty directories in current Dired buffer."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (let ((curr-file))
      (save-excursion
        (dired-go-to-first)
        (while (not (eobp))
          (setq curr-file (dired-file-name-at-point))
          (when (file-directory-p curr-file)
            (when (dired-is-empty-p curr-file)              
              (dired-mark 1)
              (dired-previous-line 1)))
          (dired-next-line 1))))))


(provide 'dired-extras)
;;; dired-extras.el ends here
