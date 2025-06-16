;;; dired-git-log.el --- Show git info in dired -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Arthur Miller <arthur.miller@live.com>
;; Author: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/amno1/dired-git-log
;; Version: 1.0.0
;; Package-Requires: ((emacs "25"))
;; Keywords: dired, files

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
;; Minor mode which shows last commit message and date (info shown is
;; configurable) for git project files in Dired.
;;
;;; Code:

(eval-and-compile
  (require 'dired)
  (require 'dired-x)
  (require 'dired-aux))

(defgroup dired-git-log nil
  "Show git info in Dired."
  :group 'files
  :prefix "dired-git-log-")

(defface dired-git-log-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message.")

(defcustom dired-git-log-auto-hide-details-p t
  "If details should get hidden automatically.

Uses function `dired-hide-details-mode' to hide details when showing git
info."
  :type 'boolean)

(defcustom dired-git-log-commit-message-format "%s\t%cr"
  "Format of the commit messages.

Entries separated by tabs are aligned.  Some common placeholders
are (see git-log PRETTY FORMATS for all):

           · %H: commit hash

           · %h: abbreviated commit hash

           · %P: parent hashes

           · %p: abbreviated parent hashes

           · %an: author name

           · %ae: author email

           · %cd: committer date

           · %cr: committer date, relative

           · %cn: committer name

           · %ce: committer email

           · %s: subject

           · %f: sanitized subject line, suitable for a filename"
  :type 'string)

(defvar-local dired-git-log--restore-no-details nil
  "If no details view has to be restored.")

(defun dired-git-log--files ()
  "Return list of file names in Dired buffer."
  (save-excursion
    (let (files)
      (goto-char 1)
      (while (not (eobp))
        (when (dired-file-name-at-point)
          (push (expand-file-name (dired-file-name-at-point)) files))
        (dired-next-line 1))
      (nreverse files))))

(defun dired-git-log--get-log ()
  "Insert git log messages into current buffer."
  (let* ((files (dired-git-log--files))
         (msgs (dired-git-log--spacers files))
         (cols (dired-git-log--get-columns)))
    (dolist (col cols)
      (let ((logs (dired-git-log--get-git-col files col)))
        (cond (logs
               (let ((spcrs (dired-git-log--spacers logs)) fmtd)
                 (dolist (log logs)
                   (push (concat (pop msgs) log (pop spcrs)) fmtd))
                 (setq msgs (nreverse fmtd))))
              (t (setq msgs nil)))))
    (when msgs
      (save-excursion
        (with-silent-modifications
          (dolist (file files)
            (dired-goto-file file)
            (when (dired-file-name-at-point)
              (dired-move-to-end-of-filename t)
              (and msgs (= (point) (line-end-position))
                   (insert (if (file-directory-p file) " " "") (pop msgs)))))
          (dired-git-log--fontify-git-log (point-max)))))))

(defun dired-git-log--spacers (lines)
  "Return a list of spaces to align LINES to the longest line in LINES."
  (let ((longest (dired-git-log--longest-line lines))
        spacers)
    (dolist (line lines)
      (push (make-string (1+ (- longest (length line))) ?\s) spacers))
    (nreverse spacers)))

(defun dired-git-log--longest-line (lines)
  "Find longest line length in a list of LINES."
  (let ((longest 0) length)
    (dolist (l lines)
      (setq length (length l))
      (if (> length longest) (setq longest length)))
    longest))

(defun dired-git-log--get-columns ()
  "Return format string split by column."
  (split-string dired-git-log-commit-message-format "\t"))

(defun dired-git-log--get-git-col (files format)
  "Return git log for FILES as format message FORMAT."
  (let ((format (concat "--pretty=" format)) logs ecode)
    (dolist (file files)
      (with-temp-buffer
        (setq ecode (call-process "git" nil t nil "log" "-1" format file))
        (when  (eq ecode 0)
          (if (= 1 (point-max)) (insert "  "))
          (push (buffer-substring-no-properties 1 (1- (point-max))) logs))))
    (nreverse logs)))

(defun dired-git-log--remove-logs ()
  "Renove inserted git log messages."
  (save-excursion
    (with-silent-modifications
      (goto-char 1)
      (while (not (eobp))
        (when (dired-file-name-at-point)
          (dired-move-to-end-of-filename)
          (delete-region (point) (line-end-position)))
        (dired-next-line 1)))))

(defun dired-git-log--after-readin ()
  "Dired-git-log hook called after a Dired buffer is modified."
  (dired-git-log--get-log))

(defun dired-git-log--before-readin ()
  "Dired-git-log hook called before a Dired buffer is modified."
  (when (bound-and-true-p dired-git-log-mode)
    (dired-git-log--remove-logs)))

(defun dired-git-log--revert-buffer (&rest _r)
  "Recalculate git info after buffer is reverted."
  (when (bound-and-true-p dired-git-log-mode)
    (dired-git-log--get-log)))

(defun dired-git-log--fontify-git-log (limit)
  "Fonitfy inserted git info in Dired buffer.

LIMIT as required by font-lock hook."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) limit)
      (when (dired-file-name-at-point)
        (dired-move-to-end-of-filename)
        (when (< (point) (line-end-position))
          (add-text-properties
           (1+ (point)) (line-end-position)
           (list 'font-lock-fontified t
                 'face 'dired-git-log-commit-message-face))))
      (dired-next-line 1))))

(defun dired-git-log--after-mode ()
  (when dired-git-log-auto-hide-details-p
    (unless (bound-and-true-p dired-hide-details-mode)
      (setq dired-git-log--restore-no-details t)
      (dired-hide-details-mode))))

(defun dired-git-log--on-first-change ()
  (dired-git-log--fontify-git-log (point-max)))

(defun dired-git-log--cleanup ()
  "Remove commit messages."
  (dired-git-log--remove-logs)
  (when dired-git-log--restore-no-details
    (setq dired-git-log--restore-no-details nil)
    (dired-hide-details-mode -1))
  (advice-remove 'dired-revert #'dired-git-log--revert-buffer)
  (remove-hook 'dired-before-readin-hook #'dired-git-log--before-readin t)
  (remove-hook 'dired-after-readin-hook #'dired-git-log--after-readin t)
  (remove-hook 'after-change-major-mode-hook #'dired-git-log--after-mode t)
  (remove-hook 'first-change-hook #'dired-git-log--on-first-change t)
  (setq font-lock-keywords
        (remove '(dired-git-log--fontify-git-log) font-lock-keywords)))

;;;###autoload
(define-minor-mode dired-git-log-mode
  "Toggle git message info in current Dired buffer."
  :lighter " dgl"
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (if (locate-dominating-file "." ".git")    
      (cond
       ((not dired-git-log-mode)
        (dired-git-log--cleanup))
       (t
        (font-lock-add-keywords nil '(dired-git-log--fontify-git-log))
        (advice-add 'dired-revert :after #'dired-git-log--revert-buffer)
        (add-hook 'dired-before-readin-hook #'dired-git-log--before-readin nil t)
        (add-hook 'dired-after-readin-hook #'dired-git-log--after-readin nil t)
        (add-hook 'after-change-major-mode-hook #'dired-git-log--after-mode nil t)
        (add-hook 'first-change-hook #'dired-git-log--on-first-change nil t)
        (dired-git-log--get-log)))))

(provide 'dired-git-log)
;;; dired-git-log.el ends here


