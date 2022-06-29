;;; dired-copy-paste.el --- cut/copy/paste files and directories in dired mode

;; Copyrigth (C) 2011 Hidaka Uchida

;; Author: Hidaka Uchida <hidaka.uchida@gmail.com>
;; Version: 0.1
;; Created: Jan 8 2011
;; Keywords; dired, cut, copy, paste

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:
;;
;; dired-copy-paste.el enables you to cut/copy/paste files and directries in dired-mode.

;;; Install:
;;
;; Put this file into your load-path and add the
;; following expression to your ~/.emacs.
;;
;; (require 'dired-copy-paste)

;;; Usage:
;;
;; In dired-mode,
;;
;;  M-x dired-copy-paste-do-cut   <C-c C-w>: Cut a file/dir on current line or all marked file/dir(s).
;;  M-x dired-copy-paste-do-copy  <C-c C-c>: Copy a file/dir on current line or all marked file/dir(s).
;;  M-x dired-copy-paste-do-paste <C-c C-y>: Paste cut/copied file/dir(s) into current directory.
;;
;; You can customize default key bindings as follows.
;;
;; (define-key dired-mode-map "\C-c\C-x" 'dired-copy-paste-do-cut)
;; (define-key dired-mode-map "\C-c\C-c" 'dired-copy-paste-do-copy)
;; (define-key dired-mode-map "\C-c\C-v" 'dired-copy-paste-do-paste)

;;; Code:

(require 'dired)

(define-key dired-mode-map "\C-c\C-w" 'dired-copy-paste-do-cut)
(define-key dired-mode-map "\C-c\C-c" 'dired-copy-paste-do-copy)
(define-key dired-mode-map "\C-c\C-y" 'dired-copy-paste-do-paste)

(defvar dired-copy-paste-func nil)
(defvar dired-copy-paste-stored-file-list nil)


(defun dired-copy-paste-do-cut ()
  "In dired-mode, cut a file/dir on current line or all marked file/dir(s)."
  (interactive)
  (setq dired-copy-paste-stored-file-list (dired-get-marked-files)
        dired-copy-paste-func 'rename-file)
  (message
   (format "%S is/are cut."dired-copy-paste-stored-file-list)))


(defun dired-copy-paste-do-copy ()
  "In dired-mode, copy a file/dir on current line or all marked file/dir(s)."
  (interactive)
  (setq dired-copy-paste-stored-file-list (dired-get-marked-files)
        dired-copy-paste-func 'dired-copy-file)
  (message
   (format "%S is/are copied."dired-copy-paste-stored-file-list)))


(defun dired-copy-paste-do-paste ()
  "In dired-mode, paste cut/copied file/dir(s) into current directory."
  (interactive)
  (let ((stored-file-list nil))
    (dolist (stored-file dired-copy-paste-stored-file-list)
      (condition-case nil
          (progn
            (funcall dired-copy-paste-func stored-file (dired-current-directory) 1)
            (push stored-file stored-file-list))
        (error nil)))
    (if (eq dired-copy-paste-func 'rename-file)
        (setq dired-copy-paste-stored-file-list nil
              dired-copy-paste-func nil))
    (revert-buffer)
    (message
     (format "%d file/dir(s) pasted into current directory." (length stored-file-list)))))


(provide 'dired-copy-paste)
