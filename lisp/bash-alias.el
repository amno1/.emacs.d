;;; bash-alias.el --- Use Bash aliases in interactive shell-comands  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Arthur Miller

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

;; Bound this to M-! as the replacement for shell-command if you use Bash as the
;; shell.

;;; Code:

(defgroup bash-alias nil
  "Shell-command that understands Bash aliases"
  :group 'tools)

(defvar bash-alias-table (make-hash-table :test 'equal)
  "Table containg names and values for Bash aliases.")

(defvar bash-alias-file
  (expand-file-name "etc/bash-alias-table" user-emacs-directory)
  "Serialize Bash aliases to this file.")

(defun bash-aliases ()
  "Obtain list of bash aliases for the current user."
  (interactive)
  (let ((shell-command-switch "-ic") beg)
    (with-temp-buffer
      (shell-command "alias" t)
      (goto-char 1)
      (switch-to-buffer (current-buffer))
      (while (search-forward "alias" nil t)
        (setq beg (point))
        (search-forward "=" (line-end-position)) ;; this has to succeed
        (replace-match " ")
        (let ((name (string-trim (buffer-substring-no-properties
                                  beg (1- (point)))))
              (beg (1+ (point))))
          (while (and (char-after)
                      (/= (char-before) ?\')
                      (/= (char-before) ?\"))
            (forward-char))
          (goto-char (line-end-position))
          (while (and (char-before)
                      (/= (char-before) ?\')
                      (/= (char-before) ?\"))
            (backward-char))
          (puthash name (buffer-substring-no-properties beg (1- (point)))
                   bash-alias-table)))
        (erase-buffer)
        (prin1 bash-alias-table (current-buffer))
        (write-region (point-min) (point-max) bash-alias-file))))

(defun bash-command-from-alias (cmd)
  "Convert a command in CMD with alias into real command name."
  (with-temp-buffer
    (insert cmd)
    (goto-char 1)
    (let ((alias (gethash (current-word) bash-alias-table)))
      (if (not alias)
          cmd
        (delete-region 1 (length cmd))
        (insert alias)
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun shell-command-with-aliases ()
  "Like `shell-command' but understands Bash aliases."
  (interactive)
  (let ((args (eval (cadr (interactive-form 'shell-command)))))
    (apply #'shell-command (bash-command-from-alias (pop args)) args)))

;;;###autoload
(defun async-shell-command-with-aliases ()
  "Like `shell-command' but understands Bash aliases."
  (interactive)
  (let ((args (eval (cadr (interactive-form 'shell-command)))))
    (apply #'async-shell-command (bash-command-from-alias (pop args)) args)))



;;;###autoload
(defun dired-smart-shell-command-with-aliases ()
  "Like `dired-smart-shell-command' but understands Bash aliases."
  (interactive)
  (unless (bound-and-true-p dired-mode)
    (user-error "This command runs only in Dired-mode."))
  (let ((args (eval (cadr (interactive-form 'dired-smart-shell-command)))))
    (apply #'dired-smart-shell-command (bash-command-from-alias (pop args)) args)))

(defvar bash-alias-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap shell-command] #'shell-command-with-aliases)
    (define-key map [remap async-shell-command] #'async-shell-command-with-aliases)
    map))

(defun bash-alias--onload-hook ()
  "Hook to run in eval-after-load."
  (define-key dired-mode-map [remap dired-smart-shell-command]
              #'dired-smart-shell-command-with-aliases))

;;;###autoload
(define-minor-mode bash-alias-mode
  "Enable Bash aliases in shell-command and async-shell-command"
  :global t :lighter " alias")

(defun bash-alias-mode-on ()
  "Init bash-alias mode."
  (unless (file-exists-p bash-alias-file)
    (bash-aliases))
  (with-temp-buffer
    (insert-file-contents bash-alias-file)
    (setq bash-alias-table (read (current-buffer))))
  ;; install ourselves into dired-mode-map when dired is loaded
  (when (bound-and-true-p dired-mode)
    (define-key dired-mode-map [remap dired-smart-shell-command]
                #'dired-smart-shell-command-with-aliases))
  ;; unless dired is not loaded, install ourselves in the future
  (with-eval-after-load 'dired
    (define-key dired-mode-map [remap dired-smart-shell-command]
                #'dired-smart-shell-command-with-aliases)))

(defun bash-alias-mode-off ()
  "Turn off bash alias mode."
  ;; install ourselves into dired-mode-map when dired is loaded
  (when (bound-and-true-p dired-mode)
    (define-key dired-mode-map [remap dired-smart-shell-command] nil))
  ;; unless dired is not loaded, install ourselves in the future
  (dolist (elt after-load-alist)
    (cond
     ((stringp (car elt))
      (when (string-match-p "dired"))))))

(provide 'bash-alias)
;;; bash-alias.el ends here

