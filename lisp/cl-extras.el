;;; cl-extras.el --- CommonLisp extras              -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'sly)
(require 'clede)
(require 'clede-sly)
(require 'clede-asdf)
(require 'clede-fiveam)
(require 'sotlisp)
(require 'autoinsert)
(require 'helm-pages)

(defun cl-hooks ()  
  (setq fill-column 80)
  (paredit-mode 1)
  ;;(company-mode 1)
  ;;  (outshine-mode 1)
  (yas-minor-mode 1)
  ;;(lisp-extra-font-lock-mode 1)
  (speed-of-thought-mode 1)
  (page-break-lines-mode 1))

(setq sly-lisp-implementations
  '((sbcl-patched ("~/repos/sbcl-fasteval/run-sbcl.sh" "--noinform"))
    (sbcl-patched ("~/repos/sbcl-statat/run-sbcl.sh" "--noinform"))
    (sbcl-clean ("/usr/bin/sbcl" "--noinform"))))

(add-to-list 'auto-insert-alist
             '(("\\.cl\\|\\.lisp\\'" . "CommonLisp header")
               ""
               ";; " (file-name-nondirectory (buffer-file-name))

               \n

               ";; Copyright (C) " (format-time-string "%Y") "  "
               (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
               '(if (search-backward "&" (line-beginning-position) t)
                    (replace-match (capitalize (user-login-name)) t t))
               '(end-of-line 1) " <" (progn user-mail-address) ">
;; Keywords: ""

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



""
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n"))


;;; A template for ASDF system files:
;; https://www.emacswiki.org/emacs/auto-insert-for-asdf
(push `(("\\.asd\\'" . "ASDF Skeleton") 
	"System Name: "
	"
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :" str ".system)
    (defpackage :" str ".system
      (:use :common-lisp :asdf))))


(in-package :" str ".system)

(defsystem :" str " 
  :description " ?\" (read-string "Description: ") ?\"" 
  :author \"" (user-full-name) " <" user-mail-address ">\" 
  :licence \"" (read-string "License: ") "\" 
  :version \"" (read-string "Version: ") "\" 
  :components (()) 
  :depends-on ())") 
  auto-insert-alist) 

;;;###autoload
(defun new-cl-scratch ()
  "Create new scratch buffer in Common Lisp major mode."
  (interactive)
  (with-current-buffer  (get-buffer-create "*cl-scratch*")
    (common-lisp-mode)))

(defun find-systems-in-buffer (&optional asdf-buffer)
  "Find all systems in ASDF-BUFFER.
If not specified, ASDF-BUFFER defaults to current buffer."
  (with-current-buffer (get-buffer (or asdf-buffer (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (let (systems)
        (while
            (condition-case _
                (progn
                  (down-list)
                  (when (eq 'defsystem (read (current-buffer)))
                    (push (read (current-buffer)) systems))
                  (up-list)
                  t)
              (scan-error nil)))
        (nreverse systems)))))

(defun find-systems-in-file (asdf-file)
  "Find all systems in ASDF-FILE."
  (with-temp-buffer
    (insert-file-contents-literally asdf-file)
    (find-systems-in-buffer)))

(defun sly-start-connects ()
  "New connection with one of compilers specified in `sly-lisp-implementations'"
  (interactive)
  (let* ((ask (completing-read "Choose compiler: " sly-lisp-implementations))
         (opt (cadr (assoc (intern-soft ask) sly-lisp-implementations)))
         (cmd (expand-file-name (car opt)))
         (args (cdr opt)))
    (apply #'sly-start `(:program ,cmd :program-args ,args))))

;;;###autoload
(defun sly-new-from-system (&optional dir)
  "Load .asd file in current folder."
  (interactive)
  (let* ((dir (or dir default-directory))
         (asd-path
          (or (and buffer-file-name
                   (equal "asd" (file-name-extension buffer-file-name))
                   buffer-file-name)
              (read-file-name "Choose asdf-file: " (expand-file-name dir))))
         (default-directory (file-name-directory asd-path)))
    (find-file asd-path)
    (sly)
    (while (not (sly-connected-p)) (sleep-for 0.1)) ; is there a better way?
    (call-interactively #'sly-compile-and-load-file)))

(provide 'cl-extras)
;;; cl-extrasl.el ends here
