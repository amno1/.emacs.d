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


(require 'sotlisp)
(require 'autoinsert)
(require 'helm-pages)

(defun cl-hooks ()  
  (setq fill-column 80)
  (paredit-mode 1)
  (company-mode 1)
  (outshine-mode 1)
  (yas-minor-mode 1)
  ;;(lisp-extra-font-lock-mode 1)
  (speed-of-thought-mode 1)
  (page-break-lines-mode 1))

(add-to-list 'auto-insert-alist
             '(("\\.cl\\|\\.lisp\\'" . "CommonLisp header")
               "Short description: "
               ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
               (make-string (max 2 (- 80 (current-column) 27)) ?\s)
               "

;; Copyright (C) " (format-time-string "%Y") "  "
               (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
               '(if (search-backward "&" (line-beginning-position) t)
                    (replace-match (capitalize (user-login-name)) t t))
               '(end-of-line 1) " <" (progn user-mail-address) ">
;; Keywords: "
               '(require 'finder)
               ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
               '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
		                 finder-known-keywords)
	              v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
	                            finder-known-keywords
	                            "\n"))
               ((let ((minibuffer-help-form v2))
                  (completing-read "Keyword, C-h: " v1 nil t))
                str ", ")
               & -2 "

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


(provide 'cl-extras)
;;; cl-extrasl.el ends here
