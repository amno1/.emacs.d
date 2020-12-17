;;; elisp-setup.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  your name here

;; Author: your name here <your-email-address-here>
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

(defun shell-command-on-buffer ()
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ") ))

;; From: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; https://stackoverflow.com/questions/2171890/emacs-how-to-evaluate-the-smallest-s-expression-the-cursor-is-in-or-the-follow
(defun eval-next-sexp ()
  (interactive)
  (forward-sexp)
  (eval-last-sexp nil))

;; this works sometimes :-)
(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (up-list (abs levels))
  (eval-last-sexp nil))

(provide 'elisp-setup)
;;; elisp-setup.el ends here
