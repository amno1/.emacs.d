;;; elisp-extras.el --- Some extra functions to work with elisp.  -*- lexical-binding: t; -*-

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

;;  FIXME remove popup package requirement (use tooltip.el)

;;; Code:

;;;; Editing

(defun sexp-copy-forward (&optional arg)
  "Save the sexp following point to the kill ring.
ARG has the same meaning as for `kill-sexp'."
  (interactive "p")
  (save-excursion
    (let ((orig-point (point)))
      (forward-sexp (or arg 1))
      (kill-ring-save orig-point (point)))))

(defun sexp-copy-backward (&optional arg)
  "Save the sexp preceding point to the kill ring.
ARG has the same meaning as for `kill-sexp'."
  (interactive "p")
  (save-excursion
    (let ((orig-point (point)))
      (backward-sexp (or arg 1))
      (kill-ring-save (point) orig-point))))


;;;; Evaluation

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
  (save-excursion
    (forward-sexp)
    (eval-last-sexp nil)))

;; this works sometimes
(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

(require 'popup)

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description
          (save-window-excursion
            (with-temp-buffer
              (help-mode)
              (describe-symbol thing)
              (buffer-string)))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun value-at-point-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (value (and (boundp thing) (symbol-value thing))))
    (popup-tip (format "%s" value)
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun last-sexp-in-popup ()
  (interactive)
  (let* ((value (eval-last-sexp nil)))
    (popup-tip (format "%s" value)
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun next-sexp-in-popup ()
  (interactive)
  (let* ((value (eval-next-sexp)))
    (popup-tip (format "%s" value)
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun surrounding-sexp-in-popup ()
  (interactive)
  (let* ((value (eval-surrounding-sexp 1)))
    (popup-tip (format "%s" value)
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(provide 'elisp-extras)
;;; elisp-extras.el ends here
