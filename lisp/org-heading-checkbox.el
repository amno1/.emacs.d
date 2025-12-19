;;; org-heading-checkbox.el --- A minor mode to fake checkboxes in org-headings.  -*- lexical-binding: t; -*-

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

(defvar ohc--enabled-re "^[ \t]*\\*+.*?[ \t]*\\[x\\]")
(defvar ohc--disabled-re "^[ \t]*\\*+.*?[ \t]*\\[ \\]")
(defvar ohc--checkbox-re "^[ \t]*\\*+.*?\\[[ x]\\]")

(defvar org-heading-checkbox-enabled-hooks nil)
(defvar org-heading-checkbox-disabled-hooks nil)

(defvar ohc--mode-on nil)

(defun ohc--mode-on ()
  (setq ohc--mode-on t))

(defun ohc--mode-off ()
  (setq ohc--mode-on nil))

(defun ohc--heading-checkbox-p ()
  "Return t if this is a heading with a checkbox."
  (save-excursion
    (beginning-of-line)
    (looking-at ohc--checkbox-re)))

(defun ohc--checkbox-enabled-p ()
  "Return t if point is at a heading with an enabed checkbox."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\\*+.*?\\[x\\]")))

(defun ohc--checkbox-disabled-p ()
  "Return t if point is at a heading with a disabeled checkbox."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\\*+.*?\\[ \\]")))

(defun ohc--checkbox-enable ()
  "Disable checkbox for heading at point."
  (interactive)
  (when (ohc--checkbox-enabled-p)
    (save-excursion
      (beginning-of-line)
      (replace-string "[ ]" "[x]" nil (line-beginning-position)
                      (line-end-position))
      (run-hooks 'org-heading-checkbox-enabled-hooks))))

(defun ohc--checkbox-disable ()
  "Disable checkbox for heading at point."
  (interactive)
  (when (ohc--checkbox-enabled-p)
    (save-excursion
      (beginning-of-line)
      (replace-string "[x]" "[ ]" nil (line-beginning-position)
                      (line-end-position))
      (run-hooks 'org-heading-disabled-hooks))))

(defun ohc--checkbox-toggle ()
  "Toggle state of checkbox at heading under the point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at ohc--enabled-re)
           (replace-string "[x]" "[ ]" nil (line-beginning-position)
                           (line-end-position))
           (run-hooks 'org-heading-checkbox-disabled-hooks))
          ((looking-at ohc--disabled-re)
           (replace-string "[ ]" "[x]" nil (line-beginning-position)
                           (line-end-position))
           (run-hooks 'org-heading-checkbox-enabled-hooks))
          (t (error "Not at org-heading-checkbox line.")))))

(defun ohc--shiftup ()
  (interactive)
  (if (ohc--heading-checkbox-p)
      (ohc--checkbox-toggle)
    (org-shiftup)))

(defun ohc--shiftdown ()
  (interactive)
  (if (ohc--heading-checkbox-p)
      (ohc--checkbox-toggle)
    (org-shiftdown)))

(defvar org-heading-checkbox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [S-up] #'ohc--shiftup)
    (define-key map [S-down] #'ohc--shiftdown)
    map)
  "Keymap used in `org-init-mode'.")

;;;###autoload
(define-minor-mode org-heading-checkbox-mode
  ""
  :global nil :lighter nil
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode."))
  (cond (org-heading-checkbox-mode
         (ohc--mode-on))
        (t (ohc--mode-off))))

(provide 'org-heading-checkbox)
;;; org-heading-checkbox.el ends here
