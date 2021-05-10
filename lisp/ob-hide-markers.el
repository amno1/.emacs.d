;;; ob-hide-markers.el --- Hide or-babel source code markers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: convenience, outlines, tools

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

;; A minor mode to help reduce clutter in org-babel code blocks by
;; hiding/unhiding markers for source code blocks in org-mode.
;;
;; To hide all markers turn on org-hbm-mode by
;;
;;          `M-x org-hbm-mode.'
;;
;; To turn it off execute same command.
;;
;; The mode provides two additional interactive commands.
;;
;; Use `hbm-refresh' if you add new code blocks, copy-paste etc.
;;
;; Alternatively it is possible to turn on/off markers for an individual source
;; code by executing `M-x hbm-toggle-current-block'. It does not require
;; org-hbm-mode to be on, but you will have to call it again to make markers
;; visible again.
;;
;; It is possible to somewhat control the appereance of org-file by customizing
;; the `orh-hbm-hide-marker-line' variable. When this variable is nil, markers
;; will be invisible but the newline character will be left visible resulting in
;; somewhat "fluffier" appereance. Whan the value is set to `t' even newline
;; character will be hidden resulting in more dense and compact code. That might
;; not be for everyone, so set it to your own preference. It is `nil' by
;; default.

;;; Code:
(require 'org)

(defcustom org-babel-hide-markers-line nil
  "If value of this variable is `t', org-hbm mode vill hide also line on which
  source code block markers are, otherwise only markers are hidden leaving an
  empty line."
  :group 'org-babel
  :tag "Org Babel Hide Source Block Markers Line"
  :type 'boolean)

(defvar hbm--marker-re "^[ \t]*#\\+\\(begin\\|end\\)_src")

(defvar hbm--org-unfontify #'org-unfontify-region)
;; (defvar hbm--org-fontify-blocks #'org-fontify-meta-lines-and-blocks)
(make-variable-buffer-local 'org-font-lock-ensure)
(make-variable-buffer-local 'org-unfontify-region)
(make-variable-buffer-local 'font-lock-fontify-region-function)
(make-variable-buffer-local 'font-lock-fontify-buffer-function)
(make-variable-buffer-local 'font-lock-unfontify-region-function)
(make-variable-buffer-local 'font-lock-unfontify-buffer-function)
(make-variable-buffer-local 'font-lock-ensure-function)
;; (make-variable-buffer-local 'org-fontify-meta-lines-and-blocks)

(defun hbm--update-line (visible)
  (let ((beg (if org-babel-hide-markers-line
                 (1- (line-beginning-position))
               (line-beginning-position)))
        (end (line-end-position)))
    (put-text-property beg end 'invisible visible)))

(defun hbm--update-markers (visible &optional beg end)
  (save-excursion
    (unless beg (setq beg (point-min)))
    (save-excursion
      (goto-char beg)
      (setq beg (line-beginning-position))
      (when end
        (goto-char end)
        (setq end (line-end-position))))
    (with-silent-modifications
      (goto-char beg)
      (while (re-search-forward hbm--marker-re end t)
        (hbm--update-line visible)))))

;;;###autoload
(defun org-babel-refresh-markers ()
  (interactive)
  (unless org-babel-hide-markers-mode
    (error "Org-hide-babel-markers mode is not enabled."))
  (hbm--update-markers t))

(defun hbm--mode-on ()
  (fset 'org-font-lock-ensure #'hbm--font-lock-ensure)
  (fset 'org-unfontify-region #'hbm--unfontify-region)
  ;; (fset 'org-fontify-meta-lines-and-blocks #'hbm--fontify-blocks)
  (setq font-lock-fontify-region-function #'hbm--fontify-region)
  (setq font-lock-fontify-buffer-function #'hbm--fontify-buffer)
  (setq font-lock-unfontify-region-function #'hbm--unfontify-region)
  (setq font-lock-unfontify-buffer-function #'hbm--unfontify-buffer)
  ;; (add-hook 'after-change-functions #'hbm--after-change-fn nil t)
  (hbm--update-markers t))

(defun hbm--mode-off ()
  (fset 'org-font-lock-ensure
        (if (fboundp 'font-lock-ensure)
            #'font-lock-ensure
          (lambda (&optional __b __e)
            (with-no-warnings (font-lock-fontify-buffer)))))
  (fset 'org-unfontify-region (symbol-value hbm--org-unfontify))
  ;; (fset 'org-fontify-meta-lines-and-blocks (symbol-value hbm--org-fontify-blocks))
  ;; (remove-hook 'after-change-functions 'hbm--after-change-fn t)
  (setq font-lock-ensure-function #'font-lock-default-fontify-buffer)
  (setq font-lock-fontify-region-function #'font-lock-default-fontify-region)
  (setq font-lock-fontify-buffer-function #'font-lock-default-fontify-buffer)
  (setq font-lock-unfontify-region-function #'font-lock-default-unfontify-region)
  (setq font-lock-unfontify-buffer-function #'font-lock-default-unfontify-buffer)
  (hbm--update-markers nil))

;; font-lock callbacks
(defun hbm--fontify-region (beg end &optional verbose)
  (font-lock-default-fontify-region beg end verbose)
  (hbm--update-markers t beg end))

(defun hbm--fontify-buffer ()
  (font-lock-default-unfontify-buffer)
  (hbm--update-markers t))

(defun hbm--unfontify-region (beg end)
  (font-lock-default-unfontify-region beg end)
  (hbm--update-markers t beg end))

(defun hbm--unfontify-buffer ()
  (font-lock-default-unfontify-buffer)
  (hbm--update-markers t))

(defun hbm--font-lock-ensure (&optional beg end)
  (font-lock-default-fontify-buffer)
  (hbm--update-markers t beg end))

(defun hbm--fontify-blocks (limit)
  ;; (run-hook-with-args hbm--org-fontify-blocks limit)
  (hbm--update-markers t (point) limit))

(defun hbm--after-change-fn (beg end)
  (hbm--update-markers org-babel-hide-markers-mode beg end))

;;;###autoload
(define-minor-mode org-babel-hide-markers-mode
  "Hide/show babel source code blocks on demand."
  :global nil :lighter " OB Hmm"
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode."))
  ;; (hbm--mode-off)
  (if org-babel-hide-markers-mode
      (hbm--mode-on)
    (hbm--mode-off)))

(provide 'ob-hide-markers)

;;; ob-hide-markers.el ends here
