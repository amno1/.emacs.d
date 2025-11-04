;;; buffer-faces.el --- show all faces being used in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2025 Arthur Miller

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by the idea by J. Rockway:
;; https://github.com/jrockway/elisp/blob/master/buffer-faces.el

;; Completely re-worked to work with the built-in help-mode.

;;; Code:

;;;###autoload
(defun describe-buffer-faces (&optional buffer)
  "Display list of font faces used in BUFFER.

If not specified, or called interactively, BUFFER defaults to `current-buffer'"
  (interactive)
  (let* ((help-buffer (or (help-buffer) (get-buffer-create "*Help*")))
         (working-buffer (or buffer (current-buffer)))
         (help-buffer-under-preparation t)
         (faces nil))

    (help-setup-xref (list #'show-buffer-faces buffer)
                     (called-interactively-p 'interactive))

    (save-excursion
      (with-current-buffer working-buffer
        (goto-char (point-min))
        (while (< (point) (point-max))
          ;; add-to-list can not use lexical var faces ????
          ;; (add-to-list 'faces (get-text-property (point) 'face))
          (let ((face (get-text-property (point) 'face)))
            (unless (member face faces)
              (push face faces)))
          (goto-char (next-property-change (point) nil (point-max))))))

    (with-help-window help-buffer
      (insert
       (format "Faces found in buffer '%s':\n\n" (buffer-name working-buffer)))

      (dolist (face (delete nil faces))
        (help-insert-xref-button
         (propertize (format "  %s" face) 'face face) 'help-face face)
        (insert "\n")))))

(provide 'buffer-faces)
;;; buffer-faces.el ends here
