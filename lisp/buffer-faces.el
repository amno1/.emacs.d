;;; buffer-faces.el --- show all faces being used in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2011  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
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

;; <2025-07-03 tor> AM: fixed som trivial warnings and added special mode 
;; <2025-07-05 lör> AM: remove speliazied buffer and display in help buffer

;;; Code:

(defun collect-buffer-faces (buffer)
  "Collect all faces found in BUFFER"
  (let ((faces nil))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (< (point) (point-max))
          (add-to-list 'faces (get-text-property (point) 'face))
          (goto-char (next-property-change (point) nil (point-max))))))
    (delete nil faces)))

;;;###autoload
(defun show-buffer-faces (&optional buffer)
  "Display faces used in the BUFFER in help window.

If not specified, or called interactively, BUFFER defaults to `current-buffer'"
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (help-buffer (or (help-buffer) (get-bufer-create "*Help*")))
         (faces (collect-buffer-faces buffer))
         (help-buffer-under-preparation t))
    (help-setup-xref (list #'show-buffer-faces buffer)
		     (called-interactively-p 'interactive))
    (with-help-window help-buffer
      (insert
       (format "Faces found in buffer %s:\n\n" (buffer-name buffer)))

      (let ((sort-start (point)))
        (dolist (face faces)
          (help-insert-xref-button
           (propertize (format "  %s" face) 'face face)
           'help-face face)
          (insert "\n"))
        (sort-lines t sort-start (point))))))

(provide 'buffer-faces)
;;; buffer-faces.el ends here
