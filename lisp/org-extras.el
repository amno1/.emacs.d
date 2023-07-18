;;; org-extras.el --- Some extra defuns for org-mode  -*- lexical-binding: t; -*-

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

;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-09/msg01435.html
(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (require 'mm-url)
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (with-current-buffer download-buffer
      (goto-char (point-min))
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string
       (buffer-substring-no-properties x1 x2)))))

(defun my-org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))


(defun org-agenda-show-agenda-and-todo (&optional arg)
  ""
  (interactive "P")
  (org-agenda arg "c")
  (org-agenda-fortnight-view))
;; ("P" "Research project" entry (file "~/Org/inbox.org")
;;  "* TODO %^{Project title} :%^G:\n:PROPERTIES:\n:CREATED:
;;     %U\n:END:\n%^{Project description}\n** [x] 
;;    TODO Literature review\n** [x] TODO %?\n** [x]
;;  TODO Summary\n** [x] TODO Reports\n** [x] Ideas\n"
;;  :clock-in t :clock-resume t)

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar
    (lambda (c)
      (if (equal c ?[) ?\( (if (equal c ?]) ?\) c ))) string-to-transform)))

(provide 'org-extras)
;;; org-extras.el ends here
