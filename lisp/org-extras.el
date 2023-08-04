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

;; This file is always loaded in my Emacs, so no autload cookies needed

;;; Code:

(require 'ob-core)

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
  (let* ((url (or (current-kill 0) (read-string "URL: "))))
    (org-insert-link nil url)))

(defun org-desc-from-clipboard (url _desc)
  "Insert an org link into current buffer from an URL in clipboard."
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (let ((title "<title>\\(.*\\)\\(/>\\|</title>\\)"))
      (if (re-search-forward title nil t)
          (string-trim (match-string-no-properties 1))
        url))))

(org-link-set-parameters "http" :insert-description #'org-desc-from-clipboard)
(org-link-set-parameters "https" :insert-description #'org-desc-from-clipboard)

;; http://www.gnu.org


(defun org-desc-from-clipbard (url)
  (url-retrieve
   url
   (lambda (_status title)
     (goto-char (point-min))
     (if (re-search-forward title nil t)
         (string-trim (match-string-no-properties 1))
       url))
   '("<title>\\(.*\\)\\(/>\\|</title>\\)") t t))

(defun org-link-from-clipboard ()
  "Insert an org link into current buffer from an URL in clipboard."
  (interactive)
  (let* ((marker (point-marker))
         (url (or (current-kill 0) (read-string "URL: "))))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (org-insert-link nil url (org-desc-from-clipboard))))))

(defun org-link-from-clipboard ()
  "Insert an org link into current buffer from an URL in clipboard."
  (interactive)
  (let ((marker (point-marker))
        (url (or (current-kill 0) (read-string "URL: "))))
    (url-retrieve
     url
     (lambda (_status title)
       (goto-char (point-min))
       (when (re-search-forward title nil t)
         (setq title (string-trim (match-string-no-properties 1))))
       (with-current-buffer (marker-buffer marker)
         (save-excursion
           (goto-char (marker-position marker))
           (org-insert-link
            nil url (or title url)))))
     '("<title>\\(.*\\)\\(/>\\|</title>\\)") t t)))

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

;;;###autoload
(defun yas-org-expand ()
  (interactive)
  
  (let* ((info (org-babel-get-src-block-info 'no-eval))
         (major-mode (if info
                         (intern-soft (concat (car info) "-mode"))
                       (intern-soft "org-mode"))))
    (yas-expand)))

(provide 'org-extras)
;;; org-extras.el ends here
