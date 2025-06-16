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
(require 'org)
(require 'ob-core)
(require 'org-protocol)
(require 'org-pretty-table)

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

;; (defun org-link-from-clipboard ()
;;   "Insert an org link into current buffer from an URL in clipboard."
;;   (interactive)
;;   (let* ((marker (point-marker))
;;          (url (or (current-kill 0) (read-string "URL: "))))
;;     (with-current-buffer (marker-buffer marker)
;;       (save-excursion
;;         (goto-char (marker-position marker))
;;         (org-insert-link nil url (org-desc-from-clipboard))))))

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
      (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c ))) string-to-transform)))

;; ;;;###autoload
;; (defun yas-org-expand ()
;;   (interactive)

;;   (let* ((info (org-babel-get-src-block-info 'no-eval))
;;          (major-mode (if info
;;                          (intern-soft (concat (car info) "-mode"))
;;                        (intern-soft "org-mode"))))
;;     (yas-expand)))

;;;###autoload
(defun yas-org-expand ()
  (interactive)
  (require 'yas)
  (let* ((info (car (org-babel-get-src-block-info 'no-eval)))
         (major-mode (if info (intern-soft (concat info "-mode")) major-mode)))
    (yas-expand-from-trigger-key)))

(setq org-capture-templates
      `(("p" "Protocol" entry (file+headline "~/webnotes.org" "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline "~/webnotes.org" "Inbox")
         "* %? [[%:link][%(transform-square-brackets-to-round-ones\"%:description\")]]\n")
        ("d" "Denote" plain (file "~/denotes.org")
         "* %^{Description} %^g\n  Created: %U\n  Author:%n\n  ID:%<%y%m%d%H%M%S>\n\n%?"
         :empty-lines 1)
        ("E" "Emmi" plain (file "~/Documents/emmi.org")
         "* %^{Description} \n  Created: %U\n  Author:%n\n\n%?"
         :empty-lines 1)
        ("g" "Gem note" plain (file "~/repos/gem/doc/notes.org")
         "* %^{Description} %^g\n  Created: %U\n  Author:%n\n  ID:%<%y%m%d%H%M%S>\n\n%?"
         :empty-lines 1)
        ("r" "To Read" plain (file "~/reading.org")
         "* %A %^g\n  Created: %U\n  ID: %<%y%m%d%H%M%S>\n\n%?"
         :empty-lines 1)
        ("e" "Email" entry (file "~/inbox.org")
         "* TODO %? email |- %:from: %:subject
                    :EMAIL:\n:PROPERTIES:\n:CREATED: %U\n:EMAIL-SOURCE:
                    %l\n:END:\n%U\n"
         :clock-in t :clock-resume t)))

(setq  org-log-done 'time
       org-ditaa-jar-path "/usr/bin/ditaa"
       org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
       org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold)))
       org-directory (expand-file-name "~/")
       org-default-notes-file (expand-file-name "notes.org" org-directory)
       org-use-speed-commands       t
       org-src-preserve-indentation t
       org-export-html-postamble    nil
       org-hide-leading-stars       t
       org-make-link-description    t
       org-hide-emphasis-markers    t
       org-link-descriptive         t
       org-startup-folded           'overview
       org-startup-indented         nil)

(provide 'org-extras)
;;; org-extras.el ends here
