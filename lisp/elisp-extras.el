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

;; This file is always loaded in my Emacs, so I don't use autoload cookies here
;;; Code:

;;(require 'paredit)
(require 'sotlisp)
;;(require 'company)
;;(require 'outshine)
(require 'yasnippet)
(require 'page-break-lines)
(require 'helm-pages)

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

(defun elisp-hooks ()
  (setq fill-column 80)
  (paredit-mode 1)
;;  (company-mode 1)
  ;; (outshine-mode 1)
  (yas-minor-mode 1)
  (speed-of-thought-mode 1)
  (page-break-lines-mode 1)
  ;;(lisp-extra-font-lock-mode 1)
  )

(defun elisp-gen-autoload-cookie ()
  "Generate an autoload cookie for the defun at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-char)
    (let ((sym (read (current-buffer))))
      (if (not (eq sym 'defun))
          (user-error "Not in a defun.")
        (beginning-of-defun)
        (insert ";;;###autoload\n")))))

(defun yas-undo-expand ()
  (interactive)
  (undo)
  (insert " "))

;; from https://www.emacswiki.org/emacs/auto-insert-for-asdf
;; (push `(("\\.asd\\'" . "ASDF Skeleton") 
;; 	              "System Name: "
;; 	              "
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (unless (find-package :" str ".system)
;;     (defpackage :" str ".system
;;       (:use :common-lisp :asdf))))

;; (in-package :" str ".system)
;; (defsystem :" str " 
;;   :description " ?\" (read-string "Description: ") ?\"" 
;;   :author \"" (user-full-name) " <" user-mail-address ">\" 
;;   :licence \"" (read-string "License: ") "\" 
;;   :version \"" (read-string "Version: ") "\" 
;;   :components (()) 
;;   :depends-on ())") 
;;                     auto-insert-alist)

(provide 'elisp-extras)
;;; elisp-extras.el ends here
