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

(require 'autoinsert)
(require 'paredit)
(require 'sotlisp)
(require 'company)
;;(require 'outshine)
(require 'yasnippet)
(require 'page-break-lines)
(require 'helm-pages)
(require 'popup)
(require 'eros)

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

(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

(defun imacro-expand (expand-all)
  (interactive "P")
  (save-excursion
    (let* ((end (progn (end-of-defun) (point)))
           (beg (progn (beginning-of-defun) (point)))
           (sxp (progn (read (current-buffer)))))
      (eros--eval-overlay
       (with-temp-buffer
         (pp
          (if expand-all (macroexpand-all sxp) (macroexpand sxp))
          (current-buffer))
         (buffer-string))
       (save-excursion
         (end-of-defun)
         (point))))))

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

(defun add-elisp-advices ()
  (advice-add #'emacs-lisp-byte-compile :before #'basic-save-buffer))

(defun remove-elisp-advices ()
  (advice-remove #'emacs-lisp-byte-compile #'basic-save-buffer))

(defun elisp-hooks ()
  (setq fill-column 80)
  (paredit-mode 1)
  (company-mode 1)
  ;; (outshine-mode 1)
  (yas-minor-mode 1)
  (speed-of-thought-mode 1)
  (page-break-lines-mode 1)
  ;;(lisp-extra-font-lock-mode 1)
  (eros-mode 1))

(defun yas-undo-expand ()
  (interactive)
  (undo)
  (insert " "))

;; from https://www.emacswiki.org/emacs/auto-insert-for-asdf
(push `(("\\.asd\\'" . "ASDF Skeleton") 
	              "System Name: "
	              "
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :" str ".system)
    (defpackage :" str ".system
      (:use :common-lisp :asdf))))

(in-package :" str ".system)
(defsystem :" str " 
  :description " ?\" (read-string "Description: ") ?\"" 
  :author \"" (user-full-name) " <" user-mail-address ">\" 
  :licence \"" (read-string "License: ") "\" 
  :version \"" (read-string "Version: ") "\" 
  :components (()) 
  :depends-on ())") 
      auto-insert-alist)

;; movement

(defun beginning-of-list ()
  "Move cursor to the beginning of current list."
  (interactive)
  (call-interactively #'backward-up-list)
  (forward-char))

(defun end-of-list ()
  "Move cursor to the end of current list"
  (interactive)
  (call-interactively #'up-list)
  (backward-char))

(defvar defun-start-re "([[:space:]]*\\(cl-\\)??def.*?[[:space:]]+")

(defun read-form ()
  (condition-case nil
      (read (current-buffer))
    (error nil)))

(defun has-operator-p (tree operator)
    (cond ((eq tree nil) nil)
          ((listp (car tree))
           (or (has-operator-p (car tree) operator) 
               (has-operator-p (cdr tree) operator)))
          (t
           (cond ((eq (car tree) operator) tree)
                 (t
                  (has-operator-p (cdr tree) operator))))))

(defun defun-start ()
  (interactive)
  (let* ((cursor (point))
         (beg (save-excursion
                (end-of-defun)
                (beginning-of-defun)
                (point)))
         (form (save-excursion (read-form)))
         (found
          (catch 'found
            (while (re-search-backward defun-start-re beg t)
              (unless (nth 3 (parse-partial-sexp beg (point)))
                (throw 'found (point)))))))
    (cond (found found)
          (t (goto-char cursor)
             nil))))

(defun defun-end ()
  (interactive)
  (when (defun-start)
    (forward-list)
    (point)))

(defun beginning-of-defun-dwim ()
  (interactive)
  (unless (defun-start)
    (beginning-of-defun)))

(defun end-of-defun-dwim ()
  (interactive)
  (unless (defun-end)
    (end-of-defun)))

(defun active-minor-modes (&optional buffer-or-buffer-name)
  (let ((buffer (or buffer-or-buffer-name (current-buffer)))
        active-modes)
    (with-current-buffer (get-buffer buffer)
      (dolist (mode minor-mode-list)
        (and (boundp mode) (symbol-value mode)
             (push mode active-modes))))
    active-modes))

(provide 'elisp-extras)
;;; elisp-extras.el ends here
