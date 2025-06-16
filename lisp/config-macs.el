;;; config-macs.el --- Few macros for init file      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Arthur Miller

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

(defcustom default-idle-interval 0.5
  "Default time interval to run an idle timer."
  :type 'number
  :group 'emacs-config)

(defvar *current-package* nil)

(defmacro on-idle (&rest body)
  "Basic wrapper for a default `idle-timer' hook."
  (declare (indent defun))
  `(run-with-idle-timer default-idle-interval nil (lambda () ,@body)))

(defmacro on-hook (hook &rest body)
  "Less-verbose `add-hook', with very basic functionality."
  (declare (indent defun) (debug (sexp def-body)))
  `(add-hook ',hook (lambda () ,@body)))

(defmacro on-load (&rest body)
  "Slightly less-verbose `with-eval-after-load'"
  (declare (indent defun) (debug (sexp def-body)))
  `(with-eval-after-load ,*current-package*
     ,@body))

(defmacro with-package (package &rest body)
  (declare (indent defun) (debug (sexp def-body)))
  `(with-eval-after-load ,package
     (lambda () ,@body)))

(defmacro defkeys (mapname &rest body)
  (declare (indent defun) (debug (sexp def-body)))
  `(let ((defs '(,@body)))
     (while defs
       (define-key ,mapname
                   (if (vectorp (car defs))
                       (car defs)
                     (read-kbd-macro (car defs)))
                   (if (or (listp (cadr defs)) (functionp (cadr defs)))
                       (cadr defs)
                     (if `(keymapp (bound-and-true-p ,(cadr defs)))
                         (eval (cadr defs)))))
       (setq defs (cddr defs)))))

(defmacro on-system (systype &rest body)
  (declare (indent defun) (debug (sexp def-body)))
  `(when (eq ',system-type ',systype)
     ,@body))

(defmacro on-host (host &rest body)
  (declare (indent defun) (debug (sexp def-body)))
  `(when (equal ,system-name ,host)
     ,@body))

;; (defmacro on-init (&rest body)
;;   (declare (indent defun) (debug (sexp def-body)))
;;   `(progn ,@body))

(defmacro on-early-init (&rest body)
  (declare (indent defun) (debug (sexp def-body)))
  `(progn ,@body))

(provide 'config-macs)
;;; config-macs.el ends here
