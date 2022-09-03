;;; edebug-x.el --- Extensions for Edebug

;; Copyright (C) 2013  Scott Barnett

;; Author: Scott Barnett <scott.n.barnett@gmail.com>
;; URL: https://github.com/ScottyB/edebug-x
;; Keywords: extensions
;; Version: 1.2

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

;; Extension to Edebug to make it a little nicer to work with.

;; Breakpoints can now be toggled from an Elisp buffer without first
;; running Edebug with `edebug-x-modify-breakpoint-wrapper', bound to
;; `C-x SPC'. If the function isn't instrumented already then it will
;; instrument it and then set the breakpoint. Conditional breakpoints
;; can also be set by calling the previous command with a prefix
;; argument.

;; The list of current break points can be viewed with
;; `edebug-x-show-breakpoints', bound to `C-c C-x b'. From the
;; tabulated list buffer the following commands are available:

;; `edebug-x-kill-breakpoint' bound to `K': clear breakpoint
;; `edebug-x-visit-breakpoint' bound to `RET': visit breakpoint location

;; To view a list of instrumented functions execute `C-c C-x i',
;; `edebug-x-show-instrumented'. The instrumented functions buffer has
;; these commands:

;; `edebug-x-evaluate-function' bound to `E': evaluate function,
;; clearing breakpoints within it.
;; `edebug-x-find-function' bound to `RET': jump to function.

;; There is also a convenience command, `edebug-x-show-data' (bound to
;; `C-c C-x s') which will split the window into two showing both the
;; breakpoints and instrumented function buffers. Executing `Q' will
;; remove both these buffers.

;;; Code:

(require 'which-func)
(require 'edebug)
(require 'cl-lib)

(defgroup edebug-x nil
  "Extensions to Edebug"
  :group 'Lisp
  :prefix "edebug-x")

(defface hi-edebug-x-stop
  '((((background dark)) (:background "plum1" :foreground "black"))
    (t (:background "wheat")))
  "Face for Edebug breakpoints."
  :group 'edebug-x)

(defface hi-edebug-x-debug-line
  '((((background dark)) (:background "light green" :foreground "black"))
    (t (:background "light green")))
  "Face for current-line while debugging."
  :group 'edebug-x)

(defvar instrumented-forms '()
  "Stores all instrumented forms. Format is (symbol name . buffer position).")

(defvar edebug-x-stop-point-overlay nil
  "Overlay that highlights the current line while debugging.")

(defun edebug-x-highlight-line ()
  "Create an overlay at line."
  (interactive)
  (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put overlay 'face 'hi-edebug-x-stop)
    (overlay-put overlay 'edebug-x-hi-lock-overlay t)))

(defun edebug-x-remove-highlight ()
  "Remove overlay at point if present."
  (interactive)
  (when (cl-find-if (lambda (elt) (equal (car (overlay-properties elt)) 'edebug-x-hi-lock-overlay))
                    (overlays-at (point)))
    (remove-overlays (line-beginning-position)
                     (line-end-position) 'edebug-x-hi-lock-overlay t)))

(defun edebug-x-highlight-all ()
  "Highlight all instrumented functions and breakpoints in
current file."
  (dolist (e instrumented-forms)
    (let ((symbol (intern (car e)))
          (pos (cdr e)))
      (when (string= (buffer-file-name)(symbol-file symbol))
        (save-excursion
          (goto-char pos)
          (edebug-x-highlight-line))
        (cl-destructuring-bind (marker breakpoints stop-points others)
            (get symbol 'edebug)
          (dolist (b breakpoints)
            (save-excursion
              (goto-char (+ pos (aref stop-points (car b))))
              (edebug-x-highlight-line))))))))

(defun edebug-x-remove-debug-line ()
  "Remove the overlay showing line that the debugger is at."
  (remove-overlays (point-min) (point-max) 'edebug-x-debug t))

(defadvice edebug-overlay-arrow (after edebug-x-highlight-debug-line activate)
  "Highlight the current line while debugging."
  (let ((start (line-beginning-position))
        (end (line-end-position))
        overlay)
    (if edebug-x-stop-point-overlay
        (move-overlay edebug-x-stop-point-overlay start end)
      (setq overlay (make-overlay start end))
      (overlay-put overlay 'edebug-x-debug t)
      (overlay-put overlay 'face 'hi-edebug-x-debug-line)
      (setq edebug-x-stop-point-overlay overlay))))

(defadvice edebug-set-windows (before edebug-x-edebug-set-windows
                                      (count &optional all-frames)
                                      activate)
  "Remove Edebug-x's current line highlighting."
  (if (string= major-mode "emacs-lisp-mode")
      (edebug-x-remove-debug-line)))

(defadvice edebug-make-form-wrapper (after edebug-x-make-form-wrapper
                                           (cursor form-begin form-end
                                                   &optional speclist)
                                           activate)
  "Highlight the form being wrapped and save it to a list."
  (save-excursion
    (let* ((func (which-function)))
      (beginning-of-defun)
      (if (not (member func instrumented-forms))
          (add-to-list 'instrumented-forms `(,func . ,(point))))
      (edebug-x-highlight-line))))

(defadvice edebug-read-sexp (before edebug-x-read-sexp activate)
  "Stores forms instrumented and removes overlay if present."
  (let* ((func (which-function)))
    (setq instrumented-forms
          (cl-remove-if (lambda (elemt) (equal (car elemt) func)) instrumented-forms))
    (save-excursion
      (remove-overlays (point)
                       (progn (forward-sexp 1) (point)) 'edebug-x-hi-lock-overlay t)
      (edebug-x-remove-debug-line))))

(defun instrumentedp (fun-symbol)
  (unless (functionp fun-symbol)
    (error "Not a function or function hasn't been evaluated yet."))
  (let ((data (get fun-symbol 'edebug)))
    (unless (markerp data)
      data)))

;;;###autoload
(defun edebug-x-modify-breakpoint-wrapper (arg)
  "Set a breakpoint from an Elisp file.
The current function that pointer is in will be instrumented if
not already. When called with a prefix argument a conditional
breakpoint is set."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (let* ((func-symbol (intern (which-function)))
           (edebug-data (get func-symbol 'edebug))
           (breakpoints (and (not (markerp edebug-data)) (car (cdr edebug-data))))
           (removed (cl-remove-if (lambda (elt) (= (cdr (edebug-find-stop-point)) (car elt)))
                                  breakpoints))
           (new-breakpoint (= (length breakpoints) (length removed))))
      (if (not (instrumentedp func-symbol))
          (edebug-eval-top-level-form))
      (save-excursion
        (goto-char
         (if new-breakpoint
             (progn
               (if (not arg)
                   (edebug-modify-breakpoint t)
                 (setq current-prefix-arg nil)
                 (call-interactively 'edebug-set-conditional-breakpoint)))
           (edebug-modify-breakpoint nil)))
        (if new-breakpoint (edebug-x-highlight-line) (edebug-x-remove-highlight))))))

(defadvice edebug-set-breakpoint (before edebug-x-set-breakpoint-highlight
                                         (arg)
                                         activate)
  "Highlights the current line."
  (edebug-x-highlight-line))

(defadvice edebug-unset-breakpoint (before edebug-x-unset-breakpoint-highlight
                                           activate)
  "Remove highlights from the current line."
  (edebug-x-remove-highlight))

(defadvice top-level (before edebug-x-top-level activate)
  "Remove current line highlight when finished debugging."
  (edebug-x-remove-debug-line))

(defun edebug-x-read-breakpoint-at-line ()
  "Return a list of values read from the breakpoints buffer.
Values are read from the line at point."
  (cl-remove-if (lambda (str) (string= str ""))
                (split-string (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)) "  ")))

(defun edebug-x-visit-breakpoint ()
  "Navigate to breakpoint at line."
  (interactive)
  (cl-destructuring-bind (func-name pos &optional condition temporary)
      (edebug-x-read-breakpoint-at-line)
    (find-function (intern func-name))
    (goto-char (string-to-number pos)))
  (edebug-x-highlight-all))

(defun edebug-x-clear-data ()
  "Delete the window setup after `edebug-show-data'."
  (interactive)
  (delete-other-windows)
  (switch-to-prev-buffer))

(defun edebug-x-kill-breakpoint ()
  "Remove breakpoint at line."
  (interactive)
  (cl-destructuring-bind (func-name pos &optional condition temporary)
      (edebug-x-read-breakpoint-at-line)
    (when (y-or-n-p (format "Edebug breakpoints: delete breakpoint %s?" func-name))
      (save-excursion
        (edebug-x-visit-breakpoint)
        (edebug-x-modify-breakpoint-wrapper nil)
        (bury-buffer)))
    (revert-buffer)))

(defun edebug-x-list-breakpoints ()
  "Checks all the instrumented functions for any breakpoints.
Returns a tablulated list friendly result to be displayed in
edebug-breakpoint-list-mode."
  (let ((results))
    (dolist (form instrumented-forms)
      (let* ((func-sym (intern (car form)))
             (edebug-data (get func-sym 'edebug))
             (pos (cdr form))
             (func-name (car form))
             (breakpoints (car (cdr edebug-data)))
             (stop-points (nth 2 edebug-data)))
        (cl-loop for i in breakpoints do
              (add-to-list
               'results
               (list form
                     (vconcat `(,func-name)
                              (list (number-to-string (+ pos (aref stop-points (car i)))))
                             (mapcar (lambda (ele) (if ele
                                                   (with-temp-buffer
                                                     (princ ele (current-buffer))
                                                     (buffer-string))
                                                 ""))
                                      (cdr i))
                              `(,(file-name-nondirectory (symbol-file func-sym)))))))))
    results))

(define-derived-mode
  edebug-x-breakpoint-list-mode tabulated-list-mode "Edebug Breakpoints"
  "Major mode for listing Edebug breakpoints"
  (setq tabulated-list-entries 'edebug-x-list-breakpoints)
  (setq tabulated-list-format
        [("Function name" 50 nil)
         ("Position" 20 nil)
         ("Condition" 50 nil)
         ("Temporary" 20 nil)
         ("File name" 20 nil)])
  (tabulated-list-init-header))

(defvar edebug-x-breakpoint-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'edebug-x-visit-breakpoint)
    (define-key map (kbd "K") 'edebug-x-kill-breakpoint)
    (define-key map (kbd "Q") 'edebug-x-clear-data)
    map)
  "Keymap used in `edebug-x-breakpoint-list-mode' buffers.")

;;;###autoload
(defun edebug-x-evaluate-function ()
  "Evaluate function on line.
This removes all breakpoints in this function."
  (interactive)
  (let ((function-name (car (split-string (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))))))
    (when (y-or-n-p (format "Edebug instrumented functions: evaluate function %s?" function-name))
      (find-function (intern function-name))
      (eval-defun nil)
      (switch-to-prev-buffer)
      (revert-buffer))))

(defun edebug-x-find-function ()
  "Navigate to function from the instrumented function buffer."
  (interactive)
  (let ((function-name (car (split-string (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))))))
    (find-function (intern function-name))
    (edebug-x-highlight-all)))

(defun edebug-x-list-instrumented-functions ()
  "Return the list of instrumented functions.
Tabulated buffer ready."
  (mapcar (lambda (item) (let ((str (car item)))
                      (list str (vector str (symbol-file (intern str))))))
          instrumented-forms))

(define-derived-mode
  edebug-x-instrumented-function-list-mode tabulated-list-mode "Edebug Instrumented functions"
  "Major mode for listing instrumented functions"
  (setq tabulated-list-entries 'edebug-x-list-instrumented-functions)
  (setq tabulated-list-format
        [("Instrumented Functions" 50 nil)
         ("File" 150 nil)])
  (tabulated-list-init-header))

(defvar edebug-x-instrumented-function-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "E") 'edebug-x-evaluate-function)
    (define-key map (kbd "Q") 'edebug-x-clear-data)
    (define-key map (kbd "RET") 'edebug-x-find-function)
    map)
  "Keymap for `edebug-x-instrumented-function-list-mode' buffers.")

;;;###autoload
(defun edebug-x-show-data ()
  "Display instrumented functions and edebug breakpoints.
Frame is split into two vertically showing the tabluated buffers
for each."
  (interactive)
  (delete-other-windows)
  (let ((buff-breakpoints (get-buffer-create "*Edebug Breakpoints*"))
        (buff-instrumented (get-buffer-create "*Instrumented Functions*")))
    (with-current-buffer buff-breakpoints
      (edebug-x-breakpoint-list-mode)
      (tabulated-list-print))
    (with-current-buffer buff-instrumented
      (edebug-x-instrumented-function-list-mode)
      (tabulated-list-print))
    (switch-to-buffer buff-breakpoints)
    (set-window-buffer (split-window-vertically)
                       buff-instrumented)))

;;;###autoload
(defun edebug-x-show-breakpoints ()
  "Display breakpoints in a tabulated list buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Edebug Breakpoints*"))
  (edebug-x-breakpoint-list-mode)
  (tabulated-list-print))

;;;###autoload
(defun edebug-x-show-instrumented ()
  "Display instrumented functions in a tabluated list buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Instrumented Functions*"))
  (edebug-x-instrumented-function-list-mode)
  (tabulated-list-print))

(defalias 'list-edebug-x-breakpoints 'edebug-x-show-breakpoints)
(defalias 'list-edebug-x-instrumented 'edebug-x-show-instrumented)

;;;###autoload
(define-minor-mode edebug-x-mode
  "A minor mode that makes it easier to use Edebug"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x SPC") 'edebug-x-modify-breakpoint-wrapper)
            (define-key map (kbd "C-c C-x s") 'edebug-x-show-data)
            (define-key map (kbd "C-c C-x b") 'edebug-x-show-breakpoints)
            (define-key map (kbd "C-c C-x i") 'edebug-x-show-instrumented)
            map))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'edebug-x-mode)

(provide 'edebug-x)

;;; edebug-x.el ends here
