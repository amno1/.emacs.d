;;; term-toggle.el --- Toggle to and from the *terminal* buffer

;; Filename: term-toggle.el
;; Description: Toggle a dedicated terminal
;; Author: Yatao <yatao.li@live.com>
;; Created: 2011-03-02
;; Version: 0.0.1
;; URL: https://github.com/v-yadli/emacs-term-toggle
;; Keywords:  term toggle shell
;; Compatibility: (Test on GNU Emacs 24.3.1).
;;
;;{{{ License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;}}}

;;; Commentary: Derived from Joseph <jixiuf@gmail.com> (URL:
;;; http://www.emacswiki.org/term-toggle.el), this plugin brings up a
;;; quake-style console with commands term-toggle{,-cd}
;;; The major difference with Joseph's version is that maximized
;;; console feature is removed (in the original version sometimes it
;;; gets stuck in maximized state, possibly because the window
;;; configuration is corrupted). Also, this plugin determines whether
;;; to split a new window for the console, or replace the buffer of
;;; current selected window if height is not enough for a
;;; split. Another feature is that this plugin will detect the status
;;; of the terminal. When there's no process running in *terminal*
;;; buffer, it will fire up another one.


;;; Installation:
;;;
;;; o Place this file in a directory in your 'load-path.
;;; o Put the following in your .emacs file:
;;;   (autoload 'term-toggle "term-toggle"
;;;    "Toggles between the *terminal* buffer and whatever buffer you are editing."
;;;    t)
;;;   (autoload 'term-toggle-cd "term-toggle"
;;;    "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
;;;   (global-set-key [M-f1] 'term-toggle)
;;;   (global-set-key [C-f1] 'term-toggle-cd)
;;; o Restart your Emacs.  To use term-toggle just hit M-f1 or C-f1
;;;
;;; Changes:
;;; 2019-01-23 A. Miller: added eshell toggle

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `term-toggle-cd'
;;    Calls `term-toggle' with a prefix argument.  Se command `term-toggle'
;;  `term-toggle'
;;    Toggles between the *eshell* buffer and whatever buffer you are editing.
;;  `term-toggle-eshell-cd'
;;    Calls `term-toggle-eshell' with a prefix argument.  Se command `term-toggle-eshell'
;;  `term-toggle-eshell'
;;    Toggles between the *eshell* buffer and whatever buffer you are editing.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;


(require 'term)
(require 'eshell)

;;;###autoload
(defvar term-toggle-goto-eob t
  "*If non-nil `term-toggle' will move point to the end of the shell-buffer
whenever the `term-toggle' switched to the shell-buffer.

When `term-toggle-cd' is called the point is allways moved to the end of the
shell-buffer")

;;;###autoload
(defvar term-toggle-minimum-split-height 10
  "The minimum height of a splittable window")

;;;###autoload
(defvar term-toggle-default-height 15
  "The default height of a splitted window")

;;;###autoload
(defvar term-toggle-automatic-cd t
  "*If non-nil `term-toggle-cd' will send the \"cd\" command to the shell.
If nil `term-toggle-cd' will only insert the \"cd\" command in the
shell-buffer.  Leaving it to the user to press RET to send the command to
the shell.")

;;;###autoload
(defun term-toggle-cd ()
  "Calls `term-toggle' with a prefix argument.  Se command `term-toggle'"
  (interactive)
  (term-toggle t))

;;;###autoload
(defun term-toggle-eshell-cd ()
  "Calls `term-toggle' with a prefix argument.  Se command `term-toggle'"
  (interactive)
  (term-toggle-eshell t))

;;;###autoload
(defun term-toggle (make-cd)
  "Toggles between the *terminal* buffer and whatever buffer you are
editing.  With a prefix ARG also insert a \"cd DIR\" command into the
shell, where DIR is the directory of the current buffer.
When called in the *terminal* buffer, the terminal window is
closed. The original buffer will be restored if it's a replace instead
of a split.  Options: `term-toggle-goto-eob'"
  (interactive "P")
  ;; If the terminal window exists, kill it
  ;; Otherwise, bring it on.
  (let ((shell-window (get-buffer-window "*terminal*" t)))
    (if shell-window
        (if term-toggle-replaced-buffer
            (progn
              (set-window-dedicated-p shell-window nil)
              (bury-buffer))
          (delete-window shell-window))
      (term-toggle-buffer-goto-shell make-cd)))
  ) ;Disable the double-in-a-row crap(which doesn't work sometimes)

;;;###autoload
(defun term-toggle-eshell (make-cd)
  "Toggles between the *eshell* buffer and whatever buffer you are
editing.  With a prefix ARG also insert a \"cd DIR\" command into the
shell, where DIR is the directory of the current buffer.
When called in the *terminal* buffer, the terminal window is
closed. The original buffer will be restored if it's a replace instead
of a split.  Options: `term-toggle-goto-eob'"
  (interactive "P")
  ;; If the terminal window exists, kill it
  ;; Otherwise, bring it on.
  (let ((shell-window (get-buffer-window "*eshell*" t)))
    (if shell-window
        (if term-toggle-replaced-buffer
            (progn
              (set-window-dedicated-p shell-window nil)
              (bury-buffer))
          (delete-window shell-window))
      (term-toggle-buffer-goto-eshell make-cd))))

;;; ======================================================================
;;; Internal functions and declarations

(defvar term-toggle-replaced-buffer nil
  "Indicator for the term toggle behavior. When set to t, the term
  buffer will appear in the selected window instead of split it.")

(defun term-toggle-fire-up-shell ()
  "Fires up a shell."
  (condition-case the-error
      (term (getenv "SHELL"))
    (error (switch-to-buffer "*terminal*"))))

(defun term-toggle-fire-up-eshell ()
  "Fires up an eshell."
  (condition-case the-error
      (eshell)
    (error (switch-to-buffer "*eshell*"))))

(defun term-toggle-buffer-goto-shell (make-cd)
  "Switches other window to the *terminal* buffer.  If no *terminal*
buffer exists start a new shell and switch to it in a window (see
`term-toggle-buffer-switch-to-window' for the algorithm).  If argument
MAKE-CD is non-nil, insert a \"cd DIR\" command into the shell, where
DIR is the directory of the current buffer."
  (let ((shell-buffer (get-buffer "*terminal*"))
	(cd-command
	 ;; Find out which directory we are in (the method differs for
	 ;; different buffers)
	 (or (and make-cd
		  (buffer-file-name)
		  (file-name-directory (buffer-file-name))
		  (concat "cd " (file-name-directory (buffer-file-name))))
	     (and make-cd
		  list-buffers-directory
		  (concat "cd " list-buffers-directory)))))

    (term-toggle-buffer-switch-to-window)
    (if shell-buffer
        (progn
          (switch-to-buffer shell-buffer)
          (if (not (term-check-proc shell-buffer))
              (progn
                (kill-buffer shell-buffer)
                (term-toggle-fire-up-shell))))
      (term-toggle-fire-up-shell))
    (set-window-dedicated-p (selected-window) t)
    (if (or cd-command term-toggle-goto-eob)
        (term-send-del))
    (if (and cd-command term-toggle-automatic-cd)
        (term-send-raw-string (concat cd-command "\n")))))

(defun term-toggle-buffer-goto-eshell (make-cd)
  "Switches other window to the *eshell* buffer.  If no *eshell*
buffer exists start a new eshell buffer and switch to it in a window (see
`term-toggle-buffer-switch-to-window' for the algorithm).  If argument
MAKE-CD is non-nil, insert a \"cd DIR\" command into the eshell, where
DIR is the directory of the current buffer."
  (let ((eshell-buffer (get-buffer "*eshell*"))
	(cd-command
	 ;; Find out which directory we are in (the method differs for
	 ;; different buffers)
	 (or (and make-cd
		  (buffer-file-name)
		  (file-name-directory (buffer-file-name))
		  (concat "cd " (file-name-directory (buffer-file-name))))
	     (and make-cd
		  list-buffers-directory
		  (concat "cd " list-buffers-directory)))))

    (term-toggle-buffer-switch-to-window)
    (if eshell-buffer
        (switch-to-buffer eshell-buffer)
      (term-toggle-fire-up-eshell))
    (set-window-dedicated-p (selected-window) t)
    (if (and cd-command term-toggle-automatic-cd)
        (eshell-send-input (concat cd-command "\n")))))

(defun term-toggle-buffer-switch-to-window ()
  "Switches to a window. If the current window has a splittable size
\(in height\), split it and switch to the bottom part.  Otherwise, use
this window and mark the `term-toggle-replaced-buffer' flag and keep
the same window selected"
  (let ((this-window (selected-window)))
    (if (>=
         (window-total-height this-window)
         term-toggle-minimum-split-height)
	(progn
          (setq term-toggle-replaced-buffer nil)
	  (split-window-vertically)
          (other-window 1)
          (setq this-window (selected-window))
          (let ((delta (- (window-height this-window) term-toggle-default-height)))
            (if (> delta 0)
                (shrink-window delta))))
      (setq term-toggle-replaced-buffer t))))

(provide 'term-toggle)

;;; term-toggle.el ends here

;;; LocalWords:  el eshell term bash shell toggle
