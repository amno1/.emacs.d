;;; c++-setup.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  your name here

;; Author: your name here <your-email-address-here>
;; Keywords: abbrev

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

(defun compile-again (pfx)
  "Run the same compile as the last time.
    If there was no last time, or there is a prefix argument,
    this acts like M-x compile."
  (interactive "p")
  (if (and (eq pfx 1) compilation-last-buffer)
      (progn
        (set-buffer compilation-last-buffer)
        (revert-buffer t t))
    (call-interactively 'compile)))


(defun find-dedicated-frames (buf)
  (let (result)
    (dolist (window (get-buffer-window-list buf t) result)
      (let ((frame (window-frame window)))
        (when (frame-parameter frame 'unsplittable)
          (push frame result))))))

(defun qtmstr-setup-compile-mode ()
  ;; Support C++ better
  (modify-syntax-entry ?< "(")
  (modify-syntax-entry ?> ")")

  (dolist (frame (find-dedicated-frames (current-buffer)))
    (let ((orig (frame-parameter frame 'orig-background)))
      (when orig
        (modify-frame-parameters
         frame (list (cons 'background-color orig)))))))

(defun qtmstr-compile-finish (buf status)
  (with-current-buffer buf
    (let* ((color (if (string-match "^finished\\b" status)
                      "#dfd" "#fdd"))
           found)
      (dolist (frame (find-dedicated-frames buf))
        (setq found t)
        (modify-frame-parameters
         frame
         (list (cons 'background-color color)
               (cons 'orig-background
                     (frame-parameter frame 'background-color)))))
      (unless found
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face (list :background color))
          (overlay-put overlay 'evaporate t))))))

(defun get-nearest-compilation-file ()
  "Search for the compilation file traversing up the directory tree."
  (let* ((dir default-directory) 
         (file-path)
         (parent-dir (file-name-directory (directory-file-name default-directory)))
         (nearest-compilation-file 'nil))
    (while (and (not (string= dir parent-dir))
                (not nearest-compilation-file))
      (dolist (filename compilation-filenames)
        (setq file-path (concat dir filename))
        (when (file-readable-p file-path)
          (setq nearest-compilation-file file-path)))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-file))

(defun clanguages-pairs-hook ()
  "Add some extra electric pairs to C and C++"
  (define-key c-mode-map "("  'electric-pair)
  (define-key c-mode-map "["  'electric-pair)
  (define-key c-mode-map "{"  'electric-pair))

(defun c-modes-keys ()
  (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-c C-r") 'compile-again)
  (define-key c-mode-base-map (kbd "C-c C-k") 'kill-compilation))

(defun my-c++-init ()
  (message "Running c++-init")
  (require 'member-functions)
  (setq compilation-last-buffer nil
        compilation-read-command nil
        compilation-filenames '("Makefile" "makefile")
        mf--source-file-extension "cc"
        auto-insert t
        auto-insert-mode t
        auto-insert-query nil
        c-default-style "gnu")
  (push '("*compilation*"
          (minibuffer . nil)
          (unsplittable . t)
          (menu-bar-lines . 0))
        special-display-buffer-names)
  ;; display-buffer-alist)
  (add-hook 'c-mode-common-hook 'company-mode)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'c-modes-keys)
  (add-hook 'c++-mode-hook 'hs-minor-mode)
  (add-hook 'c++-mode-hook 'yas-minor-mode)
  (add-hook 'c++-mode-hook 'hide-ifdef-mode)
  (add-hook 'c++-mode-hook 'auto-revert-mode)
  (add-hook 'c++-mode-hook 'clanguages-pairs-hook)
  (add-hook 'c++-mode-hook (lambda () (subword-mode 1)))
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'c-modes-keys)
  (add-hook 'c-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'hide-ifdef-mode)
  (add-hook 'c-mode-hook 'auto-revert-mode)
  (add-hook 'c-mode-hook 'clanguages-pairs-hook)
  (add-hook 'c-mode-hook (lambda () (subword-mode 1)))
  (add-hook 'compilation-mode-hook 'qtmstr-setup-compile-mode)
  (add-hook 'compilation-finish-functions 'qtmstr-compile-finish)

  ;; This prevents the extra two spaces in a namespace that Emacs
  ;; otherwise wants to put.
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  ;; Autoindent using google style guide
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  (add-hook 'c-mode-hook (lambda () (set (make-local-variable 'compile-command)
                                         (format "make -f %s"
                                                 (get-nearest-compilation-file)))))
  (add-hook 'c++-mode-hook (lambda () (set (make-local-variable 'compile-command)
                                           (format "make -f %s"
                                                   (get-nearest-compilation-file)))))
  (setq indent-tabs-mode nil))

  (provide 'c++-setup)
;;; c++-setup.el ends here
