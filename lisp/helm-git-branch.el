;;; helm-git-branch.el --- Switch git branch with Helm interface.  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'helm-ls-git)

(defvar helm-source-git-local-branches nil
  "This source will built at runtime.
It can be build explicitly with function
`helm-git-build-local-branches-source'.")

(defface helm-git-branch-current-clean-face
    '((t :foreground "#2aa198"))
  "Files which contain rebase/merge conflicts."
  :group 'helm-ls-git)

(defface helm-git-branch-current-dirty-face
    '((t :foreground "#dc322f"))
  "Files which contain rebase/merge conflicts."
  :group 'helm-ls-git)

;;; Custom

(defgroup helm-git-branch nil
  "Fast git branch switching with Helm."
  :group 'helm)

(defcustom helm-git-branch-changes-action 'stash
  "The default action upon changing a branch with uncommited changes.

If this option is set to 'stash the changes will be automatically stashed.

If this option is set to 'commit the changes will automatically commited."
  :type 'symbol
  :group 'helm)

(defgroup helm-git-branch nil
  "Fast git branch switching with Helm."
  :group 'helm)

(defcustom helm-git-branch-stash-prefix "helm-git-branch--"
  "The prefix to prepend to stash names when autostashing a branch."
  :type 'string
  :group 'helm)

(defcustom helm-git-branch-auto-save-on-change t
  ""
  :type 'boolean
  :group 'helm)

(defcustom helm-git-branch-auto-stash-unstaged t
  ""
  :type 'boolean
  :group 'helm)

(defcustom helm-git-branch-fuzzy-match nil
  "Enable fuzzy matching in `helm-git-*-branch'."
  :group 'helm-branch-git
  :set (lambda (var val)
         (set var val)
         (setq helm-source-git-local-branches nil))
  :type 'boolean)

(defmacro replace-all (from to &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (goto-char (point-min))
     (while (search-forward ,from nil t)
       (replace-match ,to))))

(defun git-make-cmd (git-args &rest cmd-args)
  (let ((cmd
         (seq-concatenate
          'list
          '(call-process "git" nil t nil) (split-string git-args) cmd-args)))
    `(lambda () ,cmd)))

(defun git-call (git-args)
  (funcall (git-make-cmd git-args)))

(defmacro with-helm-git (git-cmd &rest body)
  `(nbutlast
     (split-string
      (helm-aif (helm-ls-git-root-dir)
          (with-helm-default-directory it
            (with-output-to-string
              (with-current-buffer standard-output
                (insert (format "%s" (git-call ,git-cmd)))
                ,@body
                (buffer-string))))) "\n" t "[\s\t]*")))

(defvar helm-git-branch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    map))

(defvar helm-git-branch-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-buffer-map)
    map))

;;; Sources
(defun helm-git--local-branches ()
  (with-helm-git "branch"))

(defun helm-git-branch-local-transformer (candidates _source)
  (cl-loop with root = (helm-ls-git-root-dir)
        for i in candidates
        collect
        (cond ((string-match "^\\(*\\)\\(.*\\)" i) ; current branch
               (if (helm-git-branch--dirty-p)
                   (cons (propertize i 'face 'helm-git-branch-current-dirty-face)
                         (expand-file-name (match-string 2 i) root))
                 (cons (propertize i 'face 'helm-git-branch-current-clean-face)
                       (expand-file-name (match-string 2 i) root))))
              (t i))))

(defun helm-git-branch-local-action-transformer (actions _candidate)
  (when (= 1 (length (helm-marked-candidates)))
    (helm-make-actions "Checkout branch"
                       (lambda (_candidate)
                         (let ((marked (helm-marked-candidates)))
                           (dolist (f marked)
                             (vc-git-revert f)
                             (helm-aif (get-file-buffer f)
                                 (with-current-buffer it
                                   (revert-buffer t t)))))))
    actions))

(defclass helm-git-local-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-git--local-branches))))
   (keymap :initform 'helm-git-branch-map)
   (action :initform (helm-make-actions "Git checkout"
                                        (lambda (_candidate)
                                          (helm-git-branch--checkout _candidate))))
   (filtered-candidate-transformer :initform 'helm-git-branch-local-transformer)
   (action-transformer :initform 'helm-git-branch-local-action-transformer)))

(defun helm-git-build-local-branches-source ()
  ;;(and (memq 'helm-source-ls-git-branches helm-ls-git-default-sources)
  (helm-make-source "Local branches" 'helm-git-local-source
    :fuzzy-match helm-git-branch-fuzzy-match
    :group 'helm
    :keymap helm-git-branch-map))

(defun helm-git-branch--dirty-p ()
  (not (string-blank-p (helm-ls-git-status))))

(defun helm-git-branch--stash ()
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (insert
             (call-process "git" nil t nil "stash" "save"
                           (when helm-git-branch-auto-stash-unstaged "-u")
                           (concat helm-git-branch-stash-prefix
                                   (helm-ls-git--branch)))))))))

(defun helm-git-branch--unstash ()
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (insert (call-process "git" nil t nil "stash" "list"))
            (goto-char (point-min))
            (when (search-forward (concat helm-git-branch-stash-prefix
                                          (helm-ls-git--branch)) nil t)
              (goto-char (line-beginning-position))
              (search-forward "}" (line-end-position))
              (let ((stash (buffer-substring-no-properties
                            (line-beginning-position) (point))))
                (call-process "git" nil t nil "stash" "apply" stash)
                (call-process "git" nil t nil "stash" "drop" stash))))))))

(defun helm-git-branch--checkout (branch)
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (when helm-git-branch-auto-save-on-change
              (save-some-buffers
               t
               (lambda () (file-in-directory-p default-directory (helm-ls-git-root-dir)))))
            (when (helm-git-branch--dirty-p)
              (helm-git-branch--stash))
            (insert (call-process "git" nil t nil "checkout" branch))
            (helm-git-branch--unstash))))))

;;; Commands
;;;###autoload
(defun helm-git-local-branches (&optional arg)
  (interactive "p")
  (let ((helm-ff-default-directory
         (or helm-ff-default-directory
             default-directory)))
    (when (and arg (not (helm-ls-git-root-dir)))
      (error "Not inside a Git repository"))
    (setq helm-source-git-local-branches
          (helm-git-build-local-branches-source))
    (helm-set-local-variable
     'helm-ls-git--current-branch (helm-ls-git--branch))
    (helm :sources helm-source-git-local-branches
          :ff-transformer-show-only-basename nil
          :truncate-lines helm-buffers-truncate-lines
          :buffer "*helm branches*")))

(provide 'helm-git-branch)
;;; helm-git-branch.el ends here
