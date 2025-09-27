;;; dired-setup.el --- Setup for Dired buffer   -*- lexical-binding: t; -*-

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

;;;; External

(require 'seq)
(require 'dired)
(require 'wdired)
(require 'dired-x)
(require 'openwith)
(require 'dired-aux)
(require 'config-macs)
(require 'dired-async)
(require 'markdown-mode)
(require 'org-view-mode)
(require 'dired-subtree)
(require 'org-pretty-table)
(require 'dired-copy-paste)
(require 'dired-git-log)
(require 'term-toggle)
(require 'dired-auto-readme)
(require 'tmtxt-dired-async)

;; Help Functions
;; quick-hack - need to rewrite this
(defun my-run ()
  (interactive)
  (let ((file (expand-file-name (dired-get-file-for-visit))))
    (start-process file file file)))

(defun dired-mark-backward ()
  (interactive)
  (call-interactively 'dired-mark)
  (dired-previous-line 2))

(defun dired-goto-first ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1)
  (dired-move-to-filename))

(defun dired-goto-last ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1)
  (dired-move-to-filename))

(defun dired-dirline-p ()
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at dired-re-dir)))

(defun dired-subtree-expand-tree ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename)
        (when (dired-dirline-p)
          (unless (dired-subtree--is-expanded-p)
            (dired-subtree-insert)
            (dired-prev-dirline 1))))
      (dired-next-dirline 1))))

(defun dired-subtree-expand-all ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (not (bobp))
      (ignore-errors
        (unless (dired-subtree--is-expanded-p)
          (dired-subtree-insert)))
      (dired-previous-line 1))))

(defun dired-subtree-fold-all ()
  (interactive)
  (save-excursion
    (dired-goto-last)
    (while (not (bobp))
      (dired-next-line -1)
      (ignore-errors
        (unless (dired-subtree--is-expanded-p)
          (dired-subtree-remove))))))

(defun dired-in-vertical-split ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (dired-jump))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun dired-open-current-as-sudo ()
  "open current file as sudo"
  (interactive)
  (message "!!! SUDO opening %s"
           (dired-file-name-at-point))
  (sudo-find-file (dired-file-name-at-point)))

(defun dired-disable-line-wrap ()
  "disable line wrapping in dired-mode"
  (auto-fill-mode -1))
(add-hook 'dired-mode-hook 'dired-disable-line-wrap)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
  If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Prevent dired to write to modeline.
;; Built-in version dumps entire content of dired-listing-switches to modeline
;; which pushes everything fat to the right and makes modeline literally
;; worthless. I really don't need ot see ls switches on my modeline, so I have
;; rewrote the function to not dump switches at all.
(defvar dired-display-ls-switches nil
  "Non-nil meands the Dired will display current ls-switches on modeline.")
(defun dired-sort-set-mode-line ()  
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match-p
        	    dired-sort-by-name-regexp dired-actual-switches)
        	   "Dired by name")
        	  ((string-match-p
        	    dired-sort-by-date-regexp dired-actual-switches)
        	   "Dired by date")
        	  ((eq dired-display-ls-switches t)
        	   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))

(defun dired-mark-empty-dirs ()
  "Interactively mark all empty directories in current Dired buffer."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (ignore-errors
          (when (directory-empty-p (dired-get-filename))
            (dired-mark 1)
            (dired-previous-line 1)))
        (dired-next-line 1)))))

(defun dired-delete-empty-dirs ()
  "Interactively mark all empty directories in current Dired buffer."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (ignore-errors
          (when (directory-empty-p (dired-get-filename))
            (delete-directory (dired-get-filename))))
        (dired-next-line 1)))))

(defvar du-program
  (if (eq system-type 'windows-nt)
      "c:/msys64/usr/bin/du.exe"
    "/usr/bin/du"))

(defun du-size (files)
  (with-temp-buffer
    (apply 'call-process du-program nil t nil "-sch" files)
    (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$" nil t)
    (or (ignore-errors (match-string 1)) 0)))

(defun dired-get-size ()
  (interactive)
  (let* ((files (dired-get-marked-files))
         (size (du-size files)))
    (message
     (if (= 1 (length files))
         (format "Size of %s: %s" (car files) size)
       (format "Size of all marked files: %s" size)))))

(defun dired-directory-get-size ()
  (interactive)
  (let* ((files (directory-files
                default-directory
                t directory-files-no-dot-files-regexp))
         (size (du-size files)))
    (message "Size of %s: %s" default-directory size)))

(defun dired-directory-tree-get-size ()
  (interactive)
  (let* ((files (directory-files-recursively
                 default-directory
                 directory-files-no-dot-files-regexp))
         (size (du-size files)))
    (message "Total size of dirtree %s: %s" default-directory size)))


(defun dired-find-dups ()
  (unless (or (eq major-mode 'dired-mode) (eq major-mode 'wdired-mode))
              (error "Not in dired or wdired mode."))
  (goto-char (point-min))
  (let (files dups file)
    (while (dired-next-line 1)
      (setq file (dired-get-filename))
      (if (and file (seq-contains-p files file))
          (push file dups)
        (push file files)))
    dups))

(defun dired-change-filename ()
  "Rename file at point."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "This command runs only  in Dired mode."))
  (when-let* ((filename (dired-file-name-at-point)))
    (dired-goto-next-file)
    (set-mark (point))
    (goto-char (line-end-position))
    (search-backward ".")
    (wdired-change-to-wdired-mode)))

;; The basic function for half a dozen variations on cp/mv/ln/ln -s
;; in dired.el
(defun dired-create-files (file-creator operation fn-list name-constructor
                    &optional marker-char)
  "Create one or more new files from a list of existing files FN-LIST.
This function also handles querying the user, updating Dired
buffers, and displaying a success or failure message.

FILE-CREATOR should be a function.  It is called once for each
file in FN-LIST, and must create a new file, querying the user
and updating Dired buffers as necessary.  It should accept three
arguments: the old file name, the new name, and an argument
OK-IF-ALREADY-EXISTS with the same meaning as in `copy-file'.

OPERATION should be a capitalized string describing the operation
performed (e.g. `Copy').  It is used for error logging.

FN-LIST is the list of files to copy (full absolute file names).

NAME-CONSTRUCTOR should be a function accepting a single
argument, the name of an old file, and returning either the
corresponding new file name or nil to skip.

If optional argument MARKER-CHAR is non-nil, mark each
newly-created file's Dired entry with the character MARKER-CHAR,
or with the current marker character if MARKER-CHAR is t."
  (let (dired-create-files-failures failures
    skipped (success-count 0) (total (length fn-list)))
    (let (to)    ; for dired-handle-overwrite
      (dolist (from fn-list)
        ;; Position point on the current file -- this is useful if
        ;; handling a number of files to show where we're working at.
        (dired-goto-file from)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form (format-message
                                         (substitute-command-keys "\
Type \\`SPC' or \\`y' to overwrite file `%s',
\\`DEL' or \\`n' to skip to next,
\\`ESC' or \\`q' to not overwrite any of the remaining files,
\\`!' to overwrite all remaining files with no more questions.") to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
          ;; If DESTNAME is a subdirectory of FROM, not a symlink,
          ;; and the method in use is copying, signal an error.
          (and (eq t (file-attribute-type (file-attributes destname)))
           (eq file-creator 'dired-copy-file)
           (file-in-directory-p destname from)
           (error "Cannot copy `%s' into its subdirectory `%s'"
              from to)))
            ;; Check, that `dired-do-symlink' does not create symlinks
            ;; on different hosts.
            (when (and (eq file-creator 'make-symbolic-link)
                       (not (equal (file-remote-p from) (file-remote-p to))))
          (error "Cannot symlink `%s' to `%s' on another host" from to))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d: %s -> %s"
                           operation success-count total from to)
                  (dired-add-file to actual-marker-char))
              (file-error       ; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s: `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format (ngettext "%s failed for %d file in %d requests"
             "%s failed for %d files in %d requests"
             (length failures))
           operation (length failures) total)
       failures))
     (failures
      (dired-log-summary
       (format (ngettext "%s: %d of %d file failed"
             "%s: %d of %d files failed"
             total)
           operation (length failures) total)
       failures))
     (skipped
      (dired-log-summary
       (format (ngettext "%s: %d of %d file skipped"
             "%s: %d of %d files skipped"
             total)
           operation (length skipped) total)
       skipped))
     (t
      (message (ngettext "%s: %d file done"
             "%s: %d files done"
             success-count)
           operation success-count))))
  (dired-move-to-filename))

;; Setup
(on-system gnu/linux
    (dolist (ext (list (list (openwith-make-extension-regexp
                              '("xbm" "pbm" "pgm" "ppm" "pnm"
                                "png" "gif" "bmp" "tif" "jpeg" "jpg"))
                             "feh"
                             '(file))
                       (list (openwith-make-extension-regexp
                              '("doc" "xls" "ppt" "odt" "ods" "odg" "odp" "rtf"))
                             "libreoffice"
                             '(file))
                       (list (openwith-make-extension-regexp
                              '("\\.lyx"))
                             "lyx"
                             '(file))
                       (list (openwith-make-extension-regexp
                              '("chm"))
                             "kchmviewer"
                             '(file))
                       (list (openwith-make-extension-regexp
                              '("pdf" "ps" "ps.gz" "dvi" "epub" "djv" "djvu" "mobi" "azw3"))
                             "okular"
                             '(file))))
      (add-to-list 'openwith-associations ext)))

(setq dired-dwim-target t
      global-auto-revert-non-file-buffers nil
      dired-recursive-copies  'always
      dired-recursive-deletes 'always
      dired-listing-switches "-lA --si --time-style=long-iso --group-directories-first"
      wdired-use-dired-vertical-movement t
      wdired-allow-to-change-permissions t
      dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
              
(dolist (ext  (list (list (openwith-make-extension-regexp
                           '("flac" "mpg" "mpeg" "mp3" "mp4"
                             "avi" "wmv" "wav" "mov" "flv" "ts" "m4b"
                             "ogm" "ogg" "mkv" "webm" "m4v" "m4a"))
                          "mpv"
                          '(file))
                    (list (openwith-make-extension-regexp
                           '("html" "htm" "pdf" "xml"))
                          (getenv "BROWSER")
                          '(file))))
  (add-to-list 'openwith-associations ext))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(defkeys dired-mode-map
    "C-x <M-S-return>" dired-open-current-as-sudo
    "r"                dired-do-rename
    "C-S-r"            wdired-change-to-wdired-mode
    "C-r C-r"          tda/rsync
    "C-r C-z"          tda/zip
    "C-r C-u"          tda/unzip
    "C-r C-a"          tda/rsync-multiple-mark-file
    "C-r C-e"          tda/rsync-multiple-empty-list
    "C-r C-d"          tda/rsync-multiple-remove-item
    "C-r C-v"          tda/rsync-multiple
    "C-r C-s"          tda/get-files-size
    "C-r C-q"          tda/download-to-current-dir
    "C-x C-j"          dired-jump
    "C-x 4 C-j"        dired-jump-other-window
    "S-<return>"       dired-openwith
    "n"                scroll-up-line
    "p"                scroll-down-line
    "M-m"              dired-mark-backward
    "M-<"              dired-goto-first
    "M->"              dired-goto-last
    "M-<return>"       my-run
    "C-S-f"            dired-narrow
    "P"                peep-dired
    "<f1>"             term-toggle-term
    "TAB"              dired-subtree-toggle
    "f"                dired-subtree-fold-all
    "z"                dired-get-size
    "e"                dired-subtree-expand-all)

(provide 'dired-setup)
;;; dired-setup.el ends here
