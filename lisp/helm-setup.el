;;; helm-setup.el --- My helm config                 -*- lexical-binding: t; -*-

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

(require 'config-macs)
(require 'cl-print)
(require 'helm)
(require 'eshell)
(require 'helm-files)
(require 'helm-eshell)
(require 'helm-comint)
(require 'helm-buffers)
(require 'helm-adaptive)

(on-hook helm-mode
    ;; (helm-flx-mode +1)    
    (diminish 'helm-mode)
    (helm-adaptive-mode 1))

;; (on-hook helm-ff-cache-mode
;;   (diminish 'helm-ff-cache-mode))

(defvar helm-source-header-default-background (face-attribute
                                               'helm-source-header :background)) 
(defvar helm-source-header-default-foreground (face-attribute
                                               'helm-source-header :foreground)) 
(defvar helm-source-header-default-box (face-attribute
                                        'helm-source-header :box))
(set-face-attribute 'helm-source-header nil :height 0.1)

(defun helm-toggle-header-line ()
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-source-header-default-foreground
                          :background helm-source-header-default-background
                          :box helm-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))

(setq helm-completion-style             'emacs
      helm-display-header-line              nil
      ;; helm-completion-in-region-fuzzy-match t
      ;; helm-recentf-fuzzy-match              t
      ;; helm-buffers-fuzzy-matching           t
      ;; helm-locate-fuzzy-match               t
      ;; helm-lisp-fuzzy-completion            t
      ;; helm-session-fuzzy-match              t
      ;; helm-apropos-fuzzy-match              t
      ;; helm-imenu-fuzzy-match                t
      ;; helm-semantic-fuzzy-match             t
      ;; helm-M-x-fuzzy-match                  t
      helm-split-window-inside-p            t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-ff-auto-update-initial-value     nil
      helm-net-prefer-curl                  t
      helm-autoresize-max-height            0
      helm-autoresize-min-height           30
      helm-candidate-number-limit         100
      helm-idle-delay                     0.0
      helm-input-idle-delay               0.0
      switch-to-prev-buffer-skip         'helm-buffer-p
      helm-ff-cache-mode-lighter-sleep    nil
      helm-ff-cache-mode-lighter-updating nil
      helm-ff-cache-mode-lighter          nil
      helm-ff-skip-boring-files            t)

(defkeys helm-map
  "M-i" helm-previous-line
  "M-k" helm-next-line
  "M-I" helm-previous-page
  "M-K" helm-next-page
  "M-h" helm-beginning-of-buffer
  "M-H" helm-end-of-buffer)

(on-hook eshell-mode-hook
  (defkeys eshell-mode-map
    "C-c C-h" helm-eshell-history
    "C-c C-r" helm-comint-input-ring
    "C-c C-l" helm-minibuffer-history))

(defun helm-buffer-p (window new-buffer bury-or-kill)
  "Returns T when NEW-BUFFER's name matches any regex in
`helm-boring-buffer-regexp-list'."
  (catch 'helm-p
    (dolist (regex helm-boring-buffer-regexp-list)
      (if (string-match-p regex (buffer-name new-buffer))
          (throw 'helm-p t)))))

(defvar helm-source-header-default-background (face-attribute
                                               'helm-source-header :background))
(defvar helm-source-header-default-foreground (face-attribute
                                               'helm-source-header :foreground))
(defvar helm-source-header-default-box (face-attribute
                                        'helm-source-header :box))
(set-face-attribute 'helm-source-header nil :height 0.1)
(defun helm-toggle-header-line ()
  (if (> (length helm-sources) 1)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground helm-source-header-default-foreground
                          :background helm-source-header-default-background
                          :box helm-source-header-default-box
                          :height 1.0)
    (set-face-attribute 'helm-source-header
                        nil
                        :foreground (face-attribute 'helm-selection :background)
                        :background (face-attribute 'helm-selection :background)
                        :box nil
                        :height 0.1)))
(helm-autoresize-mode 1)
(helm-adaptive-mode t)
(helm-mode 1)
(add-to-list 'helm-sources-using-default-as-input
             'helm-source-man-pages)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-bookmarks
                                  helm-source-recentf
                                  helm-source-buffer-not-found
                                  projectile-known-projects))

(setq helm-completion-style             'emacs
      helm-display-header-line              nil
      ;; helm-completion-in-region-fuzzy-match t
      ;; helm-recentf-fuzzy-match              t
      ;; helm-buffers-fuzzy-matching           t
      ;; helm-locate-fuzzy-match               t
      ;; helm-lisp-fuzzy-completion            t
      ;; helm-session-fuzzy-match              t
      ;; helm-apropos-fuzzy-match              t
      ;; helm-imenu-fuzzy-match                t
      ;; helm-semantic-fuzzy-match             t
      ;; helm-M-x-fuzzy-match                  t
      helm-split-window-inside-p            t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-ff-auto-update-initial-value     nil
      helm-net-prefer-curl                  t
      helm-autoresize-max-height            0
      helm-autoresize-min-height           30
      helm-candidate-number-limit         100
      helm-idle-delay                     0.0
      helm-input-idle-delay               0.0
      switch-to-prev-buffer-skip         'helm-buffer-p
      helm-ff-cache-mode-lighter-sleep    nil
      helm-ff-cache-mode-lighter-updating nil
      helm-ff-cache-mode-lighter          nil
      helm-ff-skip-boring-files            t)

(dolist (regexp '("\\`\\*direnv" "\\`\\*straight" "\\`\\*xref"))
  (push regexp helm-boring-buffer-regexp-list))
  

(helm-autoresize-mode 1)
(helm-adaptive-mode t)
(helm-mode 1)
(add-to-list 'helm-sources-using-default-as-input
             'helm-source-man-pages)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-bookmarks
                                  helm-source-recentf
                                  helm-source-buffer-not-found
                                  projectile-known-projects))

(defkeys helm-map
  "M-i" helm-previous-line
  "M-k" helm-next-line
  "M-I" helm-previous-page
  "M-K" helm-next-page
  "M-h" helm-beginning-of-buffer
  "M-H" helm-end-of-buffer)

(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "C-c C-l") #'helm-comint-input-ring))

(on-hook helm-mode-hook
  ;; (helm-flx-mode +1)
  (diminish 'helm-mode)
  (helm-adaptive-mode 1))

(defun helm-follow--kill-orphan-timer ()
  "Cancel `helm-follow-mode' idle timer if it is still around."
  (when (bound-and-true-p helm-follow--timer)
    (when (timerp helm-follow--timer)
      (cancel-timer helm-follow--timer))
    (setq helm-follow--timer nil)))

(add-hook 'helm-cleanup-hook #'helm-follow--kill-orphan-timer)
(add-hook 'helm-end-hook     #'helm-follow--kill-orphan-timer)

(defun helm-execute-persistent-action--guard (orig &rest args)
  (if (and (bound-and-true-p helm-buffer)
           (buffer-live-p helm-buffer)
           (get-buffer-window helm-buffer))
      (apply orig args)
    ;; We're outside a live helm session — kill the orphan and stay quiet.
    (helm-follow--kill-orphan-timer)))

;; (advice-add 'helm-execute-persistent-action :around
;;             #'helm-execute-persistent-action--guard)

;; (advice-remove 'helm-execute-persistent-action
;;                #'helm-execute-persistent-action--guard)

(provide 'helm-setup)
;;; helm-setup.el ends here
