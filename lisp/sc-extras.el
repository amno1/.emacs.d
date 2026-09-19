;;; sc-extras.el --- Some Scheme extras              -*- lexical-binding: t; -*-

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

;; 

;;; Code:


;;;###autoload
(defun guile-repl ()
  (interactive)
  (require 'geiser)
  (require 'geiser-guile)
  (call-interactively #'geiser-guile))

;;;###autoload
(defun racket-repl ()
  (interactive)
  (require 'geiser)
  (require 'geiser-racket)
  (call-interactively #'geiser-racket))

(provide 'sc-extras)
;;; sc-extras.el ends here
