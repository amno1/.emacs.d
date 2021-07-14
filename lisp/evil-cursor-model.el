
(require 'evil)

;; Use Emacs' cursor-between-characters model of cursor positioning in
;; `evil-mode' instead of Vim's normal-mode cursor-at-characters model.


;; cursor-related `evil-mode' settings
(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol t)
(setq evil-highlight-closing-paren-at-point-states nil)

;; motion command rebindings
(evil-define-key 'normal 'global
  "p"  #'evil-paste-before)
(evil-define-key 'motion 'global
  "t"  #'evil-find-char
  "T"  #'evil-find-char-to-backward
  "f"  #'evil-find-char-after
  "F"  #'evil-find-char-backward
  "e"  #'evil-forward-after-word-end
  "E"  #'evil-forward-after-WORD-end
  "ge" #'evil-backward-after-word-end
  "gE" #'evil-backward-after-WORD-end
  "%"  #'evil-jump-item-before)


;; --- evil commands implementing Emacs' cursor model ---

(evil-define-motion evil-find-char-after (count char)
  "Move point immediately after the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (unless count (setq count 1))
  (if (< count 0)
      (evil-find-char-backward (- count) char)
    (when (= (char-after) char)
      (forward-char)
      (cl-decf count))
    (evil-find-char count char)
    (forward-char))
  (setq evil-last-find (list #'evil-find-char-after char (> count 0))))


(defun evil-forward-after-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (forward-thing thing count))
   (t
    (unless (bobp) (forward-char -1))
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when bnd
	(cond
	 ((< (point) (cdr bnd)) (goto-char (car bnd)))
	 ((= (point) (cdr bnd)) (cl-incf count))))
      (condition-case nil
          (when (zerop (setq rest (forward-thing thing count)))
            (end-of-thing thing))
        (error))
      rest))))

(defun evil-backward-after-end (thing &optional count)
  "Move backward to end of THING.
The motion is repeated COUNT times. This is the same as calling
`evil-forward-after-word-end' with -COUNT."
  (evil-forward-after-end thing (- (or count 1))))


(evil-define-motion evil-forward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (evil-forward-after-end thing count)))

(evil-define-motion evil-forward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (evil-forward-after-word-end count t))

(evil-define-motion evil-backward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-after-end thing count)))

(evil-define-motion evil-backward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (evil-backward-after-word-end count t))



;; redefine `inclusive' motion type to not include character after point
(evil-define-type inclusive
  "Return the positions unchanged, with some exceptions.
If the end position is at the beginning of a line, then:

* If the beginning position is at or before the first non-blank
  character on the line, return `line' (expanded)."
  :expand (lambda (beg end) (evil-range beg end))
  :contract (lambda (beg end) (evil-range beg end))
  :normalize (lambda (beg end)
               (cond
                ((progn
                   (goto-char end)
                   (and (/= beg end) (bolp)))
                 (setq end (max beg (1- end)))
                 (cond
                  ((progn
                     (goto-char beg)
                     (looking-back "^[ \f\t\v]*" (line-beginning-position)))
                   (evil-expand beg end 'line))
                  (t
                   (unless evil-cross-lines
                     (setq end (max beg (1- end))))
                   (evil-expand beg end 'inclusive))))
                (t
                 (evil-range beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))


;; make "e" search offset put point after last character
(defun ad-evil-ex-search-adjust-offset (offset)
  (unless (zerop (length offset))
    (save-match-data
      (string-match
       "^\\([esb]\\)?\\(\\([-+]\\)?\\([0-9]*\\)\\)$"
       offset)
      (when (and (= (aref offset (match-beginning 1)) ?e)
		 (not (bobp)))
	(forward-char 1)))))

(advice-add 'evil-ex-search-goto-offset :after #'ad-evil-ex-search-adjust-offset)


;; make `evil-jump-item' move point just after matching delimeter if it jumps forward
(evil-define-motion evil-jump-item-before (count)
    "Find the next item in this line immediately before
or somewhere after the cursor and jump to the corresponding one."
    :jump t
    :type inclusive
    (let ((pos (point)))
      (unless (or (bolp) (bobp)) (backward-char))
      (condition-case nil
	  (evil-jump-item count)
	('user-error (goto-char pos)))
      (unless (< (point) pos)
	(goto-char pos)
	(evil-jump-item count)
	(when (> (point) pos) (forward-char)))))
