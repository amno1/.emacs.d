;;; calendar-hr.el --- Croatian calendar for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Version: 1.0-hr
;; Keywords: calendar croatian localization

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Croatian calendar localization.

;; This work is adapted versoio of the work by D. Jensen for the Swedish
;; calendar: http://bigwalter.net/daniel/elisp/
;; 
;; The calendar modifies names of days, months, and their abbrevs from their
;; English names to Croatian names. It also adds Croatian hollidays instead of
;; US ones.
;;
;; To use this file, save it somewhere in Emacs load-path and (load
;; "calendar-hr") in your Emacs init file.

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'solar)
(require 'lunar)

(defcustom calendar-hr-prefer-slavic-names t
  "If this variable is set to `t', names used for months are traditional slavic
  names as found in croatian language: Siječanj, Veljača, etc. If this variable
  is nil, then names used are based on international names: Januar, Februar etc."
  :type 'boolean
  :group 'calendar)

(defvar calendar-month-names-genitive nil
  "Declension of month-names for genitive casus.")

;; The week starts with Monday
(setq calendar-week-start-day 1)

;; Use Europian style when printed
(setq calendar-date-style 'european)

;; Dateformat
(setq calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day ". " monthname-formatted " " year))

;; 24-hour clock without a timezone
(setq calendar-time-display-form
      '(24-hours ":" minutes))

;; get genitive casus form for the month name
(defun hr-month-filter (date)
  (catch 'done
    (let ((i 0))
      (while (< i 12)
        (when (equal date (aref calendar-month-name-array i))
          (throw 'done (aref calendar-month-names-genitive i)))
        (setq i (1+ i))))))

;; Day names
(setq calendar-day-name-array
      ["nedjelja" "ponedjeljak" "utorak" "srijeda" "četvrtak" "petak" "subota"]
      calendar-day-name-formatted-array
      ["nedjelje" "ponedjeljka" "utorka" "srijede" "četvrtka" "petka" "subote"]
      calendar-day-abbrev-array
      ["ned" "pon" "uto" "srj" "čet" "pet" "sub"]
      calendar-day-header-array
      ["nd" "pn" "ut" "sr" "čt" "pt" "sb"])

;; Month names
(setq calendar-month-name-array
      (if calendar-hr-prefer-slavic-names
          ["siječanj" "veljača" "ožujak" "travanj" "svibanj" "lipanj" "srpanj"
           "kolovoz" "rujan" "listopad" "studeni" "prosinac"]
      ["januar" "februar" "mart" "april" "maj" "jun" "jul" "avgust" "septembar"
       "oktobar" "novembar" "decembar"]))

(setq calendar-month-name-formatted-array
      (if calendar-hr-prefer-slavic-names
          ["siječnja" "veljače" "ožujka" "travnja" "svibnja" "lipnja" "srpnja"
           "kolovoza" "rujna" "listopada" "studenog" "prosinca"]
      ["januara" "februara" "marta" "aprila" "maja" "juna" "jula" "avgusta" "septembra"
       "oktobra" "novembra" "decembra"]))

(eval-after-load "solar"
  '(setq solar-n-hemi-seasons
         '("Proljetna ravnodnevica" "Ljetni suncostaj"
           "Jesenska ravnodnevica" "Zimski suncostaj")))

;; Lunar phases
(defadvice lunar-phase-name (around hr-lunar-phase-name activate)
  "Mjesečeve mjene na hrvatskom."
  (setq ad-return-value
	(let ((phase (ad-get-arg 0)))
	  (cond ((= 0 phase) "Mladi mjesec")
		((= 1 phase) "Prva četvrt")
		((= 2 phase) "Puni mjesec")
		((= 3 phase) "Posljednja četvrt")))))

;; Sunrise and sunset
(defadvice solar-sunrise-sunset-string (around hr-solar-sunrise-sunset-string
                                               activate)
  "Izlazak i zalazak Sunca na hrvatskom."
  (setq ad-return-value
        (let ((l (solar-sunrise-sunset date)))
          (format
           "%s, %s u %s (%s sati dnevnog svijetla)"
           (if (car l)
               (concat "Sunce izašlo " (apply 'solar-time-string (car l)))
             "bez sunčevog izlaska")
           (if (car (cdr l))
               (concat "Sunce zašlo " (apply 'solar-time-string (car (cdr l))))
             "bez sunčevog zalaska")
           (eval calendar-location-name)
           (car (cdr (cdr l)))))))

;; Do not show some holidays
(defvar hr-hide-some-holidays nil
  "Non-nil means some holidays won't show in the calendar.")

;; Uskršnji dan (from holiday-easter-etc)
(defun hr-easter (year)
  "Calculate the date for Easter in YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

(setq holiday-general-holidays
      '((holiday-fixed 1 1   "Nova godina")
        (holiday-fixed 1 6   "Bogojavnljenje ili Sveta tri kralja")
        (holiday-fixed 5 1   "Praznik rada")
        (holiday-fixed 5 30  "Dan državnosti")
        (holiday-fixed 6 22  "Dan antifašističke borbe")
        (holiday-fixed 8 05  "Dan pobjede i Dan hrvatskih branitelja")
        (holiday-fixed 8 15  "Velika Gospa")
        (holiday-fixed 11 1  "Svi sveti") ;; fixed holliday in Croatia 
        (holiday-fixed 11 18 "Dan sjećanja na žrtve Domovinskog rata")
        (holiday-fixed 12 25 "Božić")
        (holiday-fixed 12 26 "Sveti Stjepan")

        ;; Uskrs i Duhovi
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (hr-easter displayed-year) (car dag)))
                  (cadr dag)))
          '((  -2 "Veliki petak")
            (  -1 "Velika subota")
            (   0 "Uskrs")
            (  +1 "Uskrsni ponedjeljak")
            ( +39 "Tijelovo")
            ( +49 "Duhovi")
            ( +50 "Duhovski ponedjeljak"))))))

(setq holiday-other-holidays
      '((holiday-fixed 2 14 "Valentinovo")
        (holiday-fixed 3 8 "Internactionalni dan žena")

        ;; Još uskrsa
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (hr-easter displayed-year) (car dag)))
                  (cadr dag)))
          (if hr-hide-some-holidays
              '(( -3 "Veliki četvrtak"))
            '(( -7 "Cvjetnica")
              ( -3 "Veliki četvrtak")))))

        (unless hr-hide-some-holidays
          (holiday-fixed 4 1 "Prvi april"))
        (holiday-float 5 0 -1 "Dan majki")
        (holiday-fixed 10 24 "Dan Ujedinjenih Nacija")
        (holiday-float 11 0 2 "Dan očeva")
        (holiday-fixed 11 10 "Martinje")
        (holiday-float 12 0 -4 "Prvi advent" 24)
        (holiday-float 12 0 -3 "Drugi advent" 24)
        (holiday-float 12 0 -2 "Treći advent" 24)
        (holiday-float 12 0 -1 "Četvrti advent" 24)
        (holiday-fixed 12 10 "Nobelov dan")
        (holiday-fixed 12 13 "Dan Svete Lucije")
        (holiday-fixed 12 24 "Božić")
        (holiday-fixed 12 31 "Novogodišnja večer")))

(setq holiday-solar-holidays
      (if hr-hide-some-holidays
          nil
        '((if (fboundp 'atan)
              (solar-equinoxes-solstices))
          (if (progn
                (require 'cal-dst)
                t)
              (funcall 'holiday-sexp calendar-daylight-savings-starts
                       '(format "Ljetno vrijeme počinje %s"
                                (if
                                    (fboundp 'atan)
                                    (solar-time-string
                                     (/ calendar-daylight-savings-starts-time
                                        (float 60))
                                     calendar-standard-time-zone-name)
                                  ""))))
          (funcall 'holiday-sexp calendar-daylight-savings-ends
                   '(format "Zimsko vrijeme počinje %s"
                            (if
                                (fboundp 'atan)
                                (solar-time-string
                                 (/ calendar-daylight-savings-ends-time
                                    (float 60))
                                 calendar-daylight-time-zone-name)
                              ""))))))

(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays
              holiday-other-holidays holiday-solar-holidays))

(provide 'calendar-hr)
