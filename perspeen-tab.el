;;; perspeen-tab.el --- TAB for perspeen             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Peng Li

;; Author: Peng Li <seudut@gmail.com>
;; Keywords: lisp

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

;; 

;;; Code:

(window-total-width)
(window-height)
(window-text-pixel-size)
(setq window-right-divider-width  20)
;; (defconst my-header
;;   "====This is h111ead for test================================================================================================================================================================================")

(window-total-width)
(window-width)
(frame-char-height)

(window-header-line-height)


(setq header-line-format mode-line-format)
(setq header-line-format (sd/header-line))



(defun sd/header-line ()
  (let* ((bl (or (buffer-file-name) (buffer-name)))
         (len (length bl) )
         ;; (width (* (window-total-width) (/ (float 18) 13)))
	 (width (window-total-width)))
    ;; (make-string (- (round width) 10) ?-)
    (make-string (- width 3) ?-)))


(mapconcat 'identity '("foo" "bar") " ")

(message (make-string 10 ?*))

(setq header-line-format mode-line-format)

(defun perspeen-tab-switch-to-buffer (buf-or-name &optional norecord force-same-window)
  "Advice to switch buffer, to update header line"
  (setq header-line-format
	'(:eval (sd/header-line) ;; (substring my-header
		;; 	   (min (length my-header)
		;; 		(window-hscroll)))
		 )))

(advice-add 'switch-to-buffer :after #'perspeen-tab-switch-to-buffer)


(provide 'perspeen-tab)
;;; perspeen-tab.el ends here
