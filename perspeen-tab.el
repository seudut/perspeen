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

(defface sd/header-line-inactive
  '((t (:inherit mode-line)))
  "Face of header-line inactive")

(defface sd/header-line-active
  '((t (:inherit mode-line)))
  "Face of header-line active")


(defun sd/get-upper-left-most-window ()
  "Return the upper-left most window"
  (window-at (frame-width) 0))

(defface sssdd/powerline-inactive1
  '((t (:background "grey11" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(make-variable-frame-local
 (defvar perspeen-tab-confs nil
   "The configurations of all tabs
((tab1 . conf1) (tab2 . conf2) ... (tabn . confn))

"))

(defun perspeen-tab-set-tabs-configuration ()
  "Set the configuration of tabs."
  ())


(defun perspeen-tab-new-tab-internal ()
  "New tabs."
  ())

(defun perspeen-tab-create-tab ()
  "Create a new tab."
  (interactive)
  ())


(defun sd/construct-header-line ()
  "Generate header line"
  (let* ((active (powerline-selected-window-active))
	 (first-window (frame-first-window))
	 (top-right-window (sd/get-upper-left-most-window))
	 (current-window (selected-window))
	 (separator-right (intern (format "powerline-%s-%s"
					  (powerline-current-separator)
					  (cdr powerline-default-separator-dir))))
	 (separator-left (intern (format "powerline-%s-%s"
					 (powerline-current-separator)
					 (car powerline-default-separator-dir))))
	 (lhs (list
	       (powerline-raw (format "%s" "==first buffer== ") 'sd/powerline-active1 'l)
	       (funcall separator-left 'sd/powerline-active1 'sssdd/powerline-inactive1)))
	 (rhs (list
	       (funcall separator-right 'sssdd/powerline-inactive1 'sd/powerline-active1)
	       (powerline-raw (format-time-string " %Y-%m-%d %I:%M %p %a ") 'sd/powerline-active1 'r))))
    (cond ((eq current-window first-window)
	   (if (eq first-window top-right-window)
	       (concat
		(powerline-render lhs)
		(powerline-fill 'sssdd/powerline-inactive1 (powerline-width rhs))
		(powerline-render rhs))
	     (concat
	      (powerline-render lhs)
	      (powerline-fill 'sssdd/powerline-inactive1 0))))
	  ((eq current-window top-right-window)
	   (concat
	    (powerline-fill 'sssdd/powerline-inactive1 (powerline-width rhs))
	    (powerline-render rhs)))
	  (t
	   nil))))

(defun sd/set-header-line-format (&optional force)
  "Set the header line format"
  (if force
      (setq header-line-format
		    '(:eval
		      (sd/construct-header-line)))
    (setq-default header-line-format
		    '(:eval
		      (sd/construct-header-line)))))

(sd/set-header-line-format)

(add-hook 'post-command-hook (lambda () (sd/set-header-line-format t)))




;;;###autoload

(defun perspeen-tab-start ()
  "Start perspeen tab."
  (interactive)
  ())

(provide 'perspeen-tab)
;;; perspeen-tab.el ends here

