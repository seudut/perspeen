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
	       (funcall separator-left 'sd/powerline-active1 'powerline-active1)))
	 (rhs (list
	       (funcall separator-right  'powerline-active1 'sd/powerline-active1)
	       (powerline-raw (format-time-string " %Y-%m-%d %I:%M %p %a ") 'sd/powerline-active1 'r))))
     (cond ((eq current-window first-window)
	   (if (eq first-window top-right-window)
	       (concat
		(powerline-render lhs)
		(powerline-fill 'powerline-active1 (powerline-width rhs))
		(powerline-render rhs))
	     (concat
	      (powerline-render lhs)
	      (powerline-fill 'powerline-active1 0))))
	  ((eq current-window top-right-window)
	   (concat
	    (powerline-fill 'powerline-active1 (powerline-width rhs))
	    (powerline-render rhs)))
	  (t
	   nil))))

(defun sd/set-header-line-format (&optional force)
  "Set the header line format"
  (if force
      (setq header-line-format
		    '(:eval
		      (sd/construct-header-line)))
    (setq header-line-format
		    '(:eval
		      (sd/construct-header-line)))))

(sd/set-header-line-format)
;; (setq header-line-format nil)

(defun sd/switch-to-buffer-header-line (buf-or-name &optional norecord force-same-window)
  "Advice"
  (when buf-or-name
    (message "====switch--buffer===")
    (sd/set-header-line-format)))
;; (advice-add 'switch-to-buffer :after #'sd/switch-to-buffer-header-line)

;; (add-hook 'after-change-major-mode-hook #'sd/set-header-line-format)

(defun sd/haha ()
  "jfieojfo"
  (message "-----test------"))

(add-hook 'change-major-mode-hook #'sd/set-header-line-format)

(mapc (lambda (window)
	(with-selected-window window
	  (sd/set-header-line-format)))
      (window-list))

(force-window-update t)
(walk-windows (lambda (window)
		(with-selected-window window
		  (sd/set-header-line-format t))))

(provide 'perspeen-tab)


;;; perspeen-tab.el ends here
;; (defun my-update-header ()
;;   (if (powerline-selected-window-active)
;;       (setq header-line-format '(:eval `(:propertize  "==active==" face sd/header-line-face-active)))
;;     (setq header-line-format '(:eval `(:propertize "|inactive|" face sd/header-line-face-inactive))))

;;   (mapc
;;    (lambda (window)
;;      (print (buffer-name (window-buffer window)))
;;      (with-current-buffer (window-buffer window)
;;        ;; don't mess with buffers that don't have a header line
;;        (when header-line-format
;;          (let (	;; (original-format (get 'header-line-format 'original))
;;   	       ;; (original-format `(:propertize "====peli3=====" face 'default))
;;   	       (original-format `(:propertize "====peli3====="))
;;                ;; (inactive-face 'warning)
;;   	       (inactive-face `(:propertize "|||" face sd/header-line-face-inactive))
;;   	       ;; (inactive-face 'mode-line-inactive)
;;   	       ) ; change this to your favorite inactive header line face
;;            ;; if we didn't save original format yet, do it now
;;            ;; (when (not original-format)
;;            ;;   (put 'header-line-format 'original header-line-format)
;;            ;;   (setq original-format header-line-format))
;;            ;; check if this window is selected, set faces accordingly
;;            (if (powerline-selected-window-active) ;; (eq window (selected-window))
;;   	       (progn
;;   		 (message "=Activate is==%s===" (format (buffer-name)))
;;   		 ;; (setq header-line-format original-format)
;;   		 (setq header-line-format '(:eval `(:propertize  "==active==" face sd/header-line-face-active))))
;;   	     (message "=No activate is==%s===" (format (buffer-name)))
;;   	     ;; (setq header-line-format inactive-face)
;;   	     (setq header-line-format '(:eval `(:propertize "|inactive|" face sd/header-line-face-inactive)))
;;   	     ;; (print (buffer-name))
;;              ;; (setq header-line-format `(:propertize ,original-format face ,inactive-face))
;;   	     )))))
;;    (window-list)))


;; (mapc (lambda (window)
;; 	(with-current-buffer (window-buffer window)
;; 	  (print (buffer-name))))
;;       (window-list))
