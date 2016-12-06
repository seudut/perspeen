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

(defun sd/header-line ()
  (let* ((bl (or (buffer-file-name) (buffer-name)))
         (len (length bl) )
         ;; (width (* (window-total-width) (/ (float 18) 13)))
	 (width (window-total-width)))
    (when (powerline-selected-window-active)
      ;; (make-string (- (round width) 10) ?-)
      (make-string (- width 3) ?*)
      ;; (my-update-header)
      )))


(defface sd/header-line-face-inactive
  '((t ;; (:background (face-background 'default))
     (:inherit mode-line)
     ))
  "Face used to header line of inactive windows")

(defface sd/header-line-face-active
  '((t ;; (:background (face-background 'default))
     (:inherit mode-line)
     ))
  "Face used to header line of inactive windows")

;; (set-face-attribute 'sd/header-line-face-inactive nil
;; 		    :background (face-background 'default))
;; (set-face-attribute 'sd/header-line-face-active nil
;; 		    :background "red")

(defun perspeen-tab-switch-to-buffer (buf-or-name &optional norecord force-same-window)
  "Advice to switch buffer, to update header line"
  (setq header-line-format
  	'(:eval
  	  ;; (when (powerline-selected-window-active))
  	  (sd/header-line) ;; (substring my-header
  	  ;; 	   (min (length my-header)
  	  ;; 		(window-hscroll)))
  	  ))
  ;; (print "=====switch buffer======")
  (my-update-header))

;; (advice-add 'switch-to-buffer :after #'perspeen-tab-switch-to-buffer)


(defun sd/other-window (count &optional all-frames)
  "Advice to other window"
  ;; (sd/header-line)
  (my-update-header)
  ;; (perspeen-tab-switch-to-buffer)
  )

;; (advice-add 'other-window :after #'sd/other-window)




(defface sd/header-line-inactive
  '((t (:inherit mode-line)))
  "Face of header-line inactive")

(defface sd/header-line-active
  '((t (:inherit mode-line)))
  "Face of header-line active")


(defun sd/get-upper-left-most-window (&optional window)
  "Return the upper-left most window"
  (let* ((first-window (or window (frame-first-window)))
	 (right-window (window-in-direction 'right first-window)))
    (if right-window
	(sd/get-upper-left-most-window right-window)
      first-window)))

(sd/get-upper-left-most-window)

(defun sd/set-header-line-format ()
  "Set the header line format"
  (interactive)
  ;; (mapc (lambda (window)
  ;; 	  (with-current-buffer (window-buffer window)
  ;; 	    (setq header-line-format
  ;; 		  '(:eval
  ;; 		    (if (equal (selected-window) (frame-first-window))
  ;; 			(format "%s" "====First====")
  ;; 		      nil)))))
  ;; 	(window-list))
  (setq-default header-line-format
  		'(:eval
  		  (let* ((active (powerline-selected-window-active))
  			 (first-window (frame-first-window))
			 (top-left-window (sd/get-upper-left-most-window))
			 (current-window (selected-window)))

  		    (if (equal (selected-window)
  			       first-window)
			
  			(format "%s" "======FIRST======")
  		      (format "%s" "==nono==="))
  		    ;; (if active
  		    ;; 	(format "%s" "============")
  		    ;;   (format "%s" ""))
  		    ))))

(setq header-line-format nil)






(provide 'perspeen-tab)


;;; perspeen-tab.el ends here
(defun my-update-header ()
  (if (powerline-selected-window-active)
      (setq header-line-format '(:eval `(:propertize  "==active==" face sd/header-line-face-active)))
    (setq header-line-format '(:eval `(:propertize "|inactive|" face sd/header-line-face-inactive))))

  (mapc
   (lambda (window)
     (print (buffer-name (window-buffer window)))
     (with-current-buffer (window-buffer window)
       ;; don't mess with buffers that don't have a header line
       (when header-line-format
         (let (	;; (original-format (get 'header-line-format 'original))
  	       ;; (original-format `(:propertize "====peli3=====" face 'default))
  	       (original-format `(:propertize "====peli3====="))
               ;; (inactive-face 'warning)
  	       (inactive-face `(:propertize "|||" face sd/header-line-face-inactive))
  	       ;; (inactive-face 'mode-line-inactive)
  	       ) ; change this to your favorite inactive header line face
           ;; if we didn't save original format yet, do it now
           ;; (when (not original-format)
           ;;   (put 'header-line-format 'original header-line-format)
           ;;   (setq original-format header-line-format))
           ;; check if this window is selected, set faces accordingly
           (if (powerline-selected-window-active) ;; (eq window (selected-window))
  	       (progn
  		 (message "=Activate is==%s===" (format (buffer-name)))
  		 ;; (setq header-line-format original-format)
  		 (setq header-line-format '(:eval `(:propertize  "==active==" face sd/header-line-face-active))))
  	     (message "=No activate is==%s===" (format (buffer-name)))
  	     ;; (setq header-line-format inactive-face)
  	     (setq header-line-format '(:eval `(:propertize "|inactive|" face sd/header-line-face-inactive)))
  	     ;; (print (buffer-name))
             ;; (setq header-line-format `(:propertize ,original-format face ,inactive-face))
  	     )))))
   (window-list))
  )


;; (mapc (lambda (window)
;; 	(with-current-buffer (window-buffer window)
;; 	  (print (buffer-name))))
;;       (window-list))
