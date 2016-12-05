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


(setq header-line-format nil)
(setq mode-line-format (sd/header-line))

;; (set-face-attribute 'header-lin)

(set-face-attribute 'mode-line-inactive nil
                    :underline nil
                    :background (face-background 'default))

(defun sd/header-line ()
  (let* ((bl (or (buffer-file-name) (buffer-name)))
         (len (length bl) )
         ;; (width (* (window-total-width) (/ (float 18) 13)))
	 (width (window-total-width)))
    (when (powerline-selected-window-active)
      ;; (make-string (- (round width) 10) ?-)
      (make-string (- width 3) ?*))))


;; (mapconcat 'identity '("foo" "bar") " ")

;; (message (make-string 10 ?*))

;; (setq header-line-format mode-line-format)

(defun perspeen-tab-switch-to-buffer (buf-or-name &optional norecord force-same-window)
  "Advice to switch buffer, to update header line"
  (setq mode-line-format
	
	'(:eval
	  ;; (when (powerline-selected-window-active))
	  (sd/header-line) ;; (substring my-header
	  ;; 	   (min (length my-header)
	  ;; 		(window-hscroll)))
	  ))
   ;; (my-update-header)
  )

(advice-add 'switch-to-buffer :after #'perspeen-tab-switch-to-buffer)



(provide 'perspeen-tab)


;;; perspeen-tab.el ends here
(defun my-update-header ()
  (mapc
   (lambda (window)
     (with-current-buffer (window-buffer window)
       ;; don't mess with buffers that don't have a header line
       (when header-line-format
         (let ((original-format (get 'header-line-format 'original))
               ;; (inactive-face 'warning)
	       (inactive-face 'mode-line-inactive)) ; change this to your favorite inactive header line face
           ;; if we didn't save original format yet, do it now
           (when (not original-format)
             (put 'header-line-format 'original header-line-format)
             (setq original-format header-line-format))
           ;; check if this window is selected, set faces accordingly
           (if (eq window (selected-window))
               (setq header-line-format original-format)
             (setq header-line-format `(:propertize ,original-format face ,inactive-face)))))))
   (window-list)))
