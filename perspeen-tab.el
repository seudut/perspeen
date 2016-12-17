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


;; (defvar perspeen-tab-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "s-n") 'perspeen-tab-next)
;;     (define-key map (kbd "s-c") 'perspeen-tab-next)
;;     (define-key map (kbd "s-p") 'perspeen-tab-prev)
;;     map)
;;   "Keymap for perspeen-tab")

(make-variable-frame-local
 (defvar perspeen-tab-configurations nil
   "The configurations of all tabs.
It has all the tabs, which tab has a property list of
window-configuration and point-mark"))

(cl-defstruct (perspeen-tab-conf)
  tabs
  (current-tab 0)
  (last-tab 0))

(defun perspeen-tab-get-tabs ()
  "Get tabs in perspeen-tab-configurations"
  (perspeen-tab-conf-tabs perspeen-tab-configurations))

(defun perspeen-tab-get-current-tab-index ()
  "Get current tab in perspeen-tab-configurations"
  (perspeen-tab-conf-current-tab perspeen-tab-configurations))

(defun perspeen-tab-get-current-tab ()
  "Get current tab."
  (nth (perspeen-tab-get-current-tab-index) (perspeen-tab-get-tabs)))

(defun perspeen-tab-set-tabs-configuration (conf)
  "Set the configuration of tabs."
  ;; (unless (perspeen-tab-get-current-tab)
  ;;   (put (perspeen-tab-get-current-tab) 'current-buffer (current-buffer)))
  (setq perspeen-tab-configurations conf))

(defun perspeen-tab-get-tabs-configuration ()
  "Get the tabs configurations"
  perspeen-tab-configurations)

(defun perspeen-tab-new-tab-internal ()
  "New tabs."
  (let ((tab (make-symbol "perspeen-tab")))
    (put tab 'window-configuration (current-window-configuration))
    (put tab 'point-marker (point-marker))
    (put tab 'current-buffer (current-buffer))
    (push tab (perspeen-tab-conf-tabs perspeen-tab-configurations))))

(defun perspeen-tab-save-configuration ()
  "Save the tab configuration."
  (let ((current-tab (perspeen-tab-get-current-tab)))
    (put current-tab 'window-configuration (current-window-configuration))
    (put current-tab 'point-marker (point-marker))
    (put current-tab 'current-buffer (current-buffer))))

(defun perspeen-tab-apply-configuration ()
  "Apply the tab configuration."
  (let ((current-tab (perspeen-tab-get-current-tab)))
    (set-window-configuration (get current-tab 'window-configuration))
    (goto-char (get current-tab 'point-marker))))

(defun perspeen-tab-switch-internal (index)
  "Switch tabs."
  (let ((current-tab (perspeen-tab-get-current-tab)))
    ;; save
    (put current-tab 'window-configuration (current-window-configuration))
    (put current-tab 'point-marker (point-marker))
    (put current-tab 'current-buffer (current-buffer))
    ;; set 
    (setf (perspeen-tab-conf-current-tab perspeen-tab-configurations) index)
    ;; pop
    (setq current-tab (perspeen-tab-get-current-tab))
    (set-window-configuration (get current-tab 'window-configuration))
    (goto-char (get current-tab 'point-marker))
    (put current-tab 'current-buffer (current-buffer))))

(defun perspeen-tab-create-tab ()
  "Create a new tab."
  (interactive)
  (perspeen-tab-new-tab-internal)
  ;; (perspeen-tab-switch-internal (-  (length (perspeen-tab-get-tabs)) 1))
  )

(defun perspeen-tab-next ()
  "Switch to next tab."
  (interactive)
  (let ((next (+ (perspeen-tab-get-current-tab-index) 1)))
    (if (>= next (length (perspeen-tab-get-tabs)))
	(setq next 0))
    (perspeen-tab-switch-internal next)))

(defun perspeen-tab-prev ()
  "Switch to previous tab."
  (interactive)
  (let ((prev (- (perspeen-tab-get-current-tab-index) 1)))
    (if (< prev 0)
	(setq prev (- (length (perspeen-tab-get-tabs)) 1)))
    (perspeen-tab-switch-internal prev)))


(defun perspeen-tab-header-line-left-tabs (tab-separator selected-face other-face)
  "Config the left of header line with tabs."
  (let ((lhs nil)
	(separator-left (intern (format "powerline-%s-%s"
					(powerline-current-separator)
					(car powerline-default-separator-dir))))
	(face1 'powerline-active1)
	(selected-face 'sd/powerline-active1)
	(inacted-face 'sssdd/powerline-inactive1)
	(face-list nil))
    
    (push inacted-face face-list)
    (dotimes (var (length (perspeen-tab-get-tabs)))
      (push (cond ((eq (car face-list) face1) inacted-face)
		  (t face1))
	    face-list))
    (setf (nth (perspeen-tab-get-current-tab-index) face-list)
	  selected-face)
    (dolist (tab (perspeen-tab-get-tabs))
      (let ((current-face (pop face-list))
	    (current-buf (get tab 'current-buffer)))
	(setq lhs
	      (append lhs
		      (list
		       (powerline-raw (format " %s " (buffer-name current-buf)) current-face 'r)
		       (funcall separator-left current-face (car face-list)))))))
    lhs))


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
	 (lhs  (perspeen-tab-header-line-left-tabs 1 2 4))
	 
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

(defun perspeen-tab-switch-to-buffer (buf-or-name &optional norecord forece-same-window)
  "Advice of switch to buffer."
  (when buf-or-name
    (put (perspeen-tab-get-current-tab) 'current-buffer (get-buffer buf-or-name))))

(defun perspeen-tab-other-window (count &optional all-frames)
  "Advices of other window"
  (when (window-live-p (selected-window))
    (put (perspeen-tab-get-current-tab) 'current-buffer (current-buffer))))

(defun perspeen-tab-init ()
  "Init of perspeen-tab."
  (add-hook 'post-command-hook (lambda () (sd/set-header-line-format t)))
  (advice-add 'switch-to-buffer :after #'perspeen-tab-switch-to-buffer)
  (advice-add 'other-window :after #'perspeen-tab-other-window)
  ;; (perspeen-tab-new-tab-internal)
  )

;;;###autoload

(defun perspeen-tab-start ()
  "Start perspeen-tab "
  (interactive)
  (setq perspeen-tab-configurations (make-perspeen-tab-conf))
  (perspeen-tab-init))

(defun perspeen-tab-stop ()
  "Stop perspeen-tab"
  (interactive)
  (setq perspeen-tab-configurations nil)
  (remove-hook 'post-command-hook (lambda () (sd/set-header-line-format t)))
  (advice-remove 'switch-to-buffer #'perspeen-tab-switch-to-buffer)
  (advice-remove 'other-window #'perspeen-tab-other-window)
  (setq header-line-format nil))

;; (define-minor-mode perspeen-tab-mode
;;   "Toggle Perspeen-tab mode on or off."
;;   :global t
;;   :keymap perspeen-tab-mode-map
;;   (if perspeen-tab-mode
;;       (progn
;; 	(setq perspeen-tab-configurations (make-perspeen-tab-conf))
;; 	(add-hook 'post-command-hook (lambda () (sd/set-header-line-format t)))
;; 	(advice-add 'switch-to-buffer :after #'perspeen-tab-switch-to-buffer)
;; 	(advice-add 'other-window :after #'perspeen-tab-other-window)
;; 	(perspeen-tab-new-tab-internal))
;;     (setq perspeen-tab-configurations nil)
;;     (remove-hook 'post-command-hook (lambda () (sd/set-header-line-format t)))
;;     (advice-remove 'switch-to-buffer #'perspeen-tab-switch-to-buffer)
;;     (advice-remove 'other-window #'perspeen-tab-other-window)
;;     (setq header-line-format nil)))

(provide 'perspeen-tab)
;;; perspeen-tab.el ends here

