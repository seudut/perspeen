;;; perspeen-tab.el --- TAB for perspeen             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Peng Li

;; Author: Peng Li <seudut@gmail.com>
;; URL: https://github.com/seudut/perspeen
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.0") (powerline "2.4"))
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
;; This file is for tab when `perspeen-use-tab' is enabled.

;;

;;; Code:

(require 'powerline)

(defface perspeen-tab--header-line-inactive
  '((t (:inherit mode-line)))
  "Face of header-line inactive"
  :group 'perspeen)

(defface perspeen-tab--header-line-active
  '((t (:inherit mode-line)))
  "Face of header-line active"
  :group 'perspeen)


(defun perspeen-tab--get-upper-left-most-window ()
  "Return the upper-left most window."
  (window-at (frame-width) 0))

(defface perspeen-tab--powerline-inactive1
  '((t (:background "grey11" :inherit mode-line)))
  "Powerline face 1."
  :group 'perspeen)

(defvar perspeen-tab-configurations nil
  "The configurations of all tabs.
It has all the tabs, which tab has a property list of
window-configuration and point-mark")

(cl-defstruct (perspeen-tab-conf)
  tabs
  (current-tab 0)
  (last-tab 0))

(defun perspeen-tab-get-tabs ()
  "Get tabs in ‘perspeen-tab-configurations’."
  (perspeen-tab-conf-tabs perspeen-tab-configurations))

(defun perspeen-tab-get-current-tab-index ()
  "Get current tab in ‘perspeen-tab-configurations’."
  (perspeen-tab-conf-current-tab perspeen-tab-configurations))

(defun perspeen-tab-get-current-tab ()
  "Get current tab."
  (nth (perspeen-tab-get-current-tab-index) (perspeen-tab-get-tabs)))

(defun perspeen-tab-set-tabs-configuration (conf)
  "Set the configuration of tabs.
Argument CONF configuration of the tabs."
  ;; (unless (perspeen-tab-get-current-tab)
  ;;   (put (perspeen-tab-get-current-tab) 'current-buffer (current-buffer)))
  (setq perspeen-tab-configurations conf))

(defun perspeen-tab-get-tabs-configuration ()
  "Get the tabs configurations."
  perspeen-tab-configurations)

(defun perspeen-tab-new-tab-internal (&optional buffer marker)
  "Create a new tab that has BUFFER and MARKER.
And return the created tab.

If the optional BUFFER is not given or nil, using the *scratch* buffer.
If using the *scratch* buffer, MARKER set 0."
  (interactive)
  (let ((tab (make-symbol "perspeen-tab"))
	(tabs (perspeen-tab-conf-tabs perspeen-tab-configurations))
	(prev-tab (perspeen-tab-get-current-tab)))
    (unless buffer
      (unless (dolist (b (buffer-list) buffer)
		(when (string-prefix-p "*scratch*" (buffer-name b))
		  (setq buffer b
			marker 0)))
	(setq buffer (current-buffer)
	      marker (point-marker))))
    (put tab 'window-configuration (current-window-configuration))
    (put tab 'point-marker marker)
    (put tab 'current-buffer buffer)
    (setf (perspeen-tab-conf-tabs perspeen-tab-configurations) (append tabs (list tab)))
    tab))

(defun perspeen-tab-save-configuration ()
  "Save the tab configuration."
  (let ((current-tab (perspeen-tab-get-current-tab)))
    (put current-tab 'window-configuration (current-window-configuration))
    (put current-tab 'point-marker (point-marker))
    (put current-tab 'current-buffer (current-buffer))))

(defun perspeen-tab-apply-configuration ()
  "Apply the tab configuration."
  (let ((current-tab (perspeen-tab-get-current-tab)))
    (when current-tab
	(set-window-configuration (get current-tab 'window-configuration))
      (goto-char (get current-tab 'point-marker))
      (put current-tab 'current-buffer (current-buffer)))))

(defun perspeen-tab-switch-to-tab (tab)
  "Switch to TAB."
  (let ((index 0))
    (dolist (tab0 (perspeen-tab-conf-tabs perspeen-tab-configurations))
      (when (eq tab0 tab)
        (perspeen-tab-switch-internal index))
      (setq index (+ index 1)))))

(defun perspeen-tab-switch-internal (index)
  "Switch tabs.
Argument INDEX the index of the tab two switch."
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
    (switch-to-buffer (get current-tab 'current-buffer))))

(defun perspeen-tab-create-tab (&optional buffer marker switch-to-tab)
  "Create a new tab that has BUFFER and MARKER.
And return the created tab.

If SWITCH-TO-TAB is not nil, switch to the created tab.
If the optional BUFFER is not given or nil, using the *scratch* buffer.
If using the *scratch* buffer, MARKER set 0."
  (interactive)
  (let ((tab (perspeen-tab-new-tab-internal buffer marker))
        (prev-tab (perspeen-tab-get-current-tab)))
    (perspeen-tab-switch-to-tab tab)
    (delete-other-windows)
    (when switch-to-tab
      (perspeen-tab-switch-to-tab prev-tab))))

(defun perspeen-tab-del ()
  "Delete current tab."
  (interactive)
  (let ((current-tab (perspeen-tab-get-current-tab)))
    (when (> (length (perspeen-tab-get-tabs)) 1)
      (perspeen-tab-prev)
      (delq current-tab (perspeen-tab-conf-tabs perspeen-tab-configurations)))))

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
  "Config the left of header line with tabs.
Argument TAB-SEPARATOR the tab separator.
Argument SELECTED-FACE the face of selected tab.
Argument OTHER-FACE the face of un-selected tabs."
  (let ((lhs nil)
	(separator-left (intern (format "powerline-%s-%s"
					(powerline-current-separator)
					(car powerline-default-separator-dir))))
	(face1 'perspeen-tab--header-line-inactive)
	(selected-face 'perspeen-tab--header-line-active)
	(inacted-face 'perspeen-tab--powerline-inactive1)
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


(defun perspeen-tab--construct-header-line ()
  "Generate header line."
  (let* ((active (powerline-selected-window-active))
	 (first-window (frame-first-window))
	 (top-right-window (perspeen-tab--get-upper-left-most-window))
	 (current-window (selected-window))
	 (separator-right (intern (format "powerline-%s-%s"
					  (powerline-current-separator)
					  (cdr powerline-default-separator-dir))))
	 (separator-left (intern (format "powerline-%s-%s"
					 (powerline-current-separator)
					 (car powerline-default-separator-dir))))
	 (lhs  (perspeen-tab-header-line-left-tabs 1 2 4))

	 (rhs (list
	       (funcall separator-right 'perspeen-tab--powerline-inactive1 'powerline-active1)
	       (powerline-raw (format-time-string " %Y-%m-%d %I:%M %p %a ") 'powerline-active1 'r))))

    (cond ((eq current-window first-window)
	   (if (eq first-window top-right-window)
	       (concat
		(powerline-render lhs)
		(powerline-fill 'perspeen-tab--powerline-inactive1 (powerline-width rhs))
		(powerline-render rhs))
       (let ((lhs-str (powerline-render lhs)))
         (concat
          (substring lhs-str 0 (min (length lhs-str) (window-width first-window)))
          (powerline-fill 'perspeen-tab--powerline-inactive1 0)))))
	  ((eq current-window top-right-window)
     (let ((lhs-str (powerline-render lhs)))
       (concat
        (substring lhs-str (min (length lhs-str) (window-width first-window)))
        (powerline-fill 'perspeen-tab--powerline-inactive1 (powerline-width rhs))
        (powerline-render rhs))))
	  (t
	   nil))))

(defun perspeen-tab--set-header-line-format (&optional force)
  "Set the header line format.
Optional argument FORCE force or not to set the header line."
  (if force
      (setq header-line-format
	    '(:eval
	      (perspeen-tab--construct-header-line)))
    (setq-default header-line-format
		  '(:eval
		    (perspeen-tab--construct-header-line)))))

(defun perspeen-tab--update-current-buffer (&optional buf-or-name)
  "Update the buffer of current perspeen-tab.
Argument BUF-OR-NAME buffer or name."
  (interactive)
  (unless buf-or-name
    (setq buf-or-name ""))
  (let ((buf (get-buffer buf-or-name)))
    (unless buf
      (setq buf (current-buffer)))
    (when buf
      (put (perspeen-tab-get-current-tab) 'current-buffer buf))))

(defun perspeen-tab-switch-to-buffer (buf-or-name &optional norecord forece-same-window)
  "Advice of switch to buffer.
Argument BUF-OR-NAME buffer or name.
Optional argument NORECORD no record.
Optional argument FORECE-SAME-WINDOW force the same window."
  (perspeen-tab--update-current-buffer buf-or-name))

(defun perspeen-tab-advice-switch-to-prev-buffer (&optional window bury-or-kill)
  "Advice of `switch-to-prev-buffer'.
WINDOW and BURY-OR-KILL are `switch-to-prev-buffer' options."
  (perspeen-tab--update-current-buffer))

(defun perspeen-tab-other-window (count &optional all-frames)
  "Advices of other window.
Argument COUNT count argument of other window.
Optional argument ALL-FRAMES same as other window."
  (when (window-live-p (selected-window))
    (put (perspeen-tab-get-current-tab) 'current-buffer (current-buffer))))

(defun perspeen-tab-init ()
  "Init of perspeen-tab."
  (add-hook 'post-command-hook (lambda () (perspeen-tab--set-header-line-format t)))
  (advice-add 'switch-to-buffer :after #'perspeen-tab-switch-to-buffer)
  (advice-add 'switch-to-prev-buffer :after #'perspeen-tab-advice-switch-to-prev-buffer)
  (advice-add 'other-window :after #'perspeen-tab-other-window)
  ;; (perspeen-tab-new-tab-internal)
  )

;;;###autoload

(defun perspeen-tab-start ()
  "Start perspeen-tab."
  (interactive)
  (setq perspeen-tab-configurations (make-perspeen-tab-conf))
  (perspeen-tab-init))

(defun perspeen-tab-stop ()
  "Stop perspeen-tab."
  (interactive)
  (setq perspeen-tab-configurations nil)
  (remove-hook 'post-command-hook (lambda () (perspeen-tab--set-header-line-format t)))
  (advice-remove 'switch-to-buffer #'perspeen-tab-switch-to-buffer)
  (advice-remove 'switch-to-prev-buffer #'perspeen-tab-advice-switch-to-prev-buffer)
  (advice-remove 'other-window #'perspeen-tab-other-window)
  (setq header-line-format nil))


(provide 'perspeen-tab)
;;; perspeen-tab.el ends here
