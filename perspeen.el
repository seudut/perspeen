;;; perspeen.el --- An Emacs package for multi-workspace  -*- lexical-binding: t; -*-

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

;; This package intend to combine perspective and elscreen, make each perspective
;; has its elscreen tab. To make it's much convenient to work multi-workspace at
;; the same time.

;;; Code:

(defvar perspeen-mode-map (make-sparse-keymap)  "Keymap for perspeen-mode.")

(define-key perspeen-mode-map (kbd "s-p") 'perspeen-mode)
(define-key perspeen-mode-map (kbd "s-c") 'perspeen-create-ws)
(define-key perspeen-mode-map (kbd "s-n") 'perspeen-next-ws)
(define-key perspeen-mode-map (kbd "s-p") 'perspeen-previos-ws)

(defvar perspeen-ws-switch-hook nil  "A hook that's run after `perspeen-switch'.")

(defun sd/make-variables-frame-local (&rest list)
  "Make all elements in list as frame local variable"
  (mapc (lambda (v)
	    (make-variable-frame-local v))
	  list))

(sd/make-variables-frame-local
 (defvar perspeen-modestring nil "The string displayed in the modeline representing the perspeen-mode.")
 (defvar perspeen-ws-list nil "The list storing all workspace in current frame ")
 (defvar perspeen-current-ws nil "The current workspace")
 (defvar perspeen-last-ws nil "The last workspace.")
 (defvar perspeen-max-ws-prefix 1 "The maximal ws prefix"))

(put 'perspeen-modestring 'risky-local-variable t)

(defface perspeen-selected-face
  '((t (:weight bold :foreground "Black" :background "Red")))
  "The face used to highlight the current perspeen workspace on the modeline")

(defcustom perspeen-modestring-dividers '("[" "]" "|")
  "Plist of strings used to c")

(cl-defstruct (perspeen-ws-struct)
  name buffers killed local-variables
  (buffer-history buffer-name-history)
  (window-configuration (current-window-configuration))
  (pointer-marker (point-marker)))
  
	  

(defun perspeen-update-mode-string ()
  "Update perspeen-modestring when perspeen-ws-list is changed"
  (let ((full-string))
    (mapc (lambda (ws)
	    (let* ((name (perspeen-ws-struct-name ws))
		   (string-name (format "%s" name))
		   (prop-name))
	      (if (equal name (perspeen-ws-struct-name perspeen-current-ws))
		  (setq prop-name (propertize string-name 'face 'perspeen-selected-face))
		(setq prop-name (propertize string-name 'mouse-face 'mode-line-highlight)))
	      (setq full-string (append full-string
					(list (nth 2 perspeen-modestring-dividers) prop-name)))))
	  perspeen-ws-list)
    (setq full-string (cdr full-string))
    (setq perspeen-modestring (append (list (nth 0 perspeen-modestring-dividers))
				      full-string
				      (list (nth 1 perspeen-modestring-dividers))))))

(defun perspeen-create-ws ()
  "Create a new workspace"
  (interactive)
  (perspeen-new-ws-internal)
  (perspeen-update-mode-string))

(defun perspeen-next-ws ()
  "Switch to next workspace"
  (interactive)
  (let ((next-ws))
    (setq next-ws (nth 1 (memq perspeen-current-ws perspeen-ws-list)))
    (setq perspeen-current-ws (or next-ws (nth 0 perspeen-ws-list))))
  (perspeen-update-mode-string))

(defun perspeen-previos-ws ()
  "Switch to previous wrokspace"
  (interactive)
  (let ((prev-ws))
    (setq prev-ws (nth 1 (memq perspeen-current-ws (reverse perspeen-ws-list))))
    (setq perspeen-current-ws (or prev-ws (nth 0 (reverse perspeen-ws-list)))))
  (perspeen-update-mode-string))


(defun perspeen-get-new-ws-name ()
  "Generate a name for a new workspace "
  (let ((name))
    (setq name (concat " " (number-to-string perspeen-max-ws-prefix)":ws "))
    (setq perspeen-max-ws-prefix (+ perspeen-max-ws-prefix 1))
    name))

(defun perspeen-new-ws-internal ()
  "Create a new workspace with the name"
  (let ((new-ws (make-perspeen-ws-struct :name (perspeen-get-new-ws-name))))
    (add-to-list 'perspeen-ws-list new-ws t)
    (setq perspeen-current-ws new-ws))
  ;; if it the first workspace, use the current buffer list
  ;; else add new scratch buffer and clear the buffers
  (if (= 1 (length perspeen-ws-list))
      (progn
	(setf (perspeen-ws-struct-buffers perspeen-current-ws) (buffer-list)))
    (switch-to-buffer (concat "*scratch* (" (perspeen-ws-struct-name perspeen-current-ws) ")"))
    (setf (perspeen-ws-struct-buffers perspeen-current-ws) (list (current-buffer)))
    (funcall initial-major-mode)
    ;; (delete-other-windows)
    ))

(defun perspeen-set-ido-buffers ()
  "restrict the ido buffers"
  ;; modify the ido-temp-list and restrict the ido candidates
  ;; only add the same buffer in ido-temp-list and current workspace buffers
  (setq ido-temp-list
	(remq nil
	      (mapcar (lambda (buf-name)
			(if (member (get-buffer buf-name) (perspeen-ws-struct-buffers perspeen-current-ws))
			    buf-name))
		      ido-temp-list))))

;;;###autoload
(define-minor-mode perspeen-mode
  "perspeen mode"
  :global t
  :keymap perspeen-mode-map
  (if perspeen-mode
      (progn
	;; init local variables
	(setq perspeen-ws-list '())
	(setq global-mode-string (or global-mode-string '("")))
	;; create first workspace and put in into hash
	(perspeen-new-ws-internal)
	;; update perspeen-modestring
	(perspeen-update-mode-string)
	(unless (memq 'perspeen-modestring global-mode-string)
	  (setq global-mode-string (append global-mode-string '(perspeen-modestring))))
	(add-hook 'ido-make-buffer-list-hook 'perspeen-set-ido-buffers)
	
	;; run the hooks
	(run-hooks 'perspeen-mode-hook))
    ;; clear variables
    (setq global-mode-string (delq 'perspeen-modestring global-mode-string))
    (remove-hook 'ido-make-buffer-list-hook 'perspeen-set-ido-buffers)
    (setq perspeen-max-ws-prefix 1)
    (setq perspeen-ws-list nil)))

(provide 'perspeen)
;;; perspeen.el ends here
