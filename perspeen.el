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
  (mapcar (lambda (v)
	    (make-variable-frame-local v))
	  list))

(sd/make-variables-frame-local
 (defvar perspeen-modestring nil "The string displayed in the modeline representing the perspeen-mode.")
 (defvar perspeen-ws-hash nil "The hash storing all workspace in current frame ")
 (defvar perspeen-current-ws nil "The current workspace")
 (defvar perspeen-last-ws nil "The last workspace.")
 (defvar perspeen-max-ws-prefix 1 "The maximal ws prefix"))

(put 'perspeen-modestring 'risky-local-variable t)

(defface perspeen-selected-face
  '((t (:weight bold :foreground "Black" :background "Red")))
  "The face used to highlight the current perspeen workspace on the modeline")

(defcustom perspeen-modestring-dividers '("[" "]" "|")
  "Plist of strings used to c")

(cl-defstruct (perspeen-ws-struct
	       )
  name 
  ;; (buffer-history buffer-name-history)
  ;; (window-configuration (current-window-configuration))
  ;; (pointer-marker (point-marker)))
  )

(defun perspeen-get-new-ws-name ()
  "Generate a name for a new workspace "
  (let ((name))
    (setq name (concat " " (number-to-string perspeen-max-ws-prefix)":ws "))
    (setq perspeen-max-ws-prefix (+ perspeen-max-ws-prefix 1))
    name))
	  

(defun perspeen-update-mode-string ()
  "Update perspeen-modestring when perspeen-ws-hash is changed"
  (let ((full-string)
	(all-names))
    (maphash (lambda (key value)
	       (setq all-names (append all-names (list key))))
	     perspeen-ws-hash)
    (mapcar (lambda (name)
	      (let ((string-name (format "%s" name))
		    (prop-name))
		(if (equal name (perspeen-ws-struct-name perspeen-current-ws))
		    (setq prop-name (propertize string-name 'face 'perspeen-selected-face))
		  (setq prop-name (propertize string-name 'mouse-face 'mode-line-highlight)))
		(setq full-string (append full-string
					  (list (nth 2 perspeen-modestring-dividers) prop-name)))))
	    all-names)
    (setq full-string (cdr full-string))
    (setq perspeen-modestring (append (list (nth 0 perspeen-modestring-dividers))
				      full-string
				      (list (nth 1 perspeen-modestring-dividers))))))

(defun perspeen-create-ws ()
  "Create a new workspace"
  (interactive)
  (perspeen-new-ws-internal (perspeen-get-new-ws-name))
  (perspeen-update-mode-string))

(defun perspeen-get-ws-prefix (ws)
  "Get the prefix of workspace name"
  (let ((name)
	(string))
    (setq name (perspeen-ws-struct-name ws))
    (setq string (format "%s" name))
    (string-to-int (car (split-string string ":")))))

(defun perspeen-get-ws-from-index (index)
  "Get the workspace from the index prefix"
  (let ((ws))
    (maphash (lambda (key value)
	       (if (= index (perspeen-get-ws-prefix value))
		   (setq ws value)))
	     perspeen-ws-hash)
    ws))

(defun perspeen-get-index-list ()
  "Get the list with the work space index"
  (let ((index-list '()))
    (maphash (lambda (key value)
	       (setq index-list (append index-list (list (perspeen-get-ws-prefix value)))))
	     perspeen-ws-hash)
    index-list))

(defun perspeen-get-neighbour-ws (ws next-or-not)
  "Get the next or previous ws index "
  (let ((curr-index)
	(found nil)
	(target-index 0)
	(last 0)
	(index-list (perspeen-get-index-list)))
    (setq curr-index (perspeen-get-ws-prefix ws))
    (catch 'loop-set
      (mapcar (lambda (index)
    		(if (and found next-or-not)
    		    (progn
    		      (setq target-index index)
    		      (throw 'loop-set t))
    		  (if (= index curr-index)
    		      (progn
    			(unless next-or-not
    			  (setq target-index last)
    			  (throw 'loop-set t))
    			(setq found t))))
    		(setq last index))
    	      index-list))
    (if (= target-index 0)
	(if next-or-not
	    (setq target-index (car index-list))
	  (setq target-index (car (reverse index-list)))))
    (perspeen-get-ws-from-index target-index)))

(defun perspeen-next-ws ()
  "Switch to next workspace"
  (interactive)
  (setq perspeen-current-ws (perspeen-get-neighbour-ws perspeen-current-ws t))
  (perspeen-update-mode-string))

(defun perspeen-previos-ws ()
  "Switch to previous wrokspace"
  (interactive)
  (setq perspeen-current-ws (perspeen-get-neighbour-ws perspeen-current-ws nil))
  (perspeen-update-mode-string))

(defun perspeen-new-ws-internal (name)
  "Create a new workspace with the name"
  (let ((new-ws (make-perspeen-ws-struct :name name)))
    (puthash (perspeen-ws-struct-name new-ws) new-ws perspeen-ws-hash)
    (setq perspeen-current-ws new-ws)))

;;;###autoload
(define-minor-mode perspeen-mode
  "perspeen mode"
  :global t
  :keymap perspeen-mode-map
  (if perspeen-mode
      (progn
	;; init local variables
  	(setq perspeen-ws-hash (make-hash-table :test 'equal :size 10))
	(setq global-mode-string (or global-mode-string '("")))
	;; create first workspace and put in into hash
	(perspeen-new-ws-internal (perspeen-get-new-ws-name))
	;; update perspeen-modestring
	(perspeen-update-mode-string)
	(unless (memq 'perspeen-modestring global-mode-string)
	  (setq global-mode-string (append global-mode-string '(perspeen-modestring))))
	;; run the hooks
	(run-hooks 'perspeen-mode-hook))
    ;; clear variables
    (setq global-mode-string (delq 'perspeen-modestring global-mode-string))
    (setq perspeen-max-ws-prefix 1)
    (setq perspeen-ws-hash nil)))

(provide 'perspeen)
;;; perspeen.el ends here
