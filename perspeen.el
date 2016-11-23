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
 (defvar perspeen-last-ws nil "The last workspace."))

(put 'persp-modestring 'risky-local-variable t)

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
    (setq name (concat "ws-" (number-to-string (hash-table-count perspeen-ws-hash))))
    name))
	  

(defun perspeen-update-mode-string ()
  "Update perspeen-modestring when perspeen-ws-hash is changed"
  (let ((full-string))
    (maphash (lambda (key value)
	       (setq full-string (concat full-string (if (/= 0 (length full-string))
							 "|") key)))
	     perspeen-ws-hash)
    (setq perspeen-modestring (concat "[" full-string "]")))
  ;; update global mode-line
  (unless (memq 'perspeen-modestring global-mode-string)
    (setq global-mode-string (append global-mode-string '(perspeen-modestring)))))

(defun perspeen-create-ws ()
  "Create a new workspace"
  (interactive)
  (perspeen-new-ws-internal (perspeen-get-new-ws-name))
  (perspeen-update-mode-string))

(defun perspeen-switch-ws (name)
  "Switch to workspace with name"
  (setq perspeen-current-ws (gethash name perspeen-ws-hash)))

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
	;; run the hooks
	(run-hooks 'perspeen-mode-hook))
    ;; clear variables
    (setq global-mode-string (delq 'perspeen-modestring global-mode-string))
    (setq perspeen-ws-hash nil)))

(provide 'perspeen)
;;; perspeen.el ends here
