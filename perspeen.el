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
(define-key perspeen-mode-map (kbd "s-1") (lambda () (interactive) (perspeen-goto-ws 1)))
(define-key perspeen-mode-map (kbd "s-2") (lambda () (interactive) (perspeen-goto-ws 2)))
(define-key perspeen-mode-map (kbd "s-3") (lambda () (interactive) (perspeen-goto-ws 3)))
(define-key perspeen-mode-map (kbd "s-4") (lambda () (interactive) (perspeen-goto-ws 4)))

(defvar perspeen-ws-switch-hook nil  "A hook that's run after `perspeen-switch'.")
(defvar perspeen-ws-before-switch-hook nil "A hook that run before switch workspace")
(defvar perspeen-ws-after-switch-hook nil "A hook that run after switch workspace")

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
  (root-dir default-directory)
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

(defun perspeen-delete-ws ()
  "Remove current workspace"
  (interactive)
  (let ((prev-ws))
    (setq prev-ws (nth 1 (memq perspeen-current-ws (reverse perspeen-ws-list))))
    (delq perspeen-current-ws perspeen-ws-list)
    (perspeen-switch-ws-internal prev-ws))
  (perspeen-update-mode-string))

(defun perspeen-rename-ws (name)
  "Rename the current workspace"
  (interactive
   (list (read-string "Enter the new name: ")))
  (let ((old-name (perspeen-ws-struct-name perspeen-current-ws))
	(new-name))
    (setq new-name (replace-regexp-in-string ":.*$" (concat ":" name " ") old-name))
    (setf (perspeen-ws-struct-name perspeen-current-ws) new-name))
  (perspeen-update-mode-string))

(defun perspeen-ws-eshell (&optional arg)
  "Create or switch to eshell buffer"
  (interactive)
  (let* ((ebufs)
	 (dir-name (car (last (split-string (perspeen-ws-struct-root-dir perspeen-current-ws)
					    "/" t))))
	 (new-eshell-name)
	 (full-eshell-name)
	 (ii 1))
    (setq ebufs
	  (delq nil (mapcar (lambda (buf)
			      (if (equal (with-current-buffer buf major-mode) 'eshell-mode)
				  buf))
			    (perspeen-ws-struct-buffers perspeen-current-ws))))
    (if (> (length ebufs) 0)
	(switch-to-buffer (car ebufs))
      (with-temp-buffer
	(setq-local default-directory (perspeen-ws-struct-root-dir perspeen-current-ws))
	(eshell 'N)
	(setq new-eshell-name (concat eshell-buffer-name "<" dir-name ">"))
	(setq full-eshell-name new-eshell-name)
	(while (get-buffer full-eshell-name)
	  (setq ii (+ ii 1))
	  (setq full-eshell-name (concat new-eshell-name "-" (number-to-string ii))))
	(rename-buffer full-eshell-name)
	(push (current-buffer) (perspeen-ws-struct-buffers perspeen-current-ws))))))

(defun perspeen-change-root-dir (dir)
  "Change the root direcoty of current workspace"
  (interactive
   (list (read-directory-name "Input Dir: ")))
  (setq dir (directory-file-name dir))
  (setf (perspeen-ws-struct-root-dir perspeen-current-ws) dir)
  ;; change the default directory of scratch buffer
  (mapc (lambda (buf)
	  (print buf)
	  (print (buffer-name buf))
	  (when (and (buffer-name buf) (string-match "^*scratch" (buffer-name buf)))
	    (with-current-buffer buf
	      (setq-local default-directory dir))))
	(perspeen-ws-struct-buffers perspeen-current-ws))
  ;; rename current ws
  (perspeen-rename-ws (car (last
			    (split-string (perspeen-ws-struct-root-dir perspeen-current-ws) "/" t))))
  (perspeen-update-mode-string)
  (message "Root directory chagned to %s" (format dir)))

(defun perspeen-next-ws ()
  "Switch to next workspace"
  (interactive)
  (let ((next-ws))
    (setq next-ws (nth 1 (memq perspeen-current-ws perspeen-ws-list)))
    (perspeen-switch-ws-internal (or next-ws (nth 0 perspeen-ws-list))))
  (perspeen-update-mode-string))

(defun perspeen-previos-ws ()
  "Switch to previous wrokspace"
  (interactive)
  (let ((prev-ws))
    (setq prev-ws (nth 1 (memq perspeen-current-ws (reverse perspeen-ws-list))))
    (perspeen-switch-ws-internal (or prev-ws (nth 0 (reverse perspeen-ws-list)))))
  (perspeen-update-mode-string))

(defun perspeen-goto-ws (index)
  "Switch to the index workspace"
  (interactive "p")
  (if (and (<= index (length perspeen-ws-list))
	   (> index 0))
      (progn
	(perspeen-switch-ws-internal (nth (- index 1) perspeen-ws-list))
	(perspeen-update-mode-string))
    (message "No %d workspace found" index)))

(defun perspeen-switch-ws-internal (ws)
  "Switch to another workspace. save the windows configuration"
  (when ws
    (unless (equal ws perspeen-current-ws)
      (run-hooks 'perspeen-ws-before-switch-hook)
      ;; save the windows configuration
      (setf (perspeen-ws-struct-window-configuration perspeen-current-ws) (current-window-configuration))
      ;; set the current workspace
      (setq perspeen-current-ws ws)
      ;; pop up the previous windows configuration
      (set-window-configuration (perspeen-ws-struct-window-configuration perspeen-current-ws))
      (run-hooks 'perspeen-ws-after-switch-hook))))

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
    (switch-to-buffer (concat "*scratch*<" (perspeen-ws-struct-name perspeen-current-ws) ">"))
    (insert (concat ";; " (buffer-name) "\n\n"))
    (setf (perspeen-ws-struct-buffers perspeen-current-ws) (list (current-buffer)))
    (funcall initial-major-mode)
    (delete-other-windows)))

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


(defun perspeen-switch-to-buffer (buf-or-name &optional norecord force-same-window)
  "Advice of switch to buffer, add current buffer to current workspace"
  (when buf-or-name
    (unless (memq (get-buffer buf-or-name) (perspeen-ws-struct-buffers perspeen-current-ws))
      (push (get-buffer buf-or-name) (perspeen-ws-struct-buffers perspeen-current-ws)))))

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
	(advice-add 'switch-to-buffer :after #'perspeen-switch-to-buffer)
	(add-hook 'ido-make-buffer-list-hook 'perspeen-set-ido-buffers)
	
	;; run the hooks
	(run-hooks 'perspeen-mode-hook))
    ;; clear variables
    (setq global-mode-string (delq 'perspeen-modestring global-mode-string))
    (remove-hook 'ido-make-buffer-list-hook 'perspeen-set-ido-buffers)
    (advice-remove 'switch-to-buffer #'perspeen-switch-to-buffer)
    (setq perspeen-max-ws-prefix 1)
    (setq perspeen-ws-list nil)))

(provide 'perspeen)
;;; perspeen.el ends here
