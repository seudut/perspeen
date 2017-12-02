;;; perspeen.el --- An package for multi-workspace  -*- lexical-binding: t; -*-

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

;; This package is intended to combine perspective and elscreen, make each workspace
;; has its own buffers window-configuration and tabs.  The goal is to make Emacs much
;; convenient to work with multiple workspaces at the same time.
;; the same time.

;;; Code:
(require 'perspeen-tab)

(defgroup perspeen nil
  "A minor mode combining perspeen and elscreen "
  :group 'frame)

(defface perspeen-selected-face
  '((t (:weight bold :foreground "Black" :background "Red")))
  "Face used to highlight the current perspeen workspace on the modeline."
  :group 'perspeen)

(defcustom perspeen-modestring-dividers '("[" "]" "|")
  "Plist of strings used to divide workspace on modeline."
  :group 'perspeen)

(defcustom perspeen-workspace-default-name "ws"
  "Default workspace's name."
  :group 'perspeen)

(defcustom perspeen-use-tab nil
  "Enable the perspeen-tab or not."
  :type 'boolean
  :group 'perspeen)

(defcustom perspeen-rename-when-change-root-dir t
  "Rename workspace when changing root directory."
  :type 'boolean
  :group 'perspeen)

(defvar perspeen-ws-before-switch-hook nil
  "Hook run before switch workspace.")

(defvar perspeen-ws-after-switch-hook nil
  "Hook run after switch workspace.")

(defvar perspeen-modestring nil
  "The string displayed on the modeline representing the variable `perspeen-mode'.")
(defvar perspeen-ws-list nil
  "The list storing all workspace in current frame.")
(defvar perspeen-current-ws nil
  "The perspeen structure with current workspace.")
(defvar perspeen-last-ws nil
  "The perspeen structure with last workspace.")

(put 'perspeen-modestring 'risky-local-variable t)

;;* Keymap
(defcustom perspeen-keymap-prefix "\C-z"
  "Prefix key for Perspeen commands."
  :group 'perspeen)

(defvar perspeen-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'perspeen-create-ws)
    (define-key map (kbd "n") #'perspeen-next-ws)
    (define-key map (kbd "p") #'perspeen-previous-ws)
    (define-key map (kbd "'") #'perspeen-goto-last-ws)
    (define-key map (kbd "k") #'perspeen-delete-ws)
    (define-key map (kbd "r") #'perspeen-rename-ws)
    (define-key map (kbd "e") #'perspeen-ws-eshell)
    (define-key map (kbd "d") #'perspeen-change-root-dir)
    (define-key map (kbd "t") #'perspeen-tab-create-tab)
    (define-key map (kbd "1") #'perspeen-ws-jump)
    (define-key map (kbd "2") #'perspeen-ws-jump)
    (define-key map (kbd "3") #'perspeen-ws-jump)
    (define-key map (kbd "4") #'perspeen-ws-jump)
    (define-key map (kbd "5") #'perspeen-ws-jump)
    (define-key map (kbd "6") #'perspeen-ws-jump)
    (define-key map (kbd "7") #'perspeen-ws-jump)
    (define-key map (kbd "8") #'perspeen-ws-jump)
    (define-key map (kbd "9") #'perspeen-ws-jump)
    (define-key map (kbd "s") #'perspeen-goto-ws)
    map)
  "Keymap for `perspeen-mode' after `perspeen-keymap-prefix'.")


(fset 'perspeen-command-map perspeen-command-map)


(defvar perspeen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map perspeen-keymap-prefix 'perspeen-command-map)
    map)
  "Keymap for Perspeen mode.")


(cl-defstruct (perspeen-ws-struct)
  name buffers killed local-variables
  (root-dir default-directory)
  (buffer-history buffer-name-history)
  (window-configuration)
  (point-marker)
  (tabs-configuration (make-perspeen-tab-conf)))


(defun perspeen-update-mode-string ()
  "Update `perspeen-modestring' when `perspeen-ws-list' is changed."
  (let* ((index 1)
	 (ws-name-list
	  (mapcar (lambda (ws)
		    (let* ((name (or (perspeen-ws-struct-name ws) "nil"))
			  (label (format " %d:%s " index name)))
		      (setq index (1+ index))
		      (if (eq ws perspeen-current-ws)
			  (propertize label 'face 'perspeen-selected-face)
			(propertize label 'mouse-face 'mode-line-highlight))))
		  perspeen-ws-list)))
    (setq perspeen-modestring
	  (append (list (nth 0 perspeen-modestring-dividers)
			(mapconcat 'identity ws-name-list (nth 2 perspeen-modestring-dividers))
			(nth 1 perspeen-modestring-dividers))))))

(defun perspeen-create-ws ()
  "Create a new workspace."
  (interactive)
  (perspeen-new-ws-internal)
  (perspeen-update-mode-string))

(defun perspeen-delete-ws ()
  "Remove current workspace."
  (interactive)
  (let ((prev-ws))
    (setq prev-ws (nth 1 (memq perspeen-current-ws (reverse perspeen-ws-list))))
    (delq perspeen-current-ws perspeen-ws-list)
    (perspeen-switch-ws-internal prev-ws))
  (perspeen-update-mode-string))

(defun perspeen-rename-ws (name)
  "Rename the current workspace.
The workspace NAME begin with a number and
a comma as the prefix, the command won't change the prefix."
  (interactive (list (read-string "Enter the new name: " (perspeen-ws-struct-name perspeen-current-ws))))
  (setf (perspeen-ws-struct-name perspeen-current-ws) name)
  (perspeen-update-mode-string))

(defun perspeen-ws-eshell (&optional arg)
  "Create or switch to eshell buffer with current workspace root directory.
Optional argument ARG argument."
  (interactive)
  (let* ((ebufs)
	 (dir-name (car (last (split-string (perspeen-ws-struct-root-dir perspeen-current-ws)
					    "/" t))))
	 (new-eshell-name)
	 (full-eshell-name)
	 (ii 1))
    (setq ebufs
	  (delq nil (mapcar (lambda (buf)
			      (if (and (buffer-live-p buf)
				       (equal (with-current-buffer buf major-mode) 'eshell-mode))
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
  "Change the root direcoty of current workspace.
Argument DIR directory."
  (interactive
   (list (read-directory-name "Inpu Dir: " default-directory)))
  (setq dir (directory-file-name dir))
  (setf (perspeen-ws-struct-root-dir perspeen-current-ws) dir)
  ;; change the default directory of scratch buffer
  (mapc (lambda (buf)
	  (when (and (buffer-name buf) (string-match "^*scratch" (buffer-name buf)))
	    (with-current-buffer buf
	      (setq-local default-directory dir))))
	(perspeen-ws-struct-buffers perspeen-current-ws))
  ;; rename current ws
  (when perspeen-rename-when-change-root-dir
    (perspeen-rename-ws (car (last
			    (split-string (perspeen-ws-struct-root-dir perspeen-current-ws) "/" t))))
	(perspeen-update-mode-string))
  (message "Root directory changed to %s" (format dir)))


(defun perspeen-next-ws ()
  "Switch to next workspace."
  (interactive)
  (let ((next-ws))
    (setq next-ws (nth 1 (memq perspeen-current-ws perspeen-ws-list)))
    (perspeen-switch-ws-internal (or next-ws (nth 0 perspeen-ws-list))))
  (perspeen-update-mode-string))

(defun perspeen-previous-ws ()
  "Switch to previous wrokspace."
  (interactive)
  (let ((prev-ws))
    (setq prev-ws (nth 1 (memq perspeen-current-ws (reverse perspeen-ws-list))))
    (perspeen-switch-ws-internal (or prev-ws (nth 0 (reverse perspeen-ws-list)))))
  (perspeen-update-mode-string))

(defun perspeen-goto-last-ws ()
  "Switch to the last workspace."
  (interactive)
  (when perspeen-last-ws
    (perspeen-switch-ws-internal perspeen-last-ws)
    (perspeen-update-mode-string)))

(defun perspeen-ws-jump ()
  "Switch to workspace that matched with number of key.
e.x., `C-z 1' => switch to ws:1"
  (interactive)
  (let ((next (string-to-number (string last-command-event))))
    (if (and (<= 0 next) (<= next 9))
	(perspeen-goto-ws next))))

(defun perspeen-goto-ws (index)
  "Switch to the INDEX workspace.  Index is a numeric argument."
  (interactive "p")
  (if (and (<= index (length perspeen-ws-list))
	   (> index 0))
      (progn
	(perspeen-switch-ws-internal (nth (- index 1) perspeen-ws-list))
	(perspeen-update-mode-string))
    (message "No %d workspace found" index)))

(defun perspeen-start-tab ()
  "Start perspeen tab."
  (interactive)
  (perspeen-tab-new-tab-internal))

(defun perspeen-switch-ws-internal (ws)
  "Switch to another workspace.
Save the old windows configuration and restore the new configuration.
Argument WS the workspace to swith to."
  (when ws
    (unless (equal ws perspeen-current-ws)
      (run-hooks 'perspeen-ws-before-switch-hook)
      ;; save the windows configuration and point marker
      (if perspeen-use-tab
	  (progn
	    ;; (setf (perspeen-ws-struct-tabs-configuration perspeen-current-ws) (perspeen-tab-get-tabs-configuration))
	    (perspeen-tab-save-configuration))
	(setf (perspeen-ws-struct-window-configuration perspeen-current-ws) (current-window-configuration))
	(setf (perspeen-ws-struct-point-marker perspeen-current-ws) (point-marker)))
      ;; set the current and last  workspace
      (setq perspeen-last-ws perspeen-current-ws)
      (setq perspeen-current-ws ws)
      ;; pop up the previous windows configuration and point marker
      (unless (perspeen-ws-struct-window-configuration perspeen-current-ws)
	(delete-other-windows)
	(setf (perspeen-ws-struct-window-configuration perspeen-current-ws) (current-window-configuration))
	(setf (perspeen-ws-struct-point-marker perspeen-current-ws) (point-marker)))
      (if perspeen-use-tab
	  (progn
	    (perspeen-tab-set-tabs-configuration (perspeen-ws-struct-tabs-configuration perspeen-current-ws))
	    (perspeen-tab-apply-configuration))
	(set-window-configuration (perspeen-ws-struct-window-configuration perspeen-current-ws))
	(goto-char (perspeen-ws-struct-point-marker perspeen-current-ws)))
      (run-hooks 'perspeen-ws-after-switch-hook))))

(defun perspeen-get-new-ws-name ()
  "Generate a name for a new workspace."
  perspeen-workspace-default-name)

(defun perspeen-new-ws-internal (&optional name)
  "Create a new workspace as NAME."
  (let ((new-ws (make-perspeen-ws-struct :name (or name (perspeen-get-new-ws-name)))))
    (add-to-list 'perspeen-ws-list new-ws t)
    ;; this is called from `perspeen-mode'
    (if (= (length perspeen-ws-list) 1)
	(progn
	  ;; (add-to-list 'perspeen-ws-list new-ws t)
	  (setf (perspeen-ws-struct-buffers new-ws)
		(delq nil (mapcar (lambda (buf)
				    (unless (string-match "^ " (buffer-name buf))
				      buf))
				  (buffer-list))))
	  (setq perspeen-current-ws new-ws))
      
      (perspeen-switch-ws-internal new-ws)

      ;; This is not the first workspace
      (switch-to-buffer (format "*scratch*<%s>" (format-time-string "%s")))
      (insert (format ";;; %s created at %s\n\n" (buffer-name) (format-time-string "%Y-%m-%d %H:%M:%S.%N")))
      (setf (perspeen-ws-struct-buffers perspeen-current-ws) (list (current-buffer) (get-buffer "*Messages*")))
      (funcall initial-major-mode)
      ;; initialize the windows configuration of the new workspace
      (unless perspeen-use-tab
	(setf (perspeen-ws-struct-window-configuration perspeen-current-ws) (current-window-configuration))
	(setf (perspeen-ws-struct-point-marker perspeen-current-ws) (point-marker)))))
  (when perspeen-use-tab
    (perspeen-tab-set-tabs-configuration (perspeen-ws-struct-tabs-configuration perspeen-current-ws))
    (perspeen-tab-new-tab-internal)))

(defun perspeen-set-ido-buffers ()
  "Change the variable `ido-temp-list' to restrict the ido buffers candidates."
  ;; modify the ido-temp-list and restrict the ido candidates
  ;; only add the same buffer in ido-temp-list and current workspace buffers
  (setq ido-temp-list
	(remq nil
	      (mapcar (lambda (buf-name)
			(if (member (get-buffer buf-name) (perspeen-ws-struct-buffers perspeen-current-ws))
			    buf-name))
		      ido-temp-list))))


(defun perspeen-switch-to-buffer (buf-or-name &optional norecord force-same-window)
  "Advice of switch to buffer.
To add the new buffer to the buffer list of current
workspace.  Argument BUF-OR-NAME buffer name or buffer.
Optional argument NORECORD norecord.
Optional argument FORCE-SAME-WINDOW force the same window."
  (when buf-or-name
    (unless (memq (get-buffer buf-or-name) (perspeen-ws-struct-buffers perspeen-current-ws))
      (push (get-buffer buf-or-name) (perspeen-ws-struct-buffers perspeen-current-ws)))))

(defun perspeen-display-buffer (buffer-or-name &optional action frame)
  "Advice of display buffer, add it to the buffer list of current workspace.
Argument BUFFER-OR-NAME buffer.
Optional argument ACTION action.
Optional argument FRAME the frame."
  (when buffer-or-name
    (unless (memq (get-buffer buffer-or-name) (perspeen-ws-struct-buffers perspeen-current-ws))
      (push (get-buffer buffer-or-name) (perspeen-ws-struct-buffers perspeen-current-ws)))))

;;;###autoload
(define-minor-mode perspeen-mode
  "Toggle Perspeen mode on or off."
  :global t
  :require 'perspeen
  :keymap perspeen-mode-map
  (if perspeen-mode
      (progn
	;; init local variables
	(setq perspeen-ws-list '())
	(setq global-mode-string (or global-mode-string '("")))
	(when perspeen-use-tab
	  (perspeen-tab-init))
	;; create first workspace and put in into hash
	(perspeen-new-ws-internal)
	;; update perspeen-modestring
	(perspeen-update-mode-string)
	(unless (memq 'perspeen-modestring global-mode-string)
	  (setq global-mode-string (append global-mode-string '(perspeen-modestring))))
	(advice-add 'switch-to-buffer :after #'perspeen-switch-to-buffer)
	(advice-add 'display-buffer :after #'perspeen-display-buffer)

	(add-hook 'ido-make-buffer-list-hook 'perspeen-set-ido-buffers)

	;; run the hooks
	(run-hooks 'perspeen-mode-hook))
    ;; clear variables
    (setq global-mode-string (delq 'perspeen-modestring global-mode-string))
    (remove-hook 'ido-make-buffer-list-hook 'perspeen-set-ido-buffers)
    (advice-remove 'switch-to-buffer #'perspeen-switch-to-buffer)
    (advice-remove 'display-buffer #'perspeen-display-buffer)
    (perspeen-tab-stop)
    (setq perspeen-ws-list nil)))

(provide 'perspeen)
;;; perspeen.el ends here
