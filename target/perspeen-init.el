;;; perspeen-init.el --- perspeen init file for test  -*- lexical-binding: t; -*-

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

(add-to-list 'load-path default-directory)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(package-refresh-contents)
(unless (package-installed-p 'powerline)
  (package-install 'powerline))

(mapc #'byte-compile '("perspeen.el" "perspeen-tab.el"))

(require 'perspeen)

;; set use-tab as true and enable perspeen-mode
(setq perspeen-use-tab t)
(perspeen-mode)

;; disable perspeen-mode
(perspeen-mode)

;; set use-tab as nil  and enable perspeen-mode
(setq perspeen-use-tab nil)
(perspeen-mode)

;;; perspeen-init.el ends here
