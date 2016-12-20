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

(push '("melpa" . "http://melpa.org/packages") package-archives)

(package-initialize)

(unless (package-installed-p 'powerline)
  (package-install 'powerline))

(mapc #'byte-compile '("perspeen.el" "perspeen-tab.el"))

(require 'perspeen)


;;; perspeen-init.el ends here
