;;; elrb.el --- Emacs Lisp for Ruby Development Environment (elrb)  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  An Song

;; Author: An Song <songan840136@gmail.com>
;; Version: 0.0.1
;; Keywords: Ruby, IDE, Tools

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

;; Emacs Lips Ruby Environment in Emacs

;;; Code:

(defconst elrb-version "0.0.1"
  "The version of the Elrb lisp code.")

(defgroup elrb nil
  "The Emacs Lisp Ruby Environment."
  :prefix "elrb-"
  :group 'languages)

(defcustom elrb-mode-hook nil
  "Hook run when `elrb-mode' is enabled."
  :type 'hook
  :options '(hl-line-mode)
  :group 'elrb)

(defvar elrb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c C-c" 'newline-and-indent)
    (define-key map "C-q" 'newline-and-indent)

    map)
  "Key map for the Emacs Lisp Ruby Environment")

;;;###autoload
(define-minor-mode elrb-mode
  "Minor mode in Ruby buffers for Emacs Lisp Ruby Development Environment."
  :lighter " Elrb"
  (when (not (derived-mode-p 'ruby-mode))
    (error "Elrb only works with `ruby-mode'")))

;;;###autoload
(defun elrb-version ()
  "Display the version of Elrb."
  (interactive)
  (message "Elrb %s" elrb-version))

(provide 'elrb)
;;; elrb.el ends here
