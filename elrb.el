;;; elrb.el --- Emacs Lisp for Ruby Development Environment (elrb)  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  An Song

;; Author: An Song <songan840136@gmail.com>
;; Version: 0.0.1
;; Keywords: Ruby, IDE, Tools
;; Package-Requires: ((emacs "24.5") (highlight-indentation "0.5.0"))

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

;; Emacs Lips for Ruby Development Environment in Emacs

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

(defcustom elrb-modules '(elrb-module-highlight-block)
  "Elrb modules used."
  :type '(set (const :tag "Display block markers (highlight-indentation)"
                     elrb-module-highlight-block))
  :group 'elrb
  )

(defvar elrb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'newline-and-indent)
    (define-key map (kbd "C-c C-d") 'elrb-occur-definitions)

    map)
  "Key map for the Emacs Lisp Ruby Environment")

;;;;;;;;;;;
;;; Modules

(defvar elrb-modules-initialized-p nil
  "Boolean, set to true if modules were run with `global-init'.")

(defun elrb-modules-run (command &rest args)
  "Run COMMADN with ARGS for all modules in `elrb-modules'."
  (dolist (module elrb-modules)
    (apply module command args)))

(defun elrb-modules-global-init ()
  "Run the global-init method of Elrb modules.

Make sure this only happens once."
  (when (not elrb-modules-initialized-p)
    (elrb-modules-run 'global-init)
    (setq elrb-modules-initialized-p t)))

(defun elrb-modules-global-stop ()
  "Run the global-stop method of Elrb modules.

Make sure this only happens once per global-init call."
  (when elrb-modules-initialized-p
    (elrb-modules-run 'global-stop)
    (setq elrb-modules-initialized-p nil)))

(defun elrb-modules-buffer-init ()
  "Run the buffer-init method of Elrb modules.

Make sure global-init is called first."
  (elrb-modules-global-init)
  (elrb-modules-run 'buffer-init))

(defun elrb-modules-buffer-stop ()
  "Run the buffer-stop method of Elrb modules."
  (elrb-modules-run 'buffer-stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Highlight block

(defun elrb-module-highlight-block (command &rest _args)
  "Module to highlight block in Ruby files."
  (pcase command
    (`global-init
     (require 'highlight-indentation))
    (`buffer-init
     (highlight-indentation-mode 1))
    (`buffer-stop
     (highlight-indentation-mode -1))))

;;;;;;;;;;;;;;;;;;;;
;;; List Definitions

(defun elrb-occur-definitions ()
  "Display an occur buffer to list all definitions in it.

And swith to this buffer."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^ *\\(def\\|class\\|module\\) "))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))

;;;###autoload
(defun elrb-enable (&optional)
  "Enable Elrb in all future Ruby buffers."
  (interactive)
  (when (< emacs-major-version 24)
    (error "Elrb requires Emacs 24 or newer."))
  (elrb-modules-global-init)
  (add-hook 'ruby-mode-hook 'elrb-mode))

(defun elrb-disable ()
  "Disable Elrb in all future Ruby buffers."
  (interactive)
  (remove-hook 'ruby-mode-hook 'elrb-mode)
  (elrb-modules-global-stop))

;;;###autoload
(define-minor-mode elrb-mode
  "Minor mode in Ruby buffers for Emacs Lisp Ruby Development Environment."
  :lighter " Elrb"
  :keymap elrb-mode-map
  (when (not (derived-mode-p 'ruby-mode))
    (error "Elrb only works with `ruby-mode'"))
  (cond
   (elrb-mode
    (elrb-modules-buffer-init))
   ((not elrb-mode)
    (elrb-modules-buffer-stop))))

;;;###autoload
(defun elrb-version ()
  "Display the version of Elrb."
  (interactive)
  (message "Elrb %s" elrb-version))

(provide 'elrb)
;;; elrb.el ends here
