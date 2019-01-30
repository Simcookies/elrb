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

(defcustom elrb-project-root nil
  "The root of the project the current buffer is in."
  :type 'directory
  :safe 'file-directory-p
  :group 'elrb)
(make-variable-buffer-local 'elrb-project-root)

(defcustom elrb-project-root-finder-functions
  '(elrb-project-find-projectile-root
    elrb-project-find-git-root
    elrb-project-find-hg-root
    elrb-project-find-svn-root)
  "List of functions to ask for the current project root."
  :type '(set (const :tag "Projectile project root"
                     elrb-project-find-projectile-root)
              (const :tag "Git repository root (.git)"
                     elrb-project-find-git-root)
              (const :tag "Mercurial project root (.hg)"
                     elrb-project-find-hg-root)
              (const :tag "Subversion project root (.svn)"
                     elrb-project-find-svn-root))
  :group 'elrb)

(defvar elrb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'newline-and-indent)
    (define-key map (kbd "C-c C-d") 'elrb-occur-definitions)
    (define-key map (kbd "C-c C-f") 'elrb-find-file)

    map)
  "Key map for the Emacs Lisp Ruby Environment")

;;;;;;;;;;;;
;;; Projects

(defun elrb-project-root ()
  "Return the root of the current buffer's project."
  (when (not elrb-project-root)
    (setq elrb-project-root
          (run-hook-with-args-until-success
           'elrb-project-root-finder-functions)))
  elrb-project-root)

(defun elrb-set-project-root (new-root)
  "Set the Elrb project root to NEW-ROOT."
  (interactive "DNew project root: ")
  (setq elrb-project-root new-root))

(defun elrb-project-find-git-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".git"))

(defun elrb-project-find-hg-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory ".hg"))

(defun elrb-project-find-svn-root ()
  "Return the current git repository root, if any."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (and (file-directory-p (format "%s/.svn" dir))
                                 (not (file-directory-p (format "%s/../.svn"
                                                                dir)))))))

(defun elrb-project-find-projectile-root ()
  "Return the current project root according to projectile."
  ;; `ignore-errors' both to avoid an unbound function error as well
  ;; as ignore projectile saying there is no project root here.
  (ignore-errors
    (projectile-project-root)))

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

(defun elrb-find-file (&optional dwim)
  "Efficiently find a file in the current project.

With prefix argument, tries to guess what kind of file the user
wants to open."
  (interactive "P")
  (let ((ffip-project-root (or (elrb-project-root)
                               default-directory)))
    (find-file-in-project)))

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
(defun elrb-enable (&optional ignored)
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
