;;; anything-css.el --- Stylesheet selectors anything.el interface

;; Filename: anything-css.el
;; Description: Stylesheet selectors anything.el interface
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;; Maintainer: Kenichirou Oyama <k1lowxb@gmail.com>
;; Copyright (C) 2012, 101000code/101000LAB, all rights reserved.
;; Created: 2012-01-10
;; Version: 0.0.1
;; URL:
;; Keywords: anything, css
;; Compatibility: GNU Emacs 23
;;
;; Features that might be required by this library:
;;
;; `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; Reference
;; Some code referenced from anything-etags.el, anything-find-project-resources.el
;;
;; anything-find-project-resources.el
;; Author: SAKURAI, Masashi <m.sakurai@kiwanami.net>
;;
;; anything-etags.el
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;;         Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;;         Thierry Volpiatto <thierry.volpiatto@gmail.com>

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package use `anything' as a interface to find selectors in Stylesheets.
;;
;; Follow command is `anything' interface of find selectors.
;;
;; `anything-css'
;;
;; The project root directory is
;; automatically detected by `anything-css--project-filep'
;; upwards in the directory tree from the directory of
;; `buffer-file-name'.  If the editing file does not belong to some
;; project, namely could not find the project root directory, this
;; command enumerates files in current directory.
;;
;;; Installation:
;;
;; Put anything-exuberant-ctags.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-css)
;;
;; No need more.

;;; Commands:

;;; Require
(require 'cl)
(require 'anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup anything-css nil
  "Stylesheets anything.el interface."
  :prefix "anything-css--"
  :group 'convenience)

(defcustom anything-css--max-length 30
  "Max length for file path name."
  :type 'number
  :group 'anything-css)

(defcustom anything-css--idl-timer-sec 10
  "Timer sec."
  :type 'number
  :group 'anything-css)

(defcustom anything-css--line-format-func `anything-css--line-format
  "The limit level of line length.
Don't search line longer if outside this value."
  :type 'symbol
  :group 'anything-css)

(defcustom anything-css--project-root-files
  '("build.xml" "prj.el" ".project" "pom.xml"
    "Makefile" "configure" "Rakefile"
    "NAnt.build")
  "This list is employed to find project root directory at
`anything-css--project-filep'.
  General: Makefile, configure,
  Java: build.xml, prj.el, .project, pom.xml
  Ruby: Rakefile
  C#: NAnt.build ."
  :type 'symbol
  :group 'anything-css)

(defvar anything-css--buffer-init nil)
(defvar anything-css--buffer-name-prefix "*anything-css:")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-css (&optional selector-name)
  "Tag jump using `anything'.
If SYMBOL-NAME is non-nil, jump tag position with SYMBOL-NAME."
  (interactive)
  (let* ((initial-pattern               ; string or nil
          (if selector-name
              (concat "\\_<" (regexp-quote selector-name) "\\_>"
                      (if (featurep 'anything-match-plugin) " "))))
         ;;(anything-execute-action-at-once-if-one t)
         )
    (anything :sources '(anything-c-source-css-select)
              :input initial-pattern ;; Initialize input with current symbol
              :prompt "Find Selector: "
              :buffer "*anything-css*"
              )))

(defun anything-css-from-here ()
  "Find selector with current symbol `anything'."
  (interactive)
  (anything-css (thing-at-point 'symbol)))

(defun anything-css--recreate-buffer ()
  "Recreate buffer cache."
  (interactive)
  (anything-css--create-buffer)
  (message "Recreate anything-css candiates complete."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-css--project-filep (file)
  "Determine whether the given file is a project build file.
The current implementation checks
`anything-css--project-root-files'."
  (find file
        anything-css--project-root-files
        :test 'string=))

(defun anything-css--find-project-root-dir (dir)
  "Determine whether the given directory is project root dir.
This function checks parent directories recursively. If this
function found the user's home directory or the system root directory,
returns nil."
  (let* ((expanded-dir (expand-file-name dir))
         (file (some 'anything-css--project-filep (directory-files expanded-dir))))
    (if file expanded-dir
      (if (or
           (string= expanded-dir "/")
           (string= expanded-dir (expand-file-name "~/"))
           ) nil
        (anything-css--find-project-root-dir
         (concat (file-name-as-directory dir) ".."))))))

(defun anything-source-css-header-name (x)
  "CSS Selectors")

(defun anything-css--get-line (s e)
  (let ((substr (buffer-substring s e)))
    (unless (string-match "^ " substr)
      substr)))

(defun anything-css--create-buffer ()
  (let* ((current-user-dir
          (or (and buffer-file-name (file-name-directory (buffer-file-name)))
              (expand-file-name default-directory)
              (expand-file-name "~")))
         (project-root-dir
          (anything-css--find-project-root-dir current-user-dir))
         (buffer-name (unless (not project-root-dir) (concat anything-css--buffer-name-prefix project-root-dir "*"))))
    (if (or (not project-root-dir) (not anything-css--buffer-init))
        nil
      (if
          (and (executable-find "find") (executable-find "grep"))
          (with-current-buffer (get-buffer-create buffer-name)
            (buffer-disable-undo (current-buffer))
            (delete-region (point-min) (point-max))
            (call-process-shell-command
             (concat "find " project-root-dir " -type f -name *.css | xargs grep \"^[[:blank:]]*\\(.*[^ ]\\) *{\" "
                     " -n --with-filename")
             nil (current-buffer)))
        nil))))

(defun anything-css--get-buffer ()
  (let* ((current-user-dir
          (or (and buffer-file-name (file-name-directory (buffer-file-name)))
              (expand-file-name default-directory)
              (expand-file-name "~")))
         (project-root-dir
          (anything-css--find-project-root-dir current-user-dir))
         (buffer-name (unless (not project-root-dir) (concat anything-css--buffer-name-prefix project-root-dir "*"))))
    (setq anything-css--buffer-init t)
    (if (get-buffer buffer-name)
        (get-buffer buffer-name)
      (if (not project-root-dir)
          nil
        (anything-css--create-buffer)
        (get-buffer buffer-name)))))

(defun anything-css--transformer (candidates)
  (let* (list
         format-func
         (path-max-len 0)
         (line-max-len 0)
         (selector-max-len 0)
         (entries
          (mapcar
           (lambda (candidate)
             (let (entry path line selector)
               (if (string-match "^\\([^\t:]+\\):\\([0-9]+\\):[ \t]*\\([^\t{]+\\)[ \t]*{" candidate)
                   (progn
                     (setq path (match-string 1 candidate))
                     (setq line (match-string 2 candidate))
                     (setq selector (match-string 3 candidate))
                     (put-text-property 0 (length selector) 'face 'bold selector))
                 (setq path "")
                 (setq line "")
                 (setq selector ""))
               (setq path-max-len (max path-max-len (length path)))
               (setq line-max-len (max line-max-len (length line)))
               (setq selector-max-len (max selector-max-len (length selector)))
               (setq entry (list path line selector))
               )) candidates)))
    (fset 'format-func (symbol-function anything-css--line-format-func))
    (when (< anything-css--max-length path-max-len)
      (setq path-max-len anything-css--max-length))
    (when (< anything-css--max-length selector-max-len)
      (setq selector-max-len anything-css--max-length))
    (loop for entry in entries
          do
          (push
           (cons
            (format-func entry path-max-len line-max-len selector-max-len)
            (nth 4 entry))
           list)
          finally (return (nreverse list)))))

(defun anything-css--line-format (entry path-max-len line-max-len selector-max-len)
  "Format candidate line."
  (format (format "%%%ds:%%-%ds %%-%ds" path-max-len line-max-len selector-max-len)
          (let ((path (car entry)))
            (if (< path-max-len (length path))
                (substring path (- path-max-len)) path))
          (cadr entry) (caddr entry)))

(defun anything-css--find-selector (candidate)
  "Find selector that match CANDIDATE from `anything-css--create-buffer'.
And switch buffer and jump selector position.."
  (catch 'failed
    (let (file-name line)
      (set-buffer (anything-candidate-buffer))
      ;; Get selector.
      (string-match "^\\([^\t:]+\\):\\([0-9]+\\)" candidate)
      (setq file-name (match-string 1 candidate))
      (setq line (string-to-number (match-string 2 candidate)))
      (goto-char (point-min))
      (search-forward (concat file-name ":" (number-to-string line)) nil t)
      (beginning-of-line)
      (re-search-forward "^\\([^\t:]+\\):\\([0-9]+\\):[ \t]*\\([^\t{]+\\)[ \t]*{" nil t)
      (setq file-name (expand-file-name (match-string 1)))
      (unless (and file-name
                   (file-exists-p file-name))
        (message "Can't find target file: %s" file-name)
        (throw 'failed nil))
      (find-file file-name)
      (goto-line line))))

(defun anything-css--goto-location (candidate)
  (anything-css--find-selector candidate)
  (when (and anything-in-persistent-action
             (fboundp 'anything-match-line-color-current-line))
    (anything-match-line-color-current-line)))

(defvar anything-c-source-css-select
  '((name . "CSS")
    (header-name . anything-source-css-header-name)
    (init . (lambda ()
              (anything-candidate-buffer (anything-css--get-buffer))))
    (candidates-in-buffer)
    (get-line . anything-css--get-line)
    (action ("Goto the location" . anything-css--goto-location))
    (candidate-number-limit . 9999)
    (volatile)
    (candidate-transformer .
                           (lambda (candidates)
                             (anything-css--transformer candidates)))))

(run-with-idle-timer anything-css--idl-timer-sec t 'anything-css--create-buffer)

(provide 'anything-css)
;;; anything-css.el ends here