;;; geiser-loko.el --- Geiser and Loko talk to each other.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tim Van den Langenbergh

;; Author: Tim Van den Langenbergh <tmt_vdl@gmx.com>
;; Keywords: languages, geiser, loko, scheme
;; Package-Requires: ((emacs "26.1") (geiser "0.19"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides support for Loko Scheme in Geiser.


;;; Code:

(require 'geiser)

(require 'geiser-custom)
(require 'geiser-eval)
(require 'geiser-impl)
(require 'geiser-log)
(require 'geiser-syntax)

(require 'compile)
(require 'info-look)
(eval-when-compile (require 'cl-lib))


;;; Customisation:

(defgroup geiser-loko nil
  "Customisation for Geiser's Loko Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-loko-binary "loko"
  "Name to use to call the Loko Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-source-directory
    "src"
  "The path to the Loko Scheme sources' src/ directory."
  :type 'directory
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-extra-command-line-parameters nil
  "Additional parameters to supply to the Loko Scheme binary."
  :type '(repeat string)
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-extra-keywords nil
  "Extra keywords highlighted in Loko Scheme buffers."
  :type '(repeat string)
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-manual-lookup-nodes
    '("loko")
  "List of info nodes that, when present, are used for manual lookups."
  :type '(repeat string)
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-manual-lookup-other-window-p t
  "Non-nil means pop up the Info buffer in another window."
  :type 'boolean
  :group 'geiser-loko)


;;; REPL support:

(defvar geiser-loko-scheme-dir
  (expand-file-name geiser-loko-source-directory
                    (file-name-directory load-file-name))
  "Directory where the Loko Scheme Geiser modules are installed.")

(defun geiser-loko--add-library-path (impl)
  "Update the Loko library path if IMPL is \"loko\".

Adds `geiser-loko-scheme-dir' to the LOKO_LIBRARY_PATH environment variable
unless it is already in there."
  (when (eq impl 'loko)
    (let ((loko-library-path (getenv "LOKO_LIBRARY_PATH")))
      (cond ((not loko-library-path)
             (setenv "LOKO_LIBRARY_PATH" geiser-loko-scheme-dir))
            ((not (cl-search geiser-loko-scheme-dir loko-library-path))
             (setenv "LOKO_LIBRARY_PATH"
                     (concat loko-library-path
                             ":"
                             geiser-loko-scheme-dir)))))))

(defun geiser-loko--binary nil
  "Return the runnable Loko Scheme binary name without path."
  (if (listp geiser-loko-binary)
      (car geiser-loko-binary)
    geiser-loko-binary))

(defun geiser-loko--parameters nil
  "Return a list with all parameters needed to start Loko Scheme."
  geiser-loko-extra-command-line-parameters)

(defconst geiser-loko--prompt-regexp "> ")


;;; Evaluation support:

(defun geiser-loko--geiser-procedure (proc &rest args)
  "Transform PROC in string for a scheme procedure using ARGS."
  (cl-case proc
    ((eval compile)
     (let ((form (mapconcat 'identity (cdr args) " "))
           (module (cond ((string-equal "'()" (car args)) "'()")
                         ((car args) (concat "'" (car args)))
                         (t "#f"))))
       (format "(geiser:eval %s '%s)" module form)))
    ((load-file compile-file)
     (format "(geiser:load-file %s)" (car args)))
    ((no-values)
     "(geiser:no-values)")
    (t
     (let ((form (mapconcat 'identity args " ")))
       (format "(geiser:%s %s)" proc form)))))

(defun geiser-loko--symbol-begin (module)
  "Return beginning of current symbol while in MODULE."
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

(defun geiser-loko--import-command (module)
  "Return string representing a sexp importing MODULE."
  (format "(import %s)" module))

(defun geiser-loko--exit-command ()
  "Return string representing a REPL exit sexp."
  "(exit 0)")


;;; REPL startup:

(defconst geiser-loko-minimum-version "0.11.0")

(defun geiser-loko--version (binary)
  "Run BINARY to obtain Loko Scheme version."
  (car (process-lines binary
                      "--program"
                      (expand-file-name "loko-version.sls"
                                        geiser-loko-scheme-dir))))

(defun geiser-loko--startup (_remote)
  "Startup function."
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    (geiser-eval--send/wait
     "(begin (import (geiser-loko))
(write `((result) (output . \"\")))
(newline))")))


;;; Error display:

(defun geiser-loko--display-error (_module key msg)
  "Display an error found during evaluation with the given KEY and message MSG."
  (when (stringp msg)
    (save-excursion (insert msg))
    (geiser-edit--buttonize-files))
  (and (not key)
       (not (zerop (length msg)))
       msg))


;;; Buffer implementation guess:

(defconst geiser-loko--guess-re
  (regexp-opt '("loko" "scheme-script")))

(defun geiser-loko--guess ()
  "Guess whether the current buffer edits Loko Scheme code or REPL."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward geiser-loko--guess-re nil t)))


;;; Keywords and syntax:

(defconst geiser-loko--builtin-keywords
  '("call-with-input-file"
    "call-with-output-file"
    "with-input-from-file"
    "with-input-from-string"
    "with-output-to-file"
    "with-output-to-string"))

(defun geiser-loko--keywords nil
  "Return list of Loko-specific keywords."
  (append
   (geiser-syntax--simple-keywords geiser-loko-extra-keywords)
   (geiser-syntax--simple-keywords geiser-loko--builtin-keywords)))


;;; Manual lookup:

(defun geiser-loko--info-spec (&optional nodes)
  "Return an info docspec list for NODES."
  (let* ((nrx "^[       ]+-+ [^:]+:[    ]*")
         (drx "\\b")
         (res (when (Info-find-file "loko" t)
                `(("(loko)Index" nil ,nrx ,drx)))))
    (dolist (node (or nodes geiser-loko-manual-lookup-nodes) res)
      (when (Info-find-file node t)
        (mapc (lambda (idx)
                (add-to-list 'res
                             (list (format "(%s)%s" node idx) nil nrx drx)))
              '("Module Index" "Class Index" "Variable Index"))))))

(info-lookup-add-help :topic 'symbol :mode 'geiser-loko-mode
                      :ignore-case nil
                      :regexp "[^()`',\"        \n]+"
                      :doc-spec (geiser-loko--info-spec))

(defun geiser-loko--manual-look-up (id _mod)
  "Look up ID in the Loko Scheme info manual."
  (let ((info-lookup-other-window-flag
         geiser-loko-manual-lookup-other-window-p))
    (info-lookup-symbol (symbol-name id) 'geiser-loko-mode))
  (when geiser-loko-manual-lookup-other-window-p
    (switch-to-buffer-other-window "*info*"))
  (search-forward (format "%s" id) nil t))


;;; Implementation definition:

(define-geiser-implementation loko
  (binary geiser-loko--binary)
  (arglist geiser-loko--parameters)
  (version-command geiser-loko--version)
  (minimum-version geiser-loko-minimum-version)
  (repl-startup geiser-loko--startup)
  (prompt-regexp geiser-loko--prompt-regexp)
  (debugger-prompt-regexp nil)
  (marshall-procedure geiser-loko--geiser-procedure)
  (exit-command geiser-loko--exit-command)
  (import-command geiser-loko--import-command)
  (find-symbol-begin geiser-loko--symbol-begin)
  (display-error geiser-loko--display-error)
  (external-help geiser-loko--manual-look-up)
  (check-buffer geiser-loko--guess)
  (keywords geiser-loko--keywords))

(geiser-implementation-extension 'loko "sld")


;;; Autoloads:

;;;###autoload
(geiser-activate-implementation 'loko)

;;;###autoload
(autoload 'run-loko "geiser-loko" "Start a Geiser Loko Scheme REPL." t)

;;;###autoload
(autoload 'switch-to-loko "geiser-loko"
  "Start a Geiser Loko Scheme REPL, or switch to a running one." t)

;;;###autoload
(advice-add 'run-geiser :before #'geiser-loko--add-library-path)


(provide 'geiser-loko)
;;; geiser-loko.el ends here
