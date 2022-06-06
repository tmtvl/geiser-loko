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

(geiser-custom--defcustom geiser-loko-case-sensitive-p t
  "Non-nil means keyword highlighting is case-sensitive."
  :type 'boolean
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-extra-keywords nil
  "Extra keywords highlighted in Loko Scheme buffers."
  :type '(repeat string)
  :group 'geiser-loko)

(geiser-custom--defcustom geiser-loko-init-file "~/.loko-geiser"
  "Initialisation file with user code for the Loko REPL."
  :type 'string
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

(defun geiser-loko--binary nil
  "Return the runnable Loko Scheme binary name without path."
  (if (listp geiser-loko-binary)
      (car geiser-loko-binary)
    geiser-loko-binary))

(defun geiser-loko--parameters nil
  "Return a list with all parameters needed to start Loko Scheme."
  (append geiser-loko-extra-command-line-parameters
          (and (listp geiser-loko-binary)
               (cdr geiser-loko-binary))))

(defconst geiser-loko--prompt-regexp "> ")

(defconst geiser-loko--debugger-prompt-regexp nil)


;;; REPL startup:

(defvar geiser-loko-scheme-dir
  (expand-file-name "src"
                    (file-name-directory load-file-name))
  "Directory where the Loko Scheme Geiser modules are installed.")

(defun geiser-loko--version (binary)
  "Return the version the installed Loko Scheme BINARY."
  (car (process-lines binary
                      "--program"
                      (expand-file-name "loko-version.sls"
                                        geiser-loko-scheme-dir))))

(defconst geiser-loko-minimum-version "0.11.0")

(defun geiser-loko--startup (_remote)
  "Initialise a Loko Scheme REPL."
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    (geiser-eval--send/wait
     (format "(load %S)
(import (geiser-loko))"
             (expand-file-name "geiser-loko.sls" geiser-loko-scheme-dir)))
    (when-let ((init-file (and (stringp geiser-loko-init-file)
                               (expand-file-name geiser-loko-init-file))))
      (if (file-exists-p init-file)
          (geiser-eval--send/wait
           (format "(load %S)" init-file))
        (geiser-log--warn "File %s does not exist, so it's not loaded."
                          init-file)))))


;;; Evaluation support:

(defun geiser-loko--geiser-procedure (proc &rest args)
  "Create a string to send to the REPL to execute the Geiser PROC with ARGS."
  (cl-case proc
    ((eval compile)
     (let ((form (mapconcat #'identity
                            (cdr args)
                            " "))
           (module (if (and (car args)
                            (not (string-equal "'()"
                                               (car args))))
                       (concat "'"
                               (car args))
                     "#f")))
       (format "(geiser:eval %s '%s)" module form)))
    ((load-file compile-file)
     (format "(geiser:load-file %s)"
             (car args)))
    ((no-values)
     "(giser:no-values)")
    (t
     (let ((form (mapconcat 'identity args " ")))
       (format "(geiser:%s %s)" proc form)))))

(defun geiser-loko--find-module (&optional module)
  "Find current module, or normalise MODULE."
  (cond ((null module)
         :f)
        ((listp module)
         module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t
         :f)))

(defun geiser-loko--exit-command nil
  "Return a scheme expression string to exit the REPL."
  "(exit 0)")

(defun geiser-loko--import-command (module)
  "Return a scheme expression string to import MODULE."
  (format "(import %s)" module))

(defun geiser-loko--symbol-begin (module)
  "Return beginning of current symbol while in MODULE."
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))


;;; Error display:

(defun geiser-loko--display-error (_module key msg)
  "Display the KEY and message MSG of an evaluation error."
  (when (stringp msg)
    (save-excursion (insert msg))
    (geiser-edit--buttonize-files))
  (and (not key)
       (not (zerop (length msg)))
       msg))


;;; Manual lookup:

(defun geiser-loko--info-spec (&optional nodes)
  "Return an info docspec list for NODES."
  `(("(loko)Index" nil "^[       ]+-+ [^:]+:[    ]*" "\\b")))

(info-lookup-add-help :topic 'symbol
                      :mode 'geiser-loko-mode
                      :ignore-case nil
                      :regexp "[^()`',\"        \n]+"
                      :doc-spec (geiser-loko--info-spec))

(defun geiser-loko--manual-look-up (id _mod)
  "Look up ID in the Loko info manual."
  (let ((info-lookup-other-window-flag
         geiser-loko-manual-lookup-other-window-p))
    (info-lookup-symbol (symbol-name id)
                        'geiser-loko-mode)
    (when geiser-loko-manual-lookup-other-window-p
      (switch-to-buffer-other-window "*info*"))
    (search-forward (format "%s" id)
                    nil
                    t)))


;;; Recognising Loko buffers:

(defconst geiser-loko--guess-re
  (regexp-opt '("loko" "scheme-script")))

(defun geiser-loko--guess nil
  "Try to determine whether we're in a Loko Scheme buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward geiser-loko--guess-re nil t)))


;;; Keywords and syntax:

(defconst geiser-loko--binding-forms
  '("let-values"))

(defconst geiser-loko--binding-forms*
  '("let*-values"))

(defconst geiser-loko--builtin-keywords
  '("call-with-input-file"
    "call-with-output-file"
    "let-values"
    "let*-values"
    "with-input-from-file"
    "with-output-to-file"))

(defun geiser-loko--keywords nil
  "Return list of Loko-specific keywords."
  (append
   (geiser-syntax--simple-keywords geiser-loko-extra-keywords)
   (geiser-syntax--simple-keywords geiser-loko--builtin-keywords)))


;;; Implementation definition:

(define-geiser-implementation loko
  (binary geiser-loko--binary)
  (arglist geiser-loko--parameters)
  (version-command geiser-loko--version)
  (minimum-version geiser-loko-minimum-version)
  (repl-startup geiser-loko--startup)
  (prompt-regexp geiser-loko--prompt-regexp)
  (debugger-prompt-regexp geiser-loko--debugger-prompt-regexp)
  ;; TODO support debugging through use of GDB.
  ;; (enter-debugger geiser-loko--enter-debugger)
  (marshall-procedure geiser-loko--geiser-procedure)
  (find-module geiser-loko--find-module)
  ;; TODO look for a way to enter a specific module.
  ;; (enter-command geiser-loko--enter-command)
  (exit-command geiser-loko--exit-command)
  (import-command geiser-loko--import-command)
  (find-symbol-begin geiser-loko--symbol-begin)
  (display-error geiser-loko--display-error)
  (binding-forms geiser-loko--binding-forms)
  (binding-forms* geiser-loko--binding-forms*)
  (external-help geiser-loko--manual-look-up)
  (check-buffer geiser-loko--guess)
  (keywords geiser-loko--keywords)
  (case-sensitive geiser-loko-case-sensitive-p))

(geiser-implementation-extension 'loko "ss")
(geiser-implementation-extension 'loko "sls")
(geiser-implementation-extension 'loko "sld")


;;; Autoloads:

;;;###autoload
(geiser-activate-implementation 'loko)

;;;###autoload
(autoload 'run-loko "geiser-loko" "Start a Geiser Loko Scheme REPL." t)

;;;###autoload
(autoload 'switch-to-loko "geiser-loko"
  "Start a Geiser Loko Scheme REPL, or switch to a running one." t)


(provide 'geiser-loko)
;;; geiser-loko.el ends here
