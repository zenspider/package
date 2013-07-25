;;; package+.el --- Extensions for the package library.

;; Copyright (C) 2013  Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Keywords: extensions, tools
;; Package-Requires: ()
;; URL: TBA
;; Doc URL: TBA
;; Compatibility: GNU Emacs: 23.x?, 24.x

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Provides extensions to `package.el` for Emacs 24 and later.

;; Declares a manifest of packages that should be installed on this
;; system, installing any missing packages and removing any installed
;; packages that are not in the manifest.
;;
;; This makes it easy to keep a list of packages under version control
;; and replicated across all your environments, without having to have
;; all the packages themselves under version control.
;;
;; Example:
;;
;;    (package-manifest 'ag
;;                      'expand-region
;;                      'magit
;;                      'melpa
;;                      'paredit
;;                      'ruby-mode
;;                      'ssh
;;                      'window-number)

;;; Note:

;; package-version-for, package-delete-by-name, package-maybe-install,
;; and package-cleanup are all going to be submitted upstream to
;; emacs. They're in here and only defined if package-cleanup is not
;; already defined. If my contributions get accepted upstream, they'll
;; be deleted here at some point.

;;; Code:

(unless (fboundp 'package-cleanup)
  (require 'cl)

  (defun package-version-for (name)
    "Returns the installed version for a package with a given NAME."
    (package-desc-vers (cdr (assoc name package-alist))))

  (defun package-delete-by-name (name)
    "Deletes a package by NAME"
    (package-delete (symbol-name name)
                    (package-version-join (package-version-for name))))

  (defun package-maybe-install (name)
    "Installs a package by NAME, but only if it isn't already installed."
    (or (package-installed-p name)
        (progn
          (message "Installing %s" name)
          (package-install name))))

  (defun package-cleanup (packages)
    "Delete installed packages not explicitly declared in PACKAGES."
    (let ((removes (set-difference (mapcar 'car package-alist) packages)))
      (mapc 'package-delete-by-name removes))))

(defun package-manifest (&rest manifest)
  "Declares a MANIFEST of packages that should be installed on this
system, installing any missing packages and removing any installed
packages that are not in the manifest.

This makes it easy to keep a list of packages under version
control and replicated across all your environments, without
having to have all the packages themselves under version
control."
  (package-initialize)

  (unless package-archive-contents      ; why? package-install has this.
    (package-refresh-contents))

  (condition-case err
      (mapc 'package-maybe-install packages)
    (error (message "Couldn't install package: %s" err)))
  (package-cleanup packages))

(provide 'package+)

;;; package+.el ends here
