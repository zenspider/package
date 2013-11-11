;;; package+.el --- Extensions for the package library.

;; Copyright (C) 2013  Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;;         Mattias Bengtsson <mattias.jc.bengtsson@gmail.com>
;; Version: 20131111.24
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
;; system. Invoking package-sync will then install any missing packages 
;; and remove any installed packages not in the manifest.
;;
;; This makes it easy to keep a list of packages under version control
;; and replicated across all your environments, without having to have
;; all the packages themselves under version control.
;;
;; Just add the following snippet to your .emacs:
;;   (add-hook 'after-init-hook 'package-sync)
;;

;;; Note:

;; package-version-for, package-delete-by-name, package-maybe-install,
;; package-cleanup, package-deps-for, and package-transitive-closure
;; are all going to be submitted upstream to emacs. They're in here
;; and only defined if package-cleanup is not already defined. If my
;; contributions get accepted upstream, they'll be deleted here at
;; some point.

;;; Code:

(unless (fboundp 'package-cleanup)
  (require 'cl)

  (defun package-version-for (name)
    "Returns the installed version for a package with a given NAME."
    (package-desc-vers (cdr (assoc name package-alist))))

  (defun package-delete-by-name (name)
    "Deletes a package by NAME"
    (message "Removing %s" name)
    (package-delete (symbol-name name)
                    (package-version-join (package-version-for name))))

  (defun package-maybe-install (name)
    "Installs a package by NAME, but only if it isn't already installed."
    (unless (package-installed-p name)
      (message "Installing %s" name)
      (package-install name)))

  (defun package-deps-for (pkg)
    "Returns the dependency list for PKG or nil if none or the PKG doesn't exist."
    (let ((v (cdr (assoc pkg package-alist))))
      (and v (package-desc-reqs v))))

  (defun package-transitive-closure (pkgs)
    (let ((deps '()))
      (dolist (pkg pkgs deps)
        (add-to-list 'deps pkg)
        (dolist (new-pkg (mapcar 'car (package-deps-for pkg)))
          (add-to-list 'deps new-pkg)))))

  (defun package-cleanup (packages)
    "Delete installed packages not explicitly declared in PACKAGES."
    (let ((removes (set-difference (mapcar 'car package-alist)
                                   (package-transitive-closure packages))))
      (mapc 'package-delete-by-name removes))))

(defgroup package-manifest nil
  "A list of packages that should be installed on
this system. The package-manifest-sync function will try to install any package
on this list and uninstall all other packages."
  :group 'applications)

;; TODO: Investigate Variable Definitions
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Definitions.html
;; Specifically :initialize to see if it is possible to initialize this to the list of
;; currently installed packages.
(defcustom package-manifest nil
  "A list of packages that the user expects to have installed.
The package-manifest-sync function will try to install any package on
this list and also uninstall any external package not on this list."
  :type 'list
  :group 'package-manifest)

;;;###autoload
(defadvice package-install (after package-manifest-install-advice activate)
  (package--add-to-manifest (symbol-name (ad-get-arg 0)))
)
;;;###autoload
(defadvice package-delete (after package-manifest-uninstall-advice activate)
  (package--remove-from-manifest (ad-get-arg 0))
)
;;;###autoload
(defun package--add-to-manifest (pkg-name)
  (unless (member pkg-name package-manifest)
    (customize-save-variable 'package-manifest (cons pkg-name package-manifest)))
  (message (concat "Added " pkg-name " to package manifest"))
)
;;;###autoload
(defun package--remove-from-manifest (pkg-name)
  (customize-save-variable 'package-manifest (delq pkg-name package-manifest))
  (message (concat "Removed " pkg-name " from package manifest"))
)

;;;###autoload
(defun package-sync ()
  "Ensures the packages in the custom variable package-manifest
is installed and uninstalls all other packages.

This makes it easy to keep a list of packages under version
control and replicated across all your environments, without
having to have all the packages themselves under version
control."
  (interactive)
  (package-initialize)

  (unless package-archive-contents      ; why? package-install has this.
    (package-refresh-contents))

  (let ((tc-manifest (package-transitive-closure (mapcar 'intern package-manifest))))
    (condition-case err
        (mapc 'package-maybe-install tc-manifest)
      (error (message "Couldn't install package: %s" err)))
    (package-cleanup tc-manifest))

  (unless package-archive-contents      ; why? package-install has this.
    (package-refresh-contents)))

(provide 'package+)

;;; package+.el ends here
