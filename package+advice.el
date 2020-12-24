;;; package+advice.el --- Advice for package library.  -*- lexical-binding: t; -*-

;; Copyright (C) Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; URL: https://github.com/zenspider/package

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

;; Provides advice to enable/disable saving
;; `package-selected-packages` to custom.

(defun package+/customize-save-variable/packages (oldfn variable value &optional comment)
  "Bypass using customize-save-variable when saving package-selected-packages."
  (if (eq variable 'package-selected-packages)
      (setq package-selected-packages value) ; prevent writing to custom.el
    (apply oldfn variable value comment)))

;;;###autoload
(defun package+-disable-package-selected-packages ()
  "Add advice to disable saving package-selected-packages to customize."
  (interactive)
  (advice-add 'customize-save-variable :around
              #'package+/customize-save-variable/packages))

;;;###autoload
(defun package+-enable-package-selected-packages ()
  "Remove advice to enable saving package-selected-packages to customize."
  (interactive)
  (advice-remove 'customize-save-variable
                 #'package+/customize-save-variable/packages))

(provide 'package+advice)
