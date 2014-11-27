# package+.el

Provides extensions to `package.el` for Emacs 24 and later.

Declares a manifest of packages that should be installed on this
system, installing any missing packages and possibly removing any
installed packages that are not in the manifest.

This makes it easy to keep a list of packages under version control
and replicated across all your environments, without having to have
all the packages themselves under version control.

If you do not wish to delete installed packages that are not listed in
the manifest, set the variable `package-manifest-cleanup` to `nil`.
Setting this to nil also allows for multiple calls to
`package-manifest`, which may be convenient for organizing emacs
initialization.

Example:

```elisp
   (package-manifest 'ag
                     'expand-region
                     'magit
                     'melpa
                     'paredit
                     'ruby-mode
                     'ssh
                     'window-number)
```
