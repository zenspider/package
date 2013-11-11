# package+.el

Provides extensions to `package.el` for Emacs 24 and later.


Declares a manifest of packages that should be installed on this
system. Invoking package-sync will then install any missing packages
and remove any installed packages not in the manifest.

This makes it easy to keep a list of packages under version control
and replicated across all your environments, without having to have
all the packages themselves under version control.

Just add the following snippet to your .emacs:

```elisp
   (add-hook 'after-init-hook 'package-sync)
```
