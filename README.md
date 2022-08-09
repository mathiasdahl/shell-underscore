# shell-underscore

In Emacs Shell mode, enable the use of an underscore (`_') as a shorthand in shell mode for the last shell output

## Examples

```
$ echo hi there
hi there
$ sed 's/there//' _
hi!
```

## Installation

Put the file ```shell-underscore.el``` in your load path and add
something like the following to your Emacs init file:

```emacs-lisp
(defun my-shell-hook ()
  (require 'shell-underscore)
  (shell-underscore-mode t))

(add-hook 'shell-mode-hook #'my-shell-hook)
```

To just try it out manually, after putting the file in the load path, do this:

```
M-x shell RET
M-x load-library RET shell-underscore RET
M-x shell-underscore-mode
```

This will enable shell-underscore-mode in your current shell mode session.

