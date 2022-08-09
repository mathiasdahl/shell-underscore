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

To just try it out manually, do this:

```
M-x shell RET
M-x load-file RET path/to/shell-underscore.el RET
M-x shell-underscore-mode
```

This will enable shell-underscore-mode in your current shell mode session.

## Limitations

- Only one use of ```_``` per command invocation is supported.
- In case you are working with real files named ```_``` or ```_x``` or similar, this mode might not be for you... (or disable ```shell-underscore-mode``` when you need to)
- Only works with GNU/Linux or Unix like file systems since it is assumed there is a /tmp folder.

