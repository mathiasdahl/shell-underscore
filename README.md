# shell-underscore

![Screenshot of terminal input using the command cat and an underscore](/cat_.png?raw=true "cat_")

If you are not running a shell in Emacs, you can propably stop reading
here. Or, perhaps if you did try it once and found it lacking, you
might find this feature will make you want to try it again 🙂

In Emacs Shell mode, enable the use of an underscore (```_```) as a
shorthand for the output of the last command:

```
$ echo hi there!
hi there!

$ sed 's/ there//' _ <---- Magic underscore! 😮
hi!

$ tr h H < _
Hi!
```

That means, you can treat ```_``` as if it represents a real file that
contains the last shell output.  When this feature is used, the
```_``` is replaced, behind the scenes, with the name of an actual
file that contains the last shell output.

There are many uses of this feature.  For example, you might have
executed a long-running command and forgot to redirect the output, or
you realize after-the-fact that you want to use the output in another
command.  It can also be useful when you run a number of commands
after each other, each one transforming the output of the former,
without the need to use a pipe.  The first example below is an example
of that, where we first use ```sed``` to operate on the output of the
previous command, then we use ```tr``` to operate on the output of
that.

Each time you use the ```_``` syntax, a new file is created in the
background, representing the last shell output. This means that
```_``` will most probably represent different output every time
(unless you for example use ```cat``` on the output, or some other
command that does not change it.)

If you want to reuse the same output in several commands, you can use
the alternative syntax ```_x```, where ```x``` is a letter (a-z). This
makes the last shell output to be saved to a file that never change
its content. This can be used when you know you want to run different
commands on some output repeated times. We call this "named" output.

If you use this feature you might want to overwrite some named output
at some point; you can do this by adding ```!``` after the letter,
like so: ```_a!```.

## Examples

Basic flow, using the output of the previous command in the next
command:

```
$ echo hi there!
hi there!

$ sed 's/ there//' _
hi!

$ tr h H < _
Hi!
```

Using "named" output with forced overwrite in the next to last command:

```
$ long and slow complex command
foo1
foo2
foo3
bar1
bar2

$ grep foo _a
foo1
foo2
foo3

$ grep bar _a
bar1
bar2

$ grep 1 _a!
bar1

$ grep 2 _a
bar2

$ cat _a
bar1
bar2
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

If you use [use-package](https://github.com/jwiegley/use-package), the setup can look like

```emacs-lisp
(use-package shell-underscore
  ;; if you want to install it directly from git
  ;; :quelpa
  ;; (shell-underscore :repo "mathiasdahl/shell-underscore" :fetcher github)
  :load-path "path/to/shell-underscore/"
  :hook (shell-mode . shell-underscore-mode))
```

The next time you open a shell, ```shell-underscore-mode``` will be enabled.

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

## Alternatives

Emacs being Emacs, you can of course do all of the above without this
little hack. There is already a command in Shell mode (or really
Comint) to save the last piece of output to a file (```C-c C-s```),
and it's not very hard to move to the text of the last output and do
whatever you want with it. And you can of course use all the dedicated
"Emacsified" tools that removes the need for using a shell or terminal
in the first place (Grep, Diff, Magit/VC). But if you like to use the
shell in Emacs, this feature should be a nice addition.
