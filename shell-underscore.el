;;; shell-underscore.el --- Use _ as a shorthand in Shell mode for the last shell output

;; Copyright (C) 2022-2022  Mathias Dahl

;; Author: Mathias Dahl <mathias.dahl@gmail.com>
;; Maintainer: Mathias Dahl <mathias.dahl@gmail.com>
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0.0
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ShellUnderscore

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package enables the use of an underscore (`_') character in
;; Emacs Shell mode, as a shorthand for a temporary file containing the
;; last shell output.

;;; Usage:
;;
;; See documentation for `shell-underscore-mode'.

;;; Installation:
;;
;; To enable this mode when a shell is opened, put the following in
;; your Emacs init file:

;; (defun my-shell-hook ()
;;   (require 'shell-underscore)
;;   (shell-underscore-mode t))
;;
;; (add-hook 'shell-mode-hook #'my-shell-hook)

;; In case you want to remove the hook, evaluate this expression:

;; (remove-hook 'shell-mode-hook #'my-shell-hook)

;; Please see README.md from the same repository for more
;; documentation.

;;; Limitations:
;;
;; - Only one use of `_' per command invocation is supported.
;; - In case you are working with real files named `_' or `_x', this mode
;;   might not be for you...

;;; TODO
;;
;; - Think about using underscore + number to access the Nth previous
;;   output (_1 would mean the last, _2 second to last, etc.)

;;; Code:

(require 'comint)

(defvar shell-underscore--last-output-file nil)

(defun shell-underscore--maybe-make-temp-file (&optional name force)
  "Create a temp file and return its name.
NAME is either a letter or a number used to construct a fixed file name.
A non nil value for FORCE forces any named file to be overwritten with
the last result."
  (if name
      (let ((file (expand-file-name (concat "shell-output-" name)
                                    temporary-file-directory)))
        (when (or (not (file-exists-p file))
                  force)
          file))
    (make-temp-file "shell-output-")))

(defun shell-underscore--make-output-file (text &optional force)
  "If command includes a _ or FORCE is not nil , save last output TEXT to file.
If the `_' syntax is used, the saved output is used in the next step of
the flow when the command is transformed to use the temporary file containing
the output."
  (cond (;; Basic case (command _)
         (string-match ".* _\\($\\| \\)" text)
         (shell-underscore--maybe-make-temp-file))
        ;; Persist case (command _x)
        ((string-match ".* _\\([a-z0-9]\\)\\($\\| \\)" text)
         (shell-underscore--maybe-make-temp-file (match-string 1 text)))
        ;; Persist overwrite case (command _x!)
        ((string-match ".* _\\([a-z0-9]\\)!\\($\\| \\)" text)
         (shell-underscore--maybe-make-temp-file (match-string 1 text) t))
        (force
         (shell-underscore--maybe-make-temp-file))))

(defun shell-underscore-save-output (text &optional force)
  "Save last output TEXT to file and store its name.
FORCE writing even if text doesn't use `_' syntax."
  (when-let ((filename (shell-underscore--make-output-file text force)))
    (comint-write-output filename)
    (setq shell-underscore--last-output-file filename)))

(defun shell-underscore--transform-command (command temp-file)
  "Transform COMMAND, replace _ with the TEMP-FILE."
  (if (string-match ".* \\(_[a-z0-9]?\\)!?\\($\\| \\)" command)
      (let* ((file (expand-file-name (file-name-nondirectory temp-file)
                                     temporary-file-directory))
             (transformed-command
              (replace-regexp-in-string ".* \\(_[a-z0-9]?!?\\)"
                                        file command nil nil 1)))
        (message "Transformed command to: %s" transformed-command)
        transformed-command)
    command))

(defun shell-underscore-send-command (proc command)
  "If command ends in _, replace with file containing last output.
PROC is the process to send COMMAND to."
  (let ((command (shell-underscore--transform-command command shell-underscore--last-output-file)))
    (comint-simple-send proc command)))

(defun shell-underscore--file-exists-p (filename)
  "Return FILENAME if it exists."
  (if (and filename
           (file-exists-p filename))
      filename
    (progn (message "File %s doesn't exist." filename)
           nil)))

;; shell-underscore-save-output works only in shell buffers so far
;; TODO: let user choose, which shell buffer to use

(defun shell-underscore-insert-last-output-file-name (force)
  "Insert the real file name of a file containing the last output.
With a prefix FORCE argument - create an output file unconditionally."
  (interactive "P")
  (when force
    (shell-underscore-save-output "" t))
  (when (shell-underscore--file-exists-p shell-underscore--last-output-file)
    (insert shell-underscore--last-output-file)))

(defun shell-underscore-find-last-output-file (force)
  "Open the last output in an Emacs buffer.
With a prefix FORCE argument - create an output file unconditionally."
  (interactive "P")
  (when force
    (shell-underscore-save-output "" t))
  (when (shell-underscore--file-exists-p shell-underscore--last-output-file)
    (find-file shell-underscore--last-output-file)))

(define-minor-mode shell-underscore-mode
  "Toggle shell hacks underscore mode.
Shell underscore mode let you access the last shell output saved
in a file with the shorthand `_'.

That means, you can treat `_' as if it represents a file that
contains the last shell output.  When this feature is used, the
`_' is replaced, behind the scenes, with the name of an actual
file.

There are many uses of this feature.  For example, you might have
executed a long-running command and forgot to redirect the
output, or you realize after-the-fact that you want to use the
output in another command.  It can also be useful when you run a
number of commands after each other, each one transforming the
output of the former, without the need to use a pipe.

Examples:

 $ echo hello
 hello
 $ tr 'h' 'H' < _
 Hello
 $

 $ some complex shell command
 bar
 baz
 foo
 $ grep b _
 bar
 baz
 $

It's also possible to have the underscore in the middle of a command:

 $ cat test
 bar
 baz
 foo
 $ grep b _ | grep z
 baz
 $

Note: In order to work correctly, your prompt can only consist of
one line.  If the prompt consists of several lines, all but the last
one will be part of the output saved in the temporary file.

If you want to access a certain output file later by its real
name (they are not removed automatically), you can find the name
in the following way:

 $ ls _
 /tmp/shell-output-NHuKHW
 $

Each time you use `_' it will be a different file that is
used (because at each command invocation that uses the
underscore, new output is generated and saved).  If you want to
persist a certain output and reuse that for several command
invocations, suffix the `_' with a letter (a-z) or a number.
When you do that, the content in the file will not change if you
refer to it with an underscore and that letter or number again so
you can refer to the same file and therefore file content over
and over again.  If you want to overwrite one of the named output
files, add a `!' after the letter.

Example:

 $ cat foo
 hello
 hi
 hey

 $ grep e _a
 hello
 hey

 $ grep i _a
 hi

 $ cat bar
 bye
 good bye

 $ grep g _a!
 good bye

 $ grep b _a
 bye
 good bye"
  :lighter nil
  (if shell-underscore-mode
      (progn
        (add-hook 'comint-input-filter-functions #'shell-underscore-save-output nil t)
        (setq comint-input-sender 'shell-underscore-send-command))
    (remove-hook 'comint-input-filter-functions #'shell-underscore-save-output t)
    (setq comint-input-sender #'comint-simple-send)))

(provide 'shell-underscore)

;;; shell-underscore.el ends here
