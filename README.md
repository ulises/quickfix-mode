quickfix-mode
=============

A quickfix mode for emacs inspired by Eclipse's own quickfix.

![Eclipse's quickfix](http://wiki.eclipse.org/images/thumb/3/3f/Quickfix1.jpg/800px-Quickfix1.jpg)

## How does it work ##

quickfix-mode consists of a series of quickfixes registered for your
favourite programming mode. These will suggest a possible suggestion
depending on the error (or warning) reported for flymake. Oh yes,
quickfix-mode depends on [flymake](http://www.emacswiki.org/emacs/FlyMake) (and [popup](https://github.com/auto-complete/popup-el)).

## Quickfix in action ##

Let's say flymake reports some errors in your code (in this case in erlang)

![flymake errors and warnings](https://dl.dropbox.com/u/1640144/flymake-errors.png)

In particular that a function is not defined yet

![bar is undefined](https://dl.dropbox.com/u/1640144/flymake-undefined-erlang.png)

C-c C-f to the rescue...

![quickfix suggest it could define the function for us](https://dl.dropbox.com/u/1640144/flymake-undefined-erlang-quickfix-define.png)

selecting...and voila!

![and the function is defined so that we can work on it](https://dl.dropbox.com/u/1640144/flymake-undefined-erlang-quickfix-define-selected.png)

Exporting unused functions works too.

![foo is unused :-o](https://dl.dropbox.com/u/1640144/flymake-unused-erlang.png)

![quickfix to the rescue](https://dl.dropbox.com/u/1640144/flymake-unused-erlang-quickfix.png)

![and we're good!](https://dl.dropbox.com/u/1640144/flymake-unused-erlang-quickfix-selected.png)

## Install ##

Clone this repo somewhere and include the directory in your load-path:

``(add-to-list 'load-path "/path/to/quickfix-mode")``

then require the mode:

``(require 'quickfix-mode)``

To add the erlang handlers simply load the file:

``(load-file "/path/to/quickfix-mode/quickfix-erlang.el")``

### Marmalade ###

Soon to come. Stay tuned.
