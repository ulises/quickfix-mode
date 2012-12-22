quickfix-mode
=============

A quickfix mode for emacs.

## Install ##

Clone this repo somewhere and include the directory in your load-path:

``(add-to-list 'load-path "/path/to/quickfix-mode")``

then require the mode:

``(require 'quickfix-mode)``

To add the erlang handlers simply load the file:

``(load-file "/path/to/quickfix-mode/quickfix-erlang.el")``
