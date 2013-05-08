;;; quickfix-mode.el --- a mode for quickfixing common programming errors

;; Copyright (C) 2012-2013 Ulises Cervino Beresi

;; Author: Ulises Cervino Beresi <ulises.cervino@gmail.com>
;; Created: 21 Dec 2012
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;; URL: https://github.com/ulises/quickfix-mode
;; Version: 0.0.1
;; Package-Requires: '((flymake 0.4.13)(popup 0.5))

;;; Code

(require 'cl)
(require 'flymake)
(require 'popup)

;;; Helper functions

;; from http://emacswiki.org/emacs/ElispCookbook
(defun quickfix-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; stolen from fly-cursor
(defun quickfix-get-flymake-issue-at-point ()
  "Gets the first flymake error on the line at point."
  (interactive)
  (let ((line-no (line-number-at-pos))
        flyc-e)
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (setq flyc-e (car (second elem)))))
    flyc-e))

(defun quickfix-match-regex (regexp)
  "Helper function to write predicates. Makes it easier to match the
   given regexp against the text in the issue-at-point."
  `(lambda (issue-at-point)
    (and (not (null issue-at-point))
         (string-match-p ,regexp (flymake-ler-text issue-at-point)))))

(defun quickfix-when-text-matches (regexp generator-or-message)
  "Skeleton predicate for matching regexes in the text reported by flymake.
   If the regexp matches the text then if the generator is a function, it
   is called with issue-at-point. Otherwise the string is returned."
  (or (stringp generator-or-message)
      (functionp generator-or-message)
      (error "generator-or-message must be either a function or a string."))
  `(lambda (issue-at-point)
    (let ((matcher (quickfix-match-regex ,regexp)))
      (when (funcall matcher issue-at-point)
        (cond ((stringp ,generator-or-message) ,generator-or-message)
              ((functionp ,generator-or-message) (funcall ,generator-or-message issue-at-point)))))))

;; actual mode stuff

(defvar quickfix-mode-hook nil)

(defvar quickfix-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-f") 'quickfix-at-point)
    map)
  "Keymap from quickfix minor mode")

(defvar quickfix-mode-handlers '((t . nil))
  "Set of handlers for the flymake issue-at-point. This is an Association List
where keys are major modes and the values are list of handlers (see
defhandler for the definition of handlers.) Handlers for all
modes are registered under the t key.")

(defun quickfix-add-handler (handler &optional modes)
  "Register quickfix using HANDLER.
Optional Argument MODES can be a symbol or a list of symbols
matching modes for which this quickfix should be registered to.
When no MODES is provided the handler is registered for all major-modes."
  (and modes (symbolp modes) (setq modes (list modes)))
  (if modes
      (dolist (mode modes)
        (let ((alist (assq mode quickfix-mode-handlers)))
          (if (not alist)
              (add-to-list
               'quickfix-mode-handlers
               (cons mode (list handler)))
            (setcdr alist (cons handler (cdr alist))))))
    (let ((alist (assq t quickfix-mode-handlers)))
      (setcdr alist
              (cons handler (cdr alist))))))

(defmacro quickfix-make-handler (predicate handler &optional docstring)
  "Defines a quickfix handler. A handler is a function which will receive an
issue-at-point and return either a menu item with the description of the
quickfix and handling function or nil. The handling function takes the
issue-at-point as argument. Its return value is ignored. This function takes
the individual components (the predicate and handling code) and builds
such function."
  `(lambda (issue-at-point)
     (let ((message (funcall ,predicate issue-at-point)))
       (when message
         (popup-make-item message :value ,handler
                          :document (or ,docstring "No documentation."))))))

(defun quickfix-at-point ()
  "Pops up a list of available actions to handle the issue at point.
   Performs the selected action."
  (interactive)
  (let* ((issue-at-point (quickfix-get-flymake-issue-at-point))
         (handler (quickfix-popup-and-get-selected-handler
                   (quickfix-get-handlers issue-at-point))))
    (when handler (funcall handler issue-at-point))))

(defun quickfix-get-handlers (issue-at-point)
  "Returns a list of handlers for issue-at-point and their descriptions."
  (quickfix-filter
   'identity
   (mapcar (lambda (handler)
             (funcall handler issue-at-point))
           (append (cdr (assq t quickfix-mode-handlers))
                   (cdr (assq major-mode quickfix-mode-handlers))
                   nil))))

(defun quickfix-popup-and-get-selected-handler (handlers)
  "Pops up a menu with the potential fixes and lets the user choose one. Returns the selected handler fn"
  (when handlers
    (save-excursion
      (end-of-line)
      (popup-menu* handlers))))

(define-minor-mode quickfix-mode
  "Quickfix is a minor mode dependent on flymake-mode"
  :init-value nil
  :lighter " QF")

(defun quickfix-mode-maybe ()
  "Used to turn on the globalized minor mode."
  (and (buffer-file-name)
       (flymake-get-file-name-mode-and-masks
        (buffer-file-name))
       (quickfix-mode 1)))

(define-globalized-minor-mode quickfix-global-mode
  quickfix-mode quickfix-mode-maybe
  :group 'quickfix-mode
  :require 'quickfix-mode)


(provide 'quickfix-mode)
;;; quickfix-mode.el ends here
