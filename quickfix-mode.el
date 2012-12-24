(require 'popup)

(defvar quickfix-modes '()
  "List of major modes for which quickfix should be active.")

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

;; actualy mode stuff
(defvar quickfix-mode-hook nil)

(defvar quickfix-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-f") 'quickfix-at-point)
    map)
  "Keymap from quickfix minor mode")

(defvar quickfix-mode-handlers
  (make-hash-table :test 'equal)
  "Set of handlers for the flymake issue-at-point. A handler is a pair of predicate-fn
   and handler-fn. The predicate should return a description of the proposed change or
   nil. Both the predicate and handler fns take as parameter the issue-at-point which
   looks something like:
   [cl-struct-flymake-ler nil 44 \"e\" \"function bar/2 undefined\"
   \"~/development/erlang/sandbox.erl\"].")

(defun quickfix-add-handler (predicate handler)
  (print predicate)
  (print handler)
  (print quickfix-mode-handlers)
  (puthash predicate handler quickfix-mode-handlers)
  "Register a handler for quickfixing issues.")

(defun quickfix-at-point ()
  "Pops up a list of available actions to handle the issue at point.
   Performs the selected action."
  (interactive)
  (let* ((issue-at-point (quickfix-get-flymake-issue-at-point))
         (handler (quickfix-popup-and-get-selected-handler
                   (quickfix-get-handlers issue-at-point))))
    (when handler (funcall handler issue-at-point))))

(defun quickfix-major-mode-is-registered ()
  "Checks whether the buffer's major mode is active to quickfix."
  (quickfix-filter (lambda (mode) (equal mode major-mode)) quickfix-modes))

(defun quickfix-get-handlers (issue-at-point)
  "Returns a list of handlers for issue-at-point and their descriptions."
  (when (quickfix-major-mode-is-registered)
    (let ((handlers '()))
      (maphash (lambda (predicate handler)
                 (let ((description (funcall predicate issue-at-point)))
                   (when description
                     (setq handlers (cons (list description handler) handlers)))))
               quickfix-mode-handlers)
      handlers)))

(defun quickfix-popup-and-get-selected-handler (handlers)
  "Pops up a menu with the potential fixes and lets the user choose one. Returns the selected handler fn"
  (let* ((menu-entries (mapcar 'car handlers))
         (selected (when menu-entries
                     (save-excursion
                       (end-of-line)
                       (popup-menu* menu-entries)))))
    (car (quickfix-filter 'identity
                 (mapcar (lambda (handler)
                           (when (equal selected (car handler))
                             (car (cdr handler)))) handlers)))))

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
