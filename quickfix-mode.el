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

;; actualy mode stuff
(defvar quickfix-mode-hook nil)

(defvar quickfix-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-f") 'quickfix-at-point)
    map)
  "Keymap from quickfix minor mode")

(defvar quickfix-mode-handlers '((t . nil))
  "Set of handlers for the flymake issue-at-point.
This is an Association List where keys are major modes and the
values are list of conses where the car is the predicate and the
cdr is the handler.  Predicates and handler for all modes are
registered under the t key.")

(defun quickfix-add-handler (predicate handler &optional modes)
  "Register quickfix using PREDICATE and HANDLER.
Optional Argument MODES can be a symbol or a list of symbols
matching modes for which this quickfix should be registered to.
When no MODES is provided the PREDICATE check is issued for all
modes."
  (and modes (symbolp modes) (setq modes (list modes)))
  (if modes
      (dolist (mode modes)
        (let ((alist (assq mode quickfix-mode-handlers)))
          (if (not alist)
              (add-to-list
               'quickfix-mode-handlers
               (cons mode (list (cons predicate handler))))
            (setcdr alist
                    (cons (cons predicate handler)
                          (cdr alist))))))
    (let ((alist (assq t quickfix-mode-handlers)))
      (setcdr alist
              (cons (cons predicate handler)
                    (cdr alist))))))

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
  (let ((handlers))
    (mapc (lambda (predicate-handler)
            (let ((description
                   (funcall (car predicate-handler) issue-at-point)))
              (and description
                   (setq handlers
                         (cons (list description
                                     (cdr predicate-handler))
                               handlers)))))
          (append (cdr (assq t quickfix-mode-handlers))
                  (cdr (assq major-mode quickfix-mode-handlers)) nil))
    handlers))

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

(provide 'quickfix-mode)
