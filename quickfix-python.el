;;; quickfix-python.el --- Quickfixes for python

;; This file expects user is using fgallina's python.el (available by
;; default in stock Emacs 24.3 or at
;; https://github.com/fgallina/python.el).

(require 'python)
(require 'quickfix-mode)

(defun quickfix-python-mixed-tabs-and-spaces (issue-at-point)
  (interactive)
  (let ((from (line-beginning-position))
        (to (save-excursion
              (back-to-indentation)
              (point))))
    (untabify from to)))

(defvar quickfix-python-mixed-tabs-and-spaces-predicate
  (quickfix-when-text-matches
   "\\(?:E101 indentation contains mixed spaces and tabs\\|W191 indentation contains tabs\\)"
   "Indent with spaces only."))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-mixed-tabs-and-spaces-predicate
                        'quickfix-python-mixed-tabs-and-spaces)
 'python-mode)


(defun quickfix-python-line-too-long (issue-at-point)
  (interactive)
  (goto-char (+ (line-beginning-position) 79)))

(defvar quickfix-python-line-too-long-predicate
  (quickfix-when-text-matches
   "\\(?:E501 line too long\\|Line too long \\)"
   "Jump to 79th char."))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-line-too-long-predicate
                        'quickfix-python-line-too-long)
 'python-mode)


(defun quickfix-python-trailing-whitespace (issue-at-point)
  (interactive)
  (delete-trailing-whitespace (line-beginning-position) (line-end-position)))

(defvar quickfix-python-trailing-whitespace-predicate
  (quickfix-when-text-matches
   "\\(?:W291 trailing whitespace\\|W293 blank line contains whitespace\\)"
   "Delete trailing whitespace."))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-trailing-whitespace-predicate
                        'quickfix-python-trailing-whitespace)
 'python-mode)


(defvar quickfix-python-expected-newlines-regexp
  "E30[12] expected \\([12]\\) blank lines?, found \\([01]\\)")

(defun quickfix-python-expected-newlines-get-num-lines (issue-at-point)
  (let ((issue-text (flymake-ler-text issue-at-point)))
    (or
     (and
      (string-match
       quickfix-python-expected-newlines-regexp issue-text)
      (- (string-to-int (match-string 1 issue-text))
         (string-to-int (match-string 2 issue-text))))
     0)))

(defun quickfix-python-expected-newlines (issue-at-point)
  (newline (quickfix-python-expected-newlines-get-num-lines issue-at-point)))

(defvar quickfix-python-expected-newlines-predicate
  (quickfix-when-text-matches
   quickfix-python-expected-newlines-regexp
   (lambda (issue-at-point)
     (let ((num-lines
            (quickfix-python-expected-newlines-get-num-lines issue-at-point)))
       (format "Add %s newline%s." num-lines (if (> num-lines 1) "s" ""))))))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-expected-newlines-predicate
                        'quickfix-python-expected-newlines)
 'python-mode)


(defvar quickfix-python-too-many-newlines-regexp
  "E303 too many blank lines (\\([0-9]+\\))")

(defun quickfix-python-too-many-newlines-get-num-lines (issue-at-point)
  (let ((issue-text (flymake-ler-text issue-at-point)))
    (or
     (and
      (string-match
       quickfix-python-too-many-newlines-regexp issue-text)
      (string-to-int (match-string 1 issue-text)))
     0)))

(defun quickfix-python-too-many-newlines (issue-at-point)
  (backward-delete-char
   (quickfix-python-too-many-newlines-get-num-lines issue-at-point)))

(defvar quickfix-python-too-many-newlines-predicate
  (quickfix-when-text-matches
   quickfix-python-too-many-newlines-regexp
   (lambda (issue-at-point)
     (let ((num-lines
            (quickfix-python-too-many-newlines-get-num-lines issue-at-point)))
       (format "Remove %s newline%s." num-lines (if (> num-lines 1) "s" ""))))))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-too-many-newlines-predicate
                        'quickfix-python-too-many-newlines)
 'python-mode)


(defvar quickfix-python-backticks-regexp
  "^W604 backticks are deprecated, use 'repr()'")

(defun quickfix-python-backticks (issue-at-point)
  (goto-char (line-beginning-position))
  (while (and
          (re-search-forward "\\(`\\)[^`]+\\(`\\)" (line-end-position) t)
          (python-info-ppss-context 'string)))
  (when (and (not (python-info-ppss-context 'string))
             (match-string 1) (match-string 2))
    (replace-match "repr(" nil nil nil 1)
    (replace-match ")" nil nil nil 2)))

(defvar quickfix-python-backticks-predicate
  (quickfix-when-text-matches
   quickfix-python-backticks-regexp
   "Replace backticks with repr()"))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-backticks-predicate
                        'quickfix-python-backticks)
 'python-mode)


(defvar quickfix-python-not-equal-comparison-regexp
  "\\(?:W603 '<>' is deprecated, use '!='\\|Use of the <> operator\\)")

(defun quickfix-python-not-equal-comparison (issue-at-point)
  (goto-char (line-beginning-position))
  (while (and
          (re-search-forward "\\(<>\\)" (line-end-position) t)
          (python-info-ppss-context 'string)))
  (when (and (not (python-info-ppss-context 'string))
             (match-string 1))
    (replace-match "!=" nil nil nil 1)))

(defvar quickfix-python-not-equal-comparison-predicate
  (quickfix-when-text-matches
   quickfix-python-not-equal-comparison-regexp
   "Replace <> with !="))

(quickfix-add-handler
 (quickfix-make-handler quickfix-python-not-equal-comparison-predicate
                        'quickfix-python-not-equal-comparison)
 'python-mode)


(provide 'quickfix-python)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; quickfix-python.el ends here
