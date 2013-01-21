;;; quickfix-erlang.el
;;; Copyright 2012 Ulises Cervino Beresi
;;; URL: https://github.com/ulises/quickfix-mode
;;; Version: 0.0.1
;;; Package-Requires: '((quickfix-mode 0.0.1))

(require 'quickfix-mode)

(defvar quickfix-erlang-generic-fn-re
  "function \\(\\w+\\)/\\([0-9]+\\)"
  "Regexp to extract the function and arity from a flymake error/warning.")

(defvar quickfix-erlang-undefined-fn-re
  "^function \\(\\w+\\)/\\([0-9]+\\) undefined$"
  "Regexp to test whether the flymake error is about an undefined function.")

(defvar quickfix-erlang-unused-fn-re
  "^Warning: function \\(\\w+\\)/\\([0-9]+\\) is unused$"
  "Regexp to test whether the flymake error is about an unused function.")

(defvar quickfix-erlang-fn-skeleton
  "%s(%s) ->"
  "Skeleton for defining an erlang function.")

(defun quickfix-erlang-get-undefined-function-name-and-arity (issue-at-point)
  (when (not (null issue-at-point))
    (let ((issue-text (flymake-ler-text issue-at-point)))
    (string-match quickfix-erlang-generic-fn-re issue-text)
    (list (match-string 1 issue-text) (string-to-int (match-string 2 issue-text))))))

(defun quickfix-erlang-generic-arguments (n)
  (interactive)
  (quickfix-erlang-generic-arguments- n ()))

(defun quickfix-erlang-generic-arguments- (n acc)
  (interactive)
  (print n)
  (if (<= n 0) (mapconcat 'identity acc ",")
    (quickfix-erlang-generic-arguments- (- n 1) (cons (format "_Arg%d" n) acc))))

(defun quickfix-erlang-undefined-fn-fix (issue-at-point)
  (interactive)
  (let* ((name-and-arity (quickfix-erlang-get-undefined-function-name-and-arity issue-at-point))
         (fn-name (car name-and-arity))
         (fn-arity (car (cdr name-and-arity)))
         (gen-args (quickfix-erlang-generic-arguments fn-arity)))

    ;; go to the end of the buffer and start a new para
    (end-of-buffer)
    (insert-string "\n")

    ;; insert the function's skeleton
    (insert-string
     (format quickfix-erlang-fn-skeleton fn-name gen-args))

    ;; place point at beginning of argument list
    (beginning-of-line)
    (search-forward "(")))

(defun quickfix-erlang-unused-fn-fix (issue-at-point)
  (save-excursion
    (interactive)
    (let* ((name-and-arity (quickfix-erlang-get-undefined-function-name-and-arity issue-at-point))
           (fn-name (car name-and-arity))
           (fn-arity (car (cdr name-and-arity))))
      (beginning-of-buffer)
      (search-forward-regexp "^\\-export[^].)]")
      (end-of-line)
      (backward-char 3)
      (insert-string (format ", %s/%d" fn-name fn-arity)))))

(defvar quickfix-erlang-undefined-fn-predicate
  (quickfix-when-text-matches
   quickfix-erlang-undefined-fn-re
   "define function"))

(defvar quickfix-erlang-unused-fn-predicate
  (quickfix-when-text-matches quickfix-erlang-unused-fn-re
                              "export function"))


;; registering handlers

(defvar quickfix-erlang-undefined-fn-handler
  (quickfix-make-handler quickfix-erlang-undefined-fn-predicate
                         'quickfix-erlang-undefined-fn-fix))

(defvar quickfix-erlang-unused-fn-handler
  (quickfix-make-handler quickfix-erlang-unused-fn-predicate
                         'quickfix-erlang-unused-fn-fix))

(quickfix-add-handler
 quickfix-erlang-undefined-fn-handler 'erlang-mode)

(quickfix-add-handler
 quickfix-erlang-unused-fn-handler 'erlang-mode)

;;; quickfix-erlang.el ends here
