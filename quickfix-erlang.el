(require 'quickfix-mode)

;;; erlang quickfixes
(defvar quickfix-erlang-undefined-fn-re
  "^function \\(\\w+\\)/\\([0-9]+\\) undefined$"
  "Regexp to test whether the flymake error is about an undefined function.")

(defvar quickfix-erlang-fn-skeleton
  "%s(%s) ->"
  "Skeleton for defining an erlang function.")

(defun quickfix-erlang-get-undefined-function-name-and-arity (issue-at-point)
  (when (not (null issue-at-point))
    (let ((issue-text (flymake-ler-text issue-at-point)))
    (string-match quickfix-erlang-undefined-fn-re issue-text)
    (list (match-string 1 issue-text) (string-to-int (match-string 2 issue-text))))))

(defun quickfix-erlang-generic-arguments (n)
  (interactive)
  (quickfix-erlang-generic-arguments- n ()))

(defun quickfix-erlang-generic-arguments- (n acc)
  (interactive)
  (print n)
  (if (<= n 0) (mapconcat 'identity acc ",")
    (quickfix-erlang-generic-arguments- (- n 1) (cons (format "_Arg%d" n) acc))))

(defun quickfix-erlang-undefined-fn-at-point (issue-at-point)
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

(defvar quickfix-erlang-undefined-fn-predicate
  (quickfix-when-text-matches
   quickfix-erlang-undefined-fn-re
   (lambda (issue-at-point)
     (let* ((name-and-arity (quickfix-erlang-get-undefined-function-name-and-arity issue-at-point))
            (fn-name (car name-and-arity))
            (fn-arity (car (cdr name-and-arity))))
       (format "define %s/%s" fn-name fn-arity)))))


;; registering handlers
(quickfix-add-handler quickfix-erlang-undefined-fn-predicate 'quickfix-erlang-undefined-fn-at-point)










