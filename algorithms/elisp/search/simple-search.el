;;; simple-search.el --- Linear search algorithm O(n) -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple linear search that checks each element sequentially.
;; Time complexity: O(n)
;; Space complexity: O(1)

;;; Code:

(defun simple-search (lst item)
  "Return index of ITEM in LST, or nil if not found.
Performs linear search through the list."
  (let ((index 0))
    (catch 'found
      (dolist (element lst)
        (when (equal element item)
          (throw 'found index))
        (setq index (1+ index)))
      nil)))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l simple-search.el
  (let ((test-list '(1 3 5 7 9)))
    (cl-assert (= (simple-search test-list 1) 0))
    (cl-assert (= (simple-search test-list 5) 2))
    (cl-assert (= (simple-search test-list 9) 4))
    (cl-assert (null (simple-search test-list 99)))
    (cl-assert (null (simple-search test-list 0)))
    (cl-assert (null (simple-search '() 1))))
  (let ((string-list '("apple" "banana" "cherry")))
    (cl-assert (= (simple-search string-list "banana") 1))
    (cl-assert (null (simple-search string-list "grape"))))
  (message "All simple-search tests passed!"))

(provide 'simple-search)
;;; simple-search.el ends here
