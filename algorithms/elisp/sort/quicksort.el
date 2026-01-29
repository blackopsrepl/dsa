;;; quicksort.el --- Quicksort algorithm O(n log n) -*- lexical-binding: t; -*-

;;; Commentary:
;; Quicksort using functional style with list partitioning.
;; Time complexity: O(n log n) average, O(n²) worst case
;; Space complexity: O(n) - returns new list

;;; Code:

(require 'cl-lib)

(defun quicksort (lst)
  "Return new sorted list using quicksort."
  (if (or (null lst) (null (cdr lst)))
      lst
    (let* ((pivot (car lst))
           (rest (cdr lst))
           (less (cl-remove-if-not (lambda (x) (< x pivot)) rest))
           (greater (cl-remove-if-not (lambda (x) (>= x pivot)) rest)))
      (append (quicksort less) (list pivot) (quicksort greater)))))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l quicksort.el
  (cl-assert (equal (quicksort '(10 5 2 3)) '(2 3 5 10)))
  (cl-assert (equal (quicksort '(64 34 25 12 22 11 90)) '(11 12 22 25 34 64 90)))
  (cl-assert (equal (quicksort '(5 4 3 2 1)) '(1 2 3 4 5)))
  (cl-assert (equal (quicksort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (cl-assert (equal (quicksort '(1)) '(1)))
  (cl-assert (equal (quicksort '()) '()))
  (cl-assert (equal (quicksort '(3 1 4 1 5 9 2 6)) '(1 1 2 3 4 5 6 9)))
  (message "All quicksort tests passed!"))

(provide 'quicksort)
;;; quicksort.el ends here
