;;; merge-sort.el --- Merge sort algorithm O(n log n) -*- lexical-binding: t; -*-

;;; Commentary:
;; Merge sort using divide and conquer.
;; Time complexity: O(n log n)
;; Space complexity: O(n)

;;; Code:

(require 'cl-lib)

(defun merge-sort--merge (left right)
  "Merge two sorted lists LEFT and RIGHT into one sorted list."
  (cond
   ((null left) right)
   ((null right) left)
   ((<= (car left) (car right))
    (cons (car left) (merge-sort--merge (cdr left) right)))
   (t
    (cons (car right) (merge-sort--merge left (cdr right))))))

(defun merge-sort--split (lst)
  "Split LST into two roughly equal halves."
  (let* ((len (length lst))
         (mid (/ len 2)))
    (cons (cl-subseq lst 0 mid)
          (cl-subseq lst mid))))

(defun merge-sort (lst)
  "Return new sorted list using merge sort."
  (if (or (null lst) (null (cdr lst)))
      lst
    (let ((halves (merge-sort--split lst)))
      (merge-sort--merge
       (merge-sort (car halves))
       (merge-sort (cdr halves))))))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l merge-sort.el
  (cl-assert (equal (merge-sort '(38 27 43 3 9 82 10)) '(3 9 10 27 38 43 82)))
  (cl-assert (equal (merge-sort '(5 4 3 2 1)) '(1 2 3 4 5)))
  (cl-assert (equal (merge-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (cl-assert (equal (merge-sort '(1)) '(1)))
  (cl-assert (equal (merge-sort '()) '()))
  (cl-assert (equal (merge-sort '(3 1 4 1 5 9 2 6)) '(1 1 2 3 4 5 6 9)))
  ;; Test merge helper
  (cl-assert (equal (merge-sort--merge '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6)))
  (message "All merge-sort tests passed!"))

(provide 'merge-sort)
;;; merge-sort.el ends here
