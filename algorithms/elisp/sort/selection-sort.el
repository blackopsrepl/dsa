;;; selection-sort.el --- Selection sort algorithm O(n²) -*- lexical-binding: t; -*-

;;; Commentary:
;; Selection sort: repeatedly find minimum and build sorted list.
;; Time complexity: O(n²)
;; Space complexity: O(n) - returns new list

;;; Code:

(require 'cl-lib)

(defun selection-sort--find-min-index (vec start)
  "Find index of minimum element in VEC from START to end."
  (let ((min-idx start)
        (min-val (aref vec start)))
    (cl-loop for i from (1+ start) below (length vec)
             when (< (aref vec i) min-val)
             do (setq min-idx i
                      min-val (aref vec i)))
    min-idx))

(defun selection-sort (lst)
  "Return new sorted list using selection sort."
  (if (null lst)
      '()
    (let ((vec (vconcat lst))
          (len (length lst)))
      (cl-loop for i from 0 below (1- len)
               do (let ((min-idx (selection-sort--find-min-index vec i)))
                    (when (/= i min-idx)
                      (cl-rotatef (aref vec i) (aref vec min-idx)))))
      (append vec nil))))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l selection-sort.el
  (cl-assert (equal (selection-sort '(64 34 25 12 22 11 90)) '(11 12 22 25 34 64 90)))
  (cl-assert (equal (selection-sort '(5 4 3 2 1)) '(1 2 3 4 5)))
  (cl-assert (equal (selection-sort '(1 2 3 4 5)) '(1 2 3 4 5)))
  (cl-assert (equal (selection-sort '(1)) '(1)))
  (cl-assert (equal (selection-sort '()) '()))
  (cl-assert (equal (selection-sort '(3 3 3 1 1 2 2)) '(1 1 2 2 3 3 3)))
  (message "All selection-sort tests passed!"))

(provide 'selection-sort)
;;; selection-sort.el ends here
