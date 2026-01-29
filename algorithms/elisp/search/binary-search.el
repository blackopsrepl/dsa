;;; binary-search.el --- Binary search algorithm O(log n) -*- lexical-binding: t; -*-

;;; Commentary:
;; Binary search for sorted lists.
;; Time complexity: O(log n)
;; Space complexity: O(1)

;;; Code:

(defun binary-search (lst item)
  "Return index of ITEM in sorted LST, or nil if not found.
LST must be sorted in ascending order."
  (let* ((vec (vconcat lst))
         (low 0)
         (high (1- (length vec))))
    (catch 'found
      (while (<= low high)
        (let* ((mid (/ (+ low high) 2))
               (guess (aref vec mid)))
          (cond
           ((equal guess item) (throw 'found mid))
           ((< guess item) (setq low (1+ mid)))
           (t (setq high (1- mid))))))
      nil)))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l binary-search.el
  (let ((test-list '(1 3 5 7 9 11 13 15)))
    (cl-assert (= (binary-search test-list 1) 0))
    (cl-assert (= (binary-search test-list 7) 3))
    (cl-assert (= (binary-search test-list 15) 7))
    (cl-assert (null (binary-search test-list 0)))
    (cl-assert (null (binary-search test-list 8)))
    (cl-assert (null (binary-search test-list 100))))
  (cl-assert (null (binary-search '() 1)))
  (cl-assert (= (binary-search '(42) 42) 0))
  (cl-assert (null (binary-search '(42) 0)))
  (message "All binary-search tests passed!"))

(provide 'binary-search)
;;; binary-search.el ends here
