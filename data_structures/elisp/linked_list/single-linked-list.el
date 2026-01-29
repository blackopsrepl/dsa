;;; single-linked-list.el --- Singly linked list implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; A singly linked list implementation using cl-defstruct.

;;; Code:

(require 'cl-lib)

(cl-defstruct (linked-list-node (:constructor linked-list-node-create (data &optional next)))
  "A node in a singly linked list."
  data
  next)

(cl-defstruct (linked-list (:constructor linked-list-create ()))
  "A singly linked list."
  head
  (size 0))

(defun linked-list-add-first (list data)
  "Add DATA at the beginning of LIST."
  (let ((new-node (linked-list-node-create data (linked-list-head list))))
    (setf (linked-list-head list) new-node)
    (cl-incf (linked-list-size list)))
  list)

(defun linked-list-add-last (list data)
  "Add DATA at the end of LIST."
  (let ((new-node (linked-list-node-create data nil)))
    (if (null (linked-list-head list))
        (setf (linked-list-head list) new-node)
      (let ((current (linked-list-head list)))
        (while (linked-list-node-next current)
          (setq current (linked-list-node-next current)))
        (setf (linked-list-node-next current) new-node)))
    (cl-incf (linked-list-size list)))
  list)

(defun linked-list-delete (list data)
  "Delete first occurrence of DATA from LIST. Returns t if found, nil otherwise."
  (when (linked-list-head list)
    (if (equal (linked-list-node-data (linked-list-head list)) data)
        (progn
          (setf (linked-list-head list) (linked-list-node-next (linked-list-head list)))
          (cl-decf (linked-list-size list))
          t)
      (let ((current (linked-list-head list)))
        (catch 'deleted
          (while (linked-list-node-next current)
            (when (equal (linked-list-node-data (linked-list-node-next current)) data)
              (setf (linked-list-node-next current)
                    (linked-list-node-next (linked-list-node-next current)))
              (cl-decf (linked-list-size list))
              (throw 'deleted t))
            (setq current (linked-list-node-next current)))
          nil)))))

(defun linked-list-find (list data)
  "Return node containing DATA in LIST, or nil if not found."
  (let ((current (linked-list-head list)))
    (catch 'found
      (while current
        (when (equal (linked-list-node-data current) data)
          (throw 'found current))
        (setq current (linked-list-node-next current)))
      nil)))

(defun linked-list-to-list (list)
  "Convert linked LIST to a regular Lisp list."
  (let ((result '())
        (current (linked-list-head list)))
    (while current
      (push (linked-list-node-data current) result)
      (setq current (linked-list-node-next current)))
    (nreverse result)))

(defun linked-list-to-string (list)
  "Return string representation of LIST."
  (let ((items (linked-list-to-list list)))
    (if (null items)
        "[]"
      (format "[%s]" (mapconcat (lambda (x) (format "%s" x)) items " -> ")))))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l single-linked-list.el
  (let ((ll (linked-list-create)))
    ;; Test add-first
    (linked-list-add-first ll 3)
    (linked-list-add-first ll 2)
    (linked-list-add-first ll 1)
    (cl-assert (equal (linked-list-to-list ll) '(1 2 3)))
    (cl-assert (= (linked-list-size ll) 3)))

  (let ((ll (linked-list-create)))
    ;; Test add-last
    (linked-list-add-last ll 1)
    (linked-list-add-last ll 2)
    (linked-list-add-last ll 3)
    (cl-assert (equal (linked-list-to-list ll) '(1 2 3)))
    (cl-assert (= (linked-list-size ll) 3)))

  (let ((ll (linked-list-create)))
    ;; Test delete
    (linked-list-add-last ll 1)
    (linked-list-add-last ll 2)
    (linked-list-add-last ll 3)
    (cl-assert (linked-list-delete ll 2))
    (cl-assert (equal (linked-list-to-list ll) '(1 3)))
    (cl-assert (= (linked-list-size ll) 2))
    ;; Delete head
    (cl-assert (linked-list-delete ll 1))
    (cl-assert (equal (linked-list-to-list ll) '(3)))
    ;; Delete non-existent
    (cl-assert (null (linked-list-delete ll 99))))

  (let ((ll (linked-list-create)))
    ;; Test find
    (linked-list-add-last ll "a")
    (linked-list-add-last ll "b")
    (linked-list-add-last ll "c")
    (cl-assert (linked-list-find ll "b"))
    (cl-assert (equal (linked-list-node-data (linked-list-find ll "b")) "b"))
    (cl-assert (null (linked-list-find ll "z"))))

  (let ((ll (linked-list-create)))
    ;; Test to-string
    (cl-assert (equal (linked-list-to-string ll) "[]"))
    (linked-list-add-last ll 1)
    (linked-list-add-last ll 2)
    (cl-assert (equal (linked-list-to-string ll) "[1 -> 2]")))

  (message "All single-linked-list tests passed!"))

(provide 'single-linked-list)
;;; single-linked-list.el ends here
