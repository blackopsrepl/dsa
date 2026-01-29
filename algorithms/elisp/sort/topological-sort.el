;;; topological-sort.el --- Topological sort using Kahn's algorithm O(V+E) -*- lexical-binding: t; -*-

;;; Commentary:
;; Topological sort for directed acyclic graphs using Kahn's algorithm.
;; Time complexity: O(V + E)
;; Space complexity: O(V)

;;; Code:

(require 'cl-lib)

(defun topological-sort (graph nodes)
  "Return topologically sorted NODES from GRAPH.
GRAPH is a hash table mapping node -> list of neighbors.
NODES is a list of all nodes in the graph.
Signals error if cycle detected."
  (let ((in-degree (make-hash-table :test 'equal))
        (queue '())
        (result '())
        (count 0))
    ;; Initialize in-degrees to 0
    (dolist (node nodes)
      (puthash node 0 in-degree))
    ;; Calculate in-degrees
    (maphash (lambda (_from to-list)
               (dolist (to to-list)
                 (puthash to (1+ (gethash to in-degree 0)) in-degree)))
             graph)
    ;; Find nodes with no incoming edges
    (dolist (node nodes)
      (when (= (gethash node in-degree 0) 0)
        (push node queue)))
    ;; Process queue
    (while queue
      (let ((node (pop queue)))
        (push node result)
        (setq count (1+ count))
        (dolist (neighbor (gethash node graph '()))
          (let ((new-degree (1- (gethash neighbor in-degree))))
            (puthash neighbor new-degree in-degree)
            (when (= new-degree 0)
              (push neighbor queue))))))
    ;; Check for cycle
    (if (/= count (length nodes))
        (error "Graph contains a cycle")
      (nreverse result))))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l topological-sort.el
  (let ((graph (make-hash-table :test 'equal))
        (nodes '("a" "b" "c" "d" "e" "f")))
    ;; Build graph: a->b, a->c, b->d, c->d, d->e, e->f
    (puthash "a" '("b" "c") graph)
    (puthash "b" '("d") graph)
    (puthash "c" '("d") graph)
    (puthash "d" '("e") graph)
    (puthash "e" '("f") graph)
    (puthash "f" '() graph)
    (let ((result (topological-sort graph nodes)))
      ;; Verify: 'a' comes before 'b','c'; 'b','c' before 'd'; etc.
      (cl-assert (< (cl-position "a" result :test 'equal)
                    (cl-position "b" result :test 'equal)))
      (cl-assert (< (cl-position "a" result :test 'equal)
                    (cl-position "c" result :test 'equal)))
      (cl-assert (< (cl-position "b" result :test 'equal)
                    (cl-position "d" result :test 'equal)))
      (cl-assert (< (cl-position "d" result :test 'equal)
                    (cl-position "e" result :test 'equal)))
      (cl-assert (< (cl-position "e" result :test 'equal)
                    (cl-position "f" result :test 'equal)))))
  ;; Test cycle detection
  (let ((cyclic-graph (make-hash-table :test 'equal)))
    (puthash "a" '("b") cyclic-graph)
    (puthash "b" '("c") cyclic-graph)
    (puthash "c" '("a") cyclic-graph)
    (condition-case nil
        (progn
          (topological-sort cyclic-graph '("a" "b" "c"))
          (cl-assert nil "Should have detected cycle"))
      (error t)))
  ;; Test empty graph
  (cl-assert (equal (topological-sort (make-hash-table :test 'equal) '()) '()))
  (message "All topological-sort tests passed!"))

(provide 'topological-sort)
;;; topological-sort.el ends here
