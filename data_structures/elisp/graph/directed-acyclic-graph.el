;;; directed-acyclic-graph.el --- DAG with cycle detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Directed Acyclic Graph implementation with cycle detection and topological sort.

;;; Code:

(require 'cl-lib)

(cl-defstruct (dag (:constructor dag-create ()))
  "A directed acyclic graph."
  (graph (make-hash-table :test 'equal))    ; adjacency list: task -> list of dependencies
  (tasks (make-hash-table :test 'equal)))   ; task descriptions: task -> description

(defun dag-add-task (dag task-id description)
  "Add a task with TASK-ID and DESCRIPTION to DAG."
  (puthash task-id description (dag-tasks dag))
  (unless (gethash task-id (dag-graph dag))
    (puthash task-id '() (dag-graph dag)))
  dag)

(defun dag-add-dependency (dag from-task to-task)
  "Add edge FROM-TASK -> TO-TASK in DAG (from-task depends on to-task completing).
Returns nil if this would create a cycle, t otherwise."
  ;; Ensure both tasks exist in graph
  (unless (gethash from-task (dag-graph dag))
    (puthash from-task '() (dag-graph dag)))
  (unless (gethash to-task (dag-graph dag))
    (puthash to-task '() (dag-graph dag)))
  ;; Add edge temporarily
  (let ((current-deps (gethash from-task (dag-graph dag))))
    (unless (member to-task current-deps)
      (puthash from-task (cons to-task current-deps) (dag-graph dag))
      ;; Check for cycle
      (if (dag-has-cycle dag)
          (progn
            ;; Remove the edge we just added
            (puthash from-task current-deps (dag-graph dag))
            nil)
        t))))

(defun dag--dfs-cycle-check (dag node visited rec-stack)
  "Check for cycles using DFS from NODE.
VISITED tracks all visited nodes, REC-STACK tracks current path."
  (puthash node t visited)
  (puthash node t rec-stack)
  (let ((has-cycle nil))
    (dolist (neighbor (gethash node (dag-graph dag) '()))
      (cond
       ((gethash neighbor rec-stack)
        (setq has-cycle t))
       ((not (gethash neighbor visited))
        (when (dag--dfs-cycle-check dag neighbor visited rec-stack)
          (setq has-cycle t)))))
    (puthash node nil rec-stack)
    has-cycle))

(defun dag-has-cycle (dag)
  "Return t if DAG contains a cycle, nil otherwise."
  (let ((visited (make-hash-table :test 'equal))
        (rec-stack (make-hash-table :test 'equal))
        (has-cycle nil))
    (maphash (lambda (node _)
               (unless (or has-cycle (gethash node visited))
                 (when (dag--dfs-cycle-check dag node visited rec-stack)
                   (setq has-cycle t))))
             (dag-graph dag))
    has-cycle))

(defun dag-topological-sort (dag)
  "Return topologically sorted list of tasks in DAG.
Tasks with no dependencies come first.
Signals error if cycle detected."
  (let ((out-degree (make-hash-table :test 'equal))  ; count of dependencies
        (dependents (make-hash-table :test 'equal))  ; reverse graph: who depends on me
        (nodes '())
        (queue '())
        (result '())
        (count 0))
    ;; Collect all nodes and initialize out-degrees (dependency count)
    (maphash (lambda (node deps)
               (push node nodes)
               (puthash node (length deps) out-degree))
             (dag-graph dag))
    ;; Build reverse graph: for each dependency, track who depends on it
    (maphash (lambda (from to-list)
               (dolist (to to-list)
                 (puthash to (cons from (gethash to dependents '())) dependents)))
             (dag-graph dag))
    ;; Find nodes with no dependencies (out-degree = 0)
    (dolist (node nodes)
      (when (= (gethash node out-degree 0) 0)
        (push node queue)))
    ;; Process queue
    (while queue
      (let ((node (pop queue)))
        (push node result)
        (setq count (1+ count))
        ;; For each task that depends on this node, decrement their dependency count
        (dolist (dependent (gethash node dependents '()))
          (let ((new-degree (1- (gethash dependent out-degree))))
            (puthash dependent new-degree out-degree)
            (when (= new-degree 0)
              (push dependent queue))))))
    ;; Check for cycle
    (if (/= count (length nodes))
        (error "Graph contains a cycle")
      (nreverse result))))

(defun dag-get-all-tasks (dag)
  "Return list of all task IDs in DAG."
  (let ((tasks '()))
    (maphash (lambda (k _) (push k tasks)) (dag-tasks dag))
    (nreverse tasks)))

(defun dag-get-task-description (dag task-id)
  "Return description of TASK-ID in DAG."
  (gethash task-id (dag-tasks dag)))

(defun dag-get-dependencies (dag task-id)
  "Return list of tasks that TASK-ID depends on."
  (gethash task-id (dag-graph dag) '()))

;;; Tests

(when noninteractive
  ;; Tests run when: emacs --batch -l directed-acyclic-graph.el

  ;; Test basic task creation
  (let ((d (dag-create)))
    (dag-add-task d "task1" "First task")
    (dag-add-task d "task2" "Second task")
    (cl-assert (equal (dag-get-task-description d "task1") "First task"))
    (cl-assert (equal (dag-get-task-description d "task2") "Second task")))

  ;; Test dependencies
  (let ((d (dag-create)))
    (dag-add-task d "a" "Task A")
    (dag-add-task d "b" "Task B")
    (dag-add-task d "c" "Task C")
    (cl-assert (dag-add-dependency d "b" "a"))  ; b depends on a
    (cl-assert (dag-add-dependency d "c" "b"))  ; c depends on b
    (cl-assert (member "a" (dag-get-dependencies d "b")))
    (cl-assert (member "b" (dag-get-dependencies d "c"))))

  ;; Test cycle detection
  (let ((d (dag-create)))
    (dag-add-task d "x" "Task X")
    (dag-add-task d "y" "Task Y")
    (dag-add-task d "z" "Task Z")
    (cl-assert (dag-add-dependency d "y" "x"))
    (cl-assert (dag-add-dependency d "z" "y"))
    ;; This would create cycle: x -> y -> z -> x
    (cl-assert (null (dag-add-dependency d "x" "z")))
    (cl-assert (not (dag-has-cycle d))))

  ;; Test topological sort
  (let ((d (dag-create)))
    (dag-add-task d "a" "Task A")
    (dag-add-task d "b" "Task B")
    (dag-add-task d "c" "Task C")
    (dag-add-task d "d" "Task D")
    (dag-add-dependency d "b" "a")  ; b depends on a
    (dag-add-dependency d "c" "a")  ; c depends on a
    (dag-add-dependency d "d" "b")  ; d depends on b
    (dag-add-dependency d "d" "c")  ; d depends on c
    (let ((sorted (dag-topological-sort d)))
      ;; 'a' must come before 'b' and 'c'; 'b' and 'c' must come before 'd'
      (cl-assert (< (cl-position "a" sorted :test 'equal)
                    (cl-position "b" sorted :test 'equal)))
      (cl-assert (< (cl-position "a" sorted :test 'equal)
                    (cl-position "c" sorted :test 'equal)))
      (cl-assert (< (cl-position "b" sorted :test 'equal)
                    (cl-position "d" sorted :test 'equal)))
      (cl-assert (< (cl-position "c" sorted :test 'equal)
                    (cl-position "d" sorted :test 'equal)))))

  ;; Test empty DAG
  (let ((d (dag-create)))
    (cl-assert (equal (dag-topological-sort d) '()))
    (cl-assert (not (dag-has-cycle d))))

  (message "All directed-acyclic-graph tests passed!"))

(provide 'directed-acyclic-graph)
;;; directed-acyclic-graph.el ends here
