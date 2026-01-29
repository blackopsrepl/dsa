# Data Structures and Algorithms

A collection of data structures and algorithms implemented across multiple programming languages: **Python**, **Go**, **Ruby**, **C**, and **Emacs Lisp**.

## Quick Start

```bash
make              # Show help
make all          # Run all implementations
make list         # Show implementation matrix
make check-deps   # Verify language runtimes
```

## Algorithms

| Algorithm | Python | Go | Ruby | C | Elisp |
|-----------|--------|-----|------|---|-------|
| Binary Search | ✓ | ✓ | ✓ | ✓ | ✓ |
| Linear Search | ✓ | ✓ | ✓ | ✓ | ✓ |
| Merge Sort | ✓ | - | ✓ | ✓ | ✓ |
| Quicksort | ✓ | - | ✓ | ✓ | ✓ |
| Selection Sort | ✓ | ✓ | ✓ | ✓ | ✓ |
| Topological Sort | ✓ | - | ✓ | ✓ | ✓ |

## Data Structures

| Structure | Python | Go | Ruby | C | Elisp |
|-----------|--------|-----|------|---|-------|
| Linked List | ✓ | ✓ | ✓ | ✓ | ✓ |
| DAG | ✓ | ✓ | ✓ | ✓ | ✓ |

## Usage Examples

```bash
# Run a specific algorithm in one language
make algo-quicksort-python

# Run an algorithm in all available languages
make algo-quicksort

# Run all implementations for a language
make lang-ruby

# Run a specific data structure
make ds-linked-list-go
```
