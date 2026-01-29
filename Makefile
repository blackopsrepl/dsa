# ╔═══════════════════════════════════════════════════════════════════════════╗
# ║                                                                           ║
# ║     █▀▄ █▀ ▄▀█                                                            ║
# ║     █▄▀ ▄█ █▀█                                                            ║
# ║                                                                           ║
# ║   Data Structures & Algorithms Demonstration Engine                       ║
# ║                                                                           ║
# ╚═══════════════════════════════════════════════════════════════════════════╝
#
# A mathematically elegant build system for exploring DSA implementations
# across multiple programming languages.
#

.DEFAULT_GOAL := help

.PHONY: all help banner list check-deps clean
.PHONY: all-algorithms all-data-structures
.PHONY: algo-binary-search algo-simple-search algo-merge-sort algo-quicksort algo-selection-sort algo-topological-sort
.PHONY: algo-binary-search-python algo-binary-search-go algo-binary-search-ruby algo-binary-search-c algo-binary-search-elisp
.PHONY: algo-simple-search-python algo-simple-search-go algo-simple-search-ruby algo-simple-search-c algo-simple-search-elisp
.PHONY: algo-merge-sort-python algo-merge-sort-ruby algo-merge-sort-c algo-merge-sort-elisp
.PHONY: algo-quicksort-python algo-quicksort-ruby algo-quicksort-c algo-quicksort-elisp
.PHONY: algo-selection-sort-python algo-selection-sort-go algo-selection-sort-ruby algo-selection-sort-c algo-selection-sort-elisp
.PHONY: algo-topological-sort-python algo-topological-sort-ruby algo-topological-sort-c algo-topological-sort-elisp
.PHONY: ds-linked-list ds-dag
.PHONY: ds-linked-list-python ds-linked-list-go ds-linked-list-ruby ds-linked-list-c ds-linked-list-elisp
.PHONY: ds-dag-python ds-dag-go ds-dag-ruby ds-dag-c ds-dag-elisp
.PHONY: lang-python lang-go lang-ruby lang-c lang-elisp

# ═══════════════════════════════════════════════════════════════════════════════
# Configuration
# ═══════════════════════════════════════════════════════════════════════════════

SHELL := /bin/bash
BUILD_DIR := .build

# Colors (ANSI escape codes)
RESET   := \033[0m
BOLD    := \033[1m
DIM     := \033[2m

# Theme colors
GOLD    := \033[38;5;220m
AZURE   := \033[38;5;75m
VIOLET  := \033[38;5;141m
SAGE    := \033[38;5;114m
CORAL   := \033[38;5;210m
WHITE   := \033[38;5;255m
GRAY    := \033[38;5;245m

# Symbols
SYM_PROVEN    := ≡
SYM_DISPROVEN := ×
SYM_RUNNING   := ▶
SYM_FLOW      := ⇨
SYM_ALGO      := λ
SYM_DS        := ◆

# Box drawing
BOX_TL := ┌
BOX_TR := ┐
BOX_BL := └
BOX_BR := ┘
BOX_H  := ─
BOX_V  := │

# ═══════════════════════════════════════════════════════════════════════════════
# Helper Functions
# ═══════════════════════════════════════════════════════════════════════════════

define print_header
	@printf "\n$(AZURE)$(BOX_TL)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_TR)$(RESET)\n"
	@printf "$(AZURE)$(BOX_V)$(RESET)  $(BOLD)$(1)$(RESET)%*s$(AZURE)$(BOX_V)$(RESET)\n" $$(( 58 - $${#1} )) ""
	@printf "$(AZURE)$(BOX_BL)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_BR)$(RESET)\n"
endef

define run_impl
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) $(GRAY)Running$(RESET) $(BOLD)$(1)$(RESET) $(DIM)$(2)$(RESET)\n"
endef

define success
	@printf "  $(GOLD)$(SYM_PROVEN)$(RESET) $(1) $(GOLD)completed$(RESET)\n"
endef

define failure
	@printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) $(1) $(CORAL)failed$(RESET)\n"
endef

define not_available
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) $(1) $(DIM)not available$(RESET)\n"
endef

# ═══════════════════════════════════════════════════════════════════════════════
# Main Targets
# ═══════════════════════════════════════════════════════════════════════════════

all: banner all-algorithms all-data-structures
	@printf "\n"
	$(call print_header,$(SYM_PROVEN) All demonstrations complete)
	@printf "\n"

help: banner
	@printf "\n"
	@printf "$(AZURE)$(BOLD)USAGE$(RESET)\n"
	@printf "  make $(DIM)<target>$(RESET)\n"
	@printf "\n"
	@printf "$(VIOLET)$(BOLD)$(SYM_ALGO) ALGORITHM TARGETS$(RESET)\n"
	@printf "  $(VIOLET)algo-<name>-<lang>$(RESET)    Run specific algorithm in specific language\n"
	@printf "  $(VIOLET)algo-<name>$(RESET)           Run algorithm in all available languages\n"
	@printf "  $(VIOLET)all-algorithms$(RESET)        Run all algorithms\n"
	@printf "\n"
	@printf "  $(DIM)Available algorithms:$(RESET)\n"
	@printf "    $(VIOLET)binary-search$(RESET)       Binary search algorithm\n"
	@printf "    $(VIOLET)simple-search$(RESET)       Linear/simple search algorithm\n"
	@printf "    $(VIOLET)merge-sort$(RESET)          Merge sort algorithm\n"
	@printf "    $(VIOLET)quicksort$(RESET)           Quicksort algorithm\n"
	@printf "    $(VIOLET)selection-sort$(RESET)      Selection sort algorithm\n"
	@printf "    $(VIOLET)topological-sort$(RESET)    Topological sort for DAGs\n"
	@printf "\n"
	@printf "$(SAGE)$(BOLD)$(SYM_DS) DATA STRUCTURE TARGETS$(RESET)\n"
	@printf "  $(SAGE)ds-<name>-<lang>$(RESET)      Run specific data structure in specific language\n"
	@printf "  $(SAGE)ds-<name>$(RESET)             Run data structure in all available languages\n"
	@printf "  $(SAGE)all-data-structures$(RESET)   Run all data structures\n"
	@printf "\n"
	@printf "  $(DIM)Available data structures:$(RESET)\n"
	@printf "    $(SAGE)linked-list$(RESET)         Singly linked list\n"
	@printf "    $(SAGE)dag$(RESET)                 Directed acyclic graph\n"
	@printf "\n"
	@printf "$(AZURE)$(BOLD)$(SYM_FLOW) LANGUAGE TARGETS$(RESET)\n"
	@printf "  $(AZURE)lang-python$(RESET)           All Python implementations\n"
	@printf "  $(AZURE)lang-go$(RESET)               All Go implementations\n"
	@printf "  $(AZURE)lang-ruby$(RESET)             All Ruby implementations\n"
	@printf "  $(AZURE)lang-c$(RESET)                All C implementations\n"
	@printf "  $(AZURE)lang-elisp$(RESET)            All Emacs Lisp implementations\n"
	@printf "\n"
	@printf "$(GOLD)$(BOLD)UTILITY TARGETS$(RESET)\n"
	@printf "  $(GOLD)help$(RESET)                  Show this help message\n"
	@printf "  $(GOLD)banner$(RESET)                Display DSA ASCII art\n"
	@printf "  $(GOLD)list$(RESET)                  Show implementation availability matrix\n"
	@printf "  $(GOLD)check-deps$(RESET)            Verify language runtimes are installed\n"
	@printf "  $(GOLD)clean$(RESET)                 Remove build artifacts\n"
	@printf "  $(GOLD)all$(RESET)                   Run everything\n"
	@printf "\n"
	@printf "$(DIM)Examples:$(RESET)\n"
	@printf "  make algo-quicksort-python    $(DIM)# Run quicksort in Python$(RESET)\n"
	@printf "  make algo-quicksort           $(DIM)# Run quicksort in all languages$(RESET)\n"
	@printf "  make ds-linked-list-go        $(DIM)# Run linked list in Go$(RESET)\n"
	@printf "  make lang-ruby                $(DIM)# Run all Ruby implementations$(RESET)\n"
	@printf "\n"

banner:
	@printf "\n"
	@printf "$(GOLD)                    $(BOX_TL)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_TR)$(RESET)\n"
	@printf "$(GOLD)                    $(BOX_V)$(RESET)               $(BOLD)$(GOLD)█▀▄ █▀ ▄▀█$(RESET)                $(GOLD)$(BOX_V)$(RESET)\n"
	@printf "$(GOLD)                    $(BOX_V)$(RESET)               $(BOLD)$(GOLD)█▄▀ ▄█ █▀█$(RESET)                $(GOLD)$(BOX_V)$(RESET)\n"
	@printf "$(GOLD)                    $(BOX_V)$(RESET)                                         $(GOLD)$(BOX_V)$(RESET)\n"
	@printf "$(GOLD)                    $(BOX_V)$(RESET)      $(AZURE)Data Structures & Algorithms$(RESET)       $(GOLD)$(BOX_V)$(RESET)\n"
	@printf "$(GOLD)                    $(BOX_V)$(RESET)          $(DIM)Demonstration Engine$(RESET)           $(GOLD)$(BOX_V)$(RESET)\n"
	@printf "$(GOLD)                    $(BOX_BL)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_H)$(BOX_BR)$(RESET)\n"

list:
	@printf "\n"
	@printf "$(AZURE)$(BOLD)Implementation Availability Matrix$(RESET)\n"
	@printf "\n"
	@printf "$(VIOLET)$(BOLD)$(SYM_ALGO) Algorithms$(RESET)\n"
	@printf "$(DIM)%-20s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET)\n" "" "Python" "Go" "Ruby" "C" "Elisp"
	@printf "$(DIM)────────────────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET)\n"
	@printf "$(VIOLET)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "binary-search" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "$(VIOLET)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "simple-search" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "$(VIOLET)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GRAY)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "merge-sort" "$(SYM_PROVEN)" "$(SYM_DISPROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "$(VIOLET)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GRAY)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "quicksort" "$(SYM_PROVEN)" "$(SYM_DISPROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "$(VIOLET)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "selection-sort" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "$(VIOLET)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GRAY)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "topological-sort" "$(SYM_PROVEN)" "$(SYM_DISPROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "\n"
	@printf "$(SAGE)$(BOLD)$(SYM_DS) Data Structures$(RESET)\n"
	@printf "$(DIM)%-20s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET) $(AZURE)%-8s$(RESET)\n" "" "Python" "Go" "Ruby" "C" "Elisp"
	@printf "$(DIM)────────────────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET) $(DIM)────────$(RESET)\n"
	@printf "$(SAGE)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "linked-list" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "$(SAGE)%-20s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET) $(GOLD)%-8s$(RESET)\n" "dag" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)" "$(SYM_PROVEN)"
	@printf "\n"
	@printf "$(DIM)Legend: $(GOLD)$(SYM_PROVEN)$(RESET)$(DIM) = available  $(GRAY)$(SYM_DISPROVEN)$(RESET)$(DIM) = not implemented$(RESET)\n"
	@printf "\n"

check-deps:
	@printf "\n"
	@printf "$(AZURE)$(BOLD)Checking Language Runtimes$(RESET)\n"
	@printf "\n"
	@if command -v python3 >/dev/null 2>&1; then \
		printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(DIM)$$(python3 --version 2>&1)$(RESET)\n"; \
	else \
		printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)not found$(RESET)\n"; \
	fi
	@if command -v go >/dev/null 2>&1; then \
		printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Go        $(DIM)$$(go version | cut -d' ' -f3)$(RESET)\n"; \
	else \
		printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Go        $(CORAL)not found$(RESET)\n"; \
	fi
	@if command -v ruby >/dev/null 2>&1; then \
		printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(DIM)$$(ruby --version | cut -d' ' -f2)$(RESET)\n"; \
	else \
		printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)not found$(RESET)\n"; \
	fi
	@if command -v gcc >/dev/null 2>&1; then \
		printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C (gcc)   $(DIM)$$(gcc --version | head -1 | sed 's/.*) //')$(RESET)\n"; \
	else \
		printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C (gcc)   $(CORAL)not found$(RESET)\n"; \
	fi
	@if command -v emacs >/dev/null 2>&1; then \
		printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Emacs     $(DIM)$$(emacs --version | head -1 | cut -d' ' -f3)$(RESET)\n"; \
	else \
		printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Emacs     $(CORAL)not found$(RESET)\n"; \
	fi
	@printf "\n"

clean:
	@printf "\n"
	@printf "$(AZURE)$(BOLD)Cleaning build artifacts$(RESET)\n"
	@rm -rf $(BUILD_DIR)
	@printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Removed $(BUILD_DIR)/\n"
	@printf "\n"

# ═══════════════════════════════════════════════════════════════════════════════
# Algorithm Targets - All Languages
# ═══════════════════════════════════════════════════════════════════════════════

all-algorithms: banner
	$(call print_header,$(SYM_ALGO) Running All Algorithms)
	@$(MAKE) --no-print-directory algo-binary-search
	@$(MAKE) --no-print-directory algo-simple-search
	@$(MAKE) --no-print-directory algo-merge-sort
	@$(MAKE) --no-print-directory algo-quicksort
	@$(MAKE) --no-print-directory algo-selection-sort
	@$(MAKE) --no-print-directory algo-topological-sort

algo-binary-search:
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Binary Search$(RESET)\n"
	@$(MAKE) --no-print-directory algo-binary-search-python
	@$(MAKE) --no-print-directory algo-binary-search-go
	@$(MAKE) --no-print-directory algo-binary-search-ruby
	@$(MAKE) --no-print-directory algo-binary-search-c
	@$(MAKE) --no-print-directory algo-binary-search-elisp

algo-simple-search:
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Simple Search$(RESET)\n"
	@$(MAKE) --no-print-directory algo-simple-search-python
	@$(MAKE) --no-print-directory algo-simple-search-go
	@$(MAKE) --no-print-directory algo-simple-search-ruby
	@$(MAKE) --no-print-directory algo-simple-search-c
	@$(MAKE) --no-print-directory algo-simple-search-elisp

algo-merge-sort:
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Merge Sort$(RESET)\n"
	@$(MAKE) --no-print-directory algo-merge-sort-python
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) Go        $(DIM)not available$(RESET)\n"
	@$(MAKE) --no-print-directory algo-merge-sort-ruby
	@$(MAKE) --no-print-directory algo-merge-sort-c
	@$(MAKE) --no-print-directory algo-merge-sort-elisp

algo-quicksort:
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Quicksort$(RESET)\n"
	@$(MAKE) --no-print-directory algo-quicksort-python
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) Go        $(DIM)not available$(RESET)\n"
	@$(MAKE) --no-print-directory algo-quicksort-ruby
	@$(MAKE) --no-print-directory algo-quicksort-c
	@$(MAKE) --no-print-directory algo-quicksort-elisp

algo-selection-sort:
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Selection Sort$(RESET)\n"
	@$(MAKE) --no-print-directory algo-selection-sort-python
	@$(MAKE) --no-print-directory algo-selection-sort-go
	@$(MAKE) --no-print-directory algo-selection-sort-ruby
	@$(MAKE) --no-print-directory algo-selection-sort-c
	@$(MAKE) --no-print-directory algo-selection-sort-elisp

algo-topological-sort:
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Topological Sort$(RESET)\n"
	@$(MAKE) --no-print-directory algo-topological-sort-python
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) Go        $(DIM)not available$(RESET)\n"
	@$(MAKE) --no-print-directory algo-topological-sort-ruby
	@$(MAKE) --no-print-directory algo-topological-sort-c
	@$(MAKE) --no-print-directory algo-topological-sort-elisp

# ═══════════════════════════════════════════════════════════════════════════════
# Algorithm Targets - Individual Language Implementations
# ═══════════════════════════════════════════════════════════════════════════════

# Binary Search
algo-binary-search-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 algorithms/python/search/binary_search.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

algo-binary-search-go:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Go        " && cd algorithms/go && go run ./search/binary_search/... && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Go        $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Go        $(CORAL)failed$(RESET)\n"

algo-binary-search-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby algorithms/ruby/search/binary_search.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

algo-binary-search-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/binary_search algorithms/c/search/binary_search.c && $(BUILD_DIR)/binary_search && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

algo-binary-search-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l algorithms/elisp/search/binary-search.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# Simple Search
algo-simple-search-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 algorithms/python/search/simple_search.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

algo-simple-search-go:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Go        " && cd algorithms/go && go run ./search/simple_search/... && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Go        $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Go        $(CORAL)failed$(RESET)\n"

algo-simple-search-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby algorithms/ruby/search/simple_search.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

algo-simple-search-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/simple_search algorithms/c/search/simple_search.c && $(BUILD_DIR)/simple_search && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

algo-simple-search-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l algorithms/elisp/search/simple-search.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# Merge Sort
algo-merge-sort-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 algorithms/python/sort/merge_sort.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

algo-merge-sort-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby algorithms/ruby/sort/merge_sort.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

algo-merge-sort-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/merge_sort algorithms/c/sort/merge_sort.c && $(BUILD_DIR)/merge_sort && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

algo-merge-sort-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l algorithms/elisp/sort/merge-sort.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# Quicksort
algo-quicksort-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 algorithms/python/sort/quicksort.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

algo-quicksort-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby algorithms/ruby/sort/quicksort.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

algo-quicksort-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/quicksort algorithms/c/sort/quicksort.c && $(BUILD_DIR)/quicksort && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

algo-quicksort-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l algorithms/elisp/sort/quicksort.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# Selection Sort
algo-selection-sort-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 algorithms/python/sort/selection_sort_array.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

algo-selection-sort-go:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Go        " && cd algorithms/go && go run ./sort/selection_sort_array/... && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Go        $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Go        $(CORAL)failed$(RESET)\n"

algo-selection-sort-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby algorithms/ruby/sort/selection_sort_array.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

algo-selection-sort-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/selection_sort algorithms/c/sort/selection_sort_array.c && $(BUILD_DIR)/selection_sort && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

algo-selection-sort-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l algorithms/elisp/sort/selection-sort.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# Topological Sort
algo-topological-sort-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 algorithms/python/sort/topological_sort_graph.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

algo-topological-sort-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby algorithms/ruby/sort/topological_sort_graph.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

algo-topological-sort-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/topological_sort algorithms/c/sort/topological_sort_graph.c && $(BUILD_DIR)/topological_sort && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

algo-topological-sort-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l algorithms/elisp/sort/topological-sort.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# ═══════════════════════════════════════════════════════════════════════════════
# Data Structure Targets - All Languages
# ═══════════════════════════════════════════════════════════════════════════════

all-data-structures: banner
	$(call print_header,$(SYM_DS) Running All Data Structures)
	@$(MAKE) --no-print-directory ds-linked-list
	@$(MAKE) --no-print-directory ds-dag

ds-linked-list:
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Linked List$(RESET)\n"
	@$(MAKE) --no-print-directory ds-linked-list-python
	@$(MAKE) --no-print-directory ds-linked-list-go
	@$(MAKE) --no-print-directory ds-linked-list-ruby
	@$(MAKE) --no-print-directory ds-linked-list-c
	@$(MAKE) --no-print-directory ds-linked-list-elisp

ds-dag:
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Directed Acyclic Graph$(RESET)\n"
	@$(MAKE) --no-print-directory ds-dag-python
	@$(MAKE) --no-print-directory ds-dag-go
	@$(MAKE) --no-print-directory ds-dag-ruby
	@$(MAKE) --no-print-directory ds-dag-c
	@$(MAKE) --no-print-directory ds-dag-elisp

# ═══════════════════════════════════════════════════════════════════════════════
# Data Structure Targets - Individual Language Implementations
# ═══════════════════════════════════════════════════════════════════════════════

# Linked List
ds-linked-list-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 data_structures/python/linked_list/single_linked_list.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

ds-linked-list-go:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Go        " && go run data_structures/go/linked_list/single_linked_list.go && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Go        $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Go        $(CORAL)failed$(RESET)\n"

ds-linked-list-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby data_structures/ruby/linked_list/single_linked_list.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

ds-linked-list-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/linked_list data_structures/c/linked_list/single_linked_list.c && $(BUILD_DIR)/linked_list && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

ds-linked-list-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l data_structures/elisp/linked_list/single-linked-list.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# DAG
ds-dag-python:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Python    " && python3 data_structures/python/graph/directed_acyclic_graph.py && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Python    $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Python    $(CORAL)failed$(RESET)\n"

ds-dag-go:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Go        " && go run data_structures/go/graph/directed_acyclic_graph.go && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Go        $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Go        $(CORAL)failed$(RESET)\n"

ds-dag-ruby:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Ruby      " && ruby data_structures/ruby/graph/directed_acyclic_graph.rb && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Ruby      $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Ruby      $(CORAL)failed$(RESET)\n"

ds-dag-c:
	@mkdir -p $(BUILD_DIR)
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) C         " && gcc -Wall -Wextra -std=c11 -o $(BUILD_DIR)/dag data_structures/c/graph/directed_acyclic_graph.c && $(BUILD_DIR)/dag && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) C         $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) C         $(CORAL)failed$(RESET)\n"

ds-dag-elisp:
	@printf "  $(AZURE)$(SYM_RUNNING)$(RESET) Elisp     " && emacs --batch -l data_structures/elisp/graph/directed-acyclic-graph.el 2>&1 && printf "  $(GOLD)$(SYM_PROVEN)$(RESET) Elisp     $(GOLD)completed$(RESET)\n" || printf "  $(CORAL)$(SYM_DISPROVEN)$(RESET) Elisp     $(CORAL)failed$(RESET)\n"

# ═══════════════════════════════════════════════════════════════════════════════
# Language Targets
# ═══════════════════════════════════════════════════════════════════════════════

lang-python: banner
	$(call print_header,Python Implementations)
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Algorithms$(RESET)\n"
	@$(MAKE) --no-print-directory algo-binary-search-python
	@$(MAKE) --no-print-directory algo-simple-search-python
	@$(MAKE) --no-print-directory algo-merge-sort-python
	@$(MAKE) --no-print-directory algo-quicksort-python
	@$(MAKE) --no-print-directory algo-selection-sort-python
	@$(MAKE) --no-print-directory algo-topological-sort-python
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Data Structures$(RESET)\n"
	@$(MAKE) --no-print-directory ds-linked-list-python
	@$(MAKE) --no-print-directory ds-dag-python
	@printf "\n"

lang-go: banner
	$(call print_header,Go Implementations)
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Algorithms$(RESET)\n"
	@$(MAKE) --no-print-directory algo-binary-search-go
	@$(MAKE) --no-print-directory algo-simple-search-go
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) merge-sort     $(DIM)not available$(RESET)\n"
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) quicksort      $(DIM)not available$(RESET)\n"
	@$(MAKE) --no-print-directory algo-selection-sort-go
	@printf "  $(GRAY)$(SYM_DISPROVEN)$(RESET) topological-sort $(DIM)not available$(RESET)\n"
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Data Structures$(RESET)\n"
	@$(MAKE) --no-print-directory ds-linked-list-go
	@$(MAKE) --no-print-directory ds-dag-go
	@printf "\n"

lang-ruby: banner
	$(call print_header,Ruby Implementations)
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Algorithms$(RESET)\n"
	@$(MAKE) --no-print-directory algo-binary-search-ruby
	@$(MAKE) --no-print-directory algo-simple-search-ruby
	@$(MAKE) --no-print-directory algo-merge-sort-ruby
	@$(MAKE) --no-print-directory algo-quicksort-ruby
	@$(MAKE) --no-print-directory algo-selection-sort-ruby
	@$(MAKE) --no-print-directory algo-topological-sort-ruby
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Data Structures$(RESET)\n"
	@$(MAKE) --no-print-directory ds-linked-list-ruby
	@$(MAKE) --no-print-directory ds-dag-ruby
	@printf "\n"

lang-c: banner
	$(call print_header,C Implementations)
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Algorithms$(RESET)\n"
	@$(MAKE) --no-print-directory algo-binary-search-c
	@$(MAKE) --no-print-directory algo-simple-search-c
	@$(MAKE) --no-print-directory algo-merge-sort-c
	@$(MAKE) --no-print-directory algo-quicksort-c
	@$(MAKE) --no-print-directory algo-selection-sort-c
	@$(MAKE) --no-print-directory algo-topological-sort-c
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Data Structures$(RESET)\n"
	@$(MAKE) --no-print-directory ds-linked-list-c
	@$(MAKE) --no-print-directory ds-dag-c
	@printf "\n"

lang-elisp: banner
	$(call print_header,Emacs Lisp Implementations)
	@printf "\n$(VIOLET)$(SYM_ALGO) $(BOLD)Algorithms$(RESET)\n"
	@$(MAKE) --no-print-directory algo-binary-search-elisp
	@$(MAKE) --no-print-directory algo-simple-search-elisp
	@$(MAKE) --no-print-directory algo-merge-sort-elisp
	@$(MAKE) --no-print-directory algo-quicksort-elisp
	@$(MAKE) --no-print-directory algo-selection-sort-elisp
	@$(MAKE) --no-print-directory algo-topological-sort-elisp
	@printf "\n$(SAGE)$(SYM_DS) $(BOLD)Data Structures$(RESET)\n"
	@$(MAKE) --no-print-directory ds-linked-list-elisp
	@$(MAKE) --no-print-directory ds-dag-elisp
	@printf "\n"
