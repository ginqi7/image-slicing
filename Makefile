# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: test solve-dependencies

EMACS_GENERIC_OPTS=--quick --directory . --directory .deps
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM=@rm -rf

solve-dependencies:
	@echo "Installing dependencies..."
	@mkdir -p ~/.emacs.d/lisp
	@if [ ! -d ~/.emacs.d/lisp/dash ]; then \
		git clone https://github.com/magnars/dash.el ~/.emacs.d/lisp/dash; \
	else \
		echo "dash already exists, skipping..."; \
	fi
	@mkdir -p ~/.emacs.d/lisp
	@if [ ! -d ~/.emacs.d/lisp/f ]; then \
		git clone http://github.com/rejeep/f.el ~/.emacs.d/lisp/f; \
	else \
		echo "f already exists, skipping..."; \
	fi
	@if [ ! -d ~/.emacs.d/lisp/s ]; then \
		git clone https://github.com/magnars/s.el ~/.emacs.d/lisp/s; \
	else \
		echo "s already exists, skipping..."; \
	fi
	@echo "Dependencies installed installed successfully."

test:   solve-dependencies
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/image-slicing-tests.el
