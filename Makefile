export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask build

.PHONY: test
test: compile
	cask exec buttercup --traceback full -L . test

.PHONY: coverage
coverage:
	mkdir -p coverage
	make -e UNDERCOVER_FORCE=true test
