export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

clean:
	cask clean-elc

.PHONY: compile
compile: cask clean
	cask build

.PHONY: test
test:
	cask exec buttercup --traceback full -L . test

.PHONY: coverage
coverage: clean
	mkdir -p coverage
	UNDERCOVER_FORCE=true cask exec buttercup --traceback full -L . test
