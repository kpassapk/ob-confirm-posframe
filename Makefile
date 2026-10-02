EMACS ?= emacs
# Watchdog: batch ERT can hang on stdin/loops. Kill after TIMEOUT seconds.
TIMEOUT ?= 120

EL := ob-confirm-posframe.el
TEST := ob-confirm-posframe-test.el

# package-initialize puts posframe on the load path.
BATCH := $(EMACS) --batch -L . \
  --eval "(setq native-comp-enable-subr-trampolines nil)" \
  --eval "(package-initialize)"
WATCHDOG := perl -e 'alarm shift; exec @ARGV' $(TIMEOUT)

.PHONY: test test-% compile clean

## Run full ERT suite
test:
	$(WATCHDOG) $(BATCH) \
	  --eval "(require 'ob-confirm-posframe-test)" \
	  -f ert-run-tests-batch-and-exit

## Run tests matching a regex: make test-PATTERN
test-%:
	$(WATCHDOG) $(BATCH) \
	  --eval "(require 'ob-confirm-posframe-test)" \
	  --eval "(ert-run-tests-batch-and-exit \"$*\")"

## Byte-compile sources, treat warnings as errors
compile:
	$(BATCH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(EL) $(TEST)

clean:
	rm -f *.elc
