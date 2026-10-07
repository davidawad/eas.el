# eas.el: an Emacs-native, interactive, agent-drivable chart engine.
#
#   make test             unit, golden and runtime tests (fast; no :gallery)
#   make compile          checkdoc, then byte-compile src/ with warnings as errors
#   make checkdoc         checkdoc over the library files; any warning fails
#   make test-gallery     every gallery group, one Emacs each, in sequence
#   make test-gallery-GROUP           one official Vega-Lite gallery group
#   make test-gallery-conformance     the bin/chart conformance oracle
#   make test-gallery-vega            templates/vega against test/vega-examples/ref
#   make bench            the 1k/10k/100k ladder against bench-budget.json
#   make bench-budget     re-measure the budget's references (review the diff)
#   make tty-check        real-terminal check in tmux (private server -L eas)
#   make melpa-check      recipes/eas installed flat: compile, doctor, package-lint
#   make clean            remove byte-compiled files
#
# EAS_UPDATE_GOLDEN=1 rewrites goldens; TEST_SKIP_LOG=FILE logs skips.

EMACS ?= emacs
LOAD := -L src -L test/eas
BATCH := $(EMACS) -Q --batch $(LOAD) --eval '(setq load-prefer-newer t)'

LIB := $(filter-out %-test.el,$(wildcard src/*.el))
TESTS := $(wildcard src/*-test.el test/eas/*-test.el)

GALLERY_GROUPS := area-circular bar calculations distributions interactive \
                  layered line multiview scatter-table
GALLERY_TARGETS := $(addprefix test-gallery-,$(GALLERY_GROUPS))

# Load every test file (src/ and test/eas/), then run the tests SELECTOR picks.
define run-tests
$(BATCH) --eval '(dolist (f (append (directory-files "src" t "-test\\.el\\'"'"'") (directory-files "test/eas" t "-test\\.el\\'"'"'"))) (load f nil t))' \
  --eval '(ert-run-tests-batch-and-exit (quote $(1)))'
endef

.PHONY: all test compile checkdoc test-gallery $(GALLERY_TARGETS) \
        test-gallery-conformance test-gallery-vega bench bench-budget tty-check melpa-check clean

all: compile test

test:
	$(call run-tests,(not (tag :gallery)))

compile: checkdoc
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(LIB) $(TESTS)

checkdoc:
	$(BATCH) -l scripts/eas-checkdoc.el -f eas-checkdoc-batch $(LIB)

test-gallery: $(GALLERY_TARGETS) test-gallery-conformance test-gallery-vega

$(GALLERY_TARGETS): test-gallery-%:
	EAS_GALLERY_GROUPS=$* $(call run-tests,(and (tag :gallery) (or "^eas-vl-gallery-groups-hold-their-status$$" "^eas-vl-gallery-groups-hold-their-text-status$$")))

test-gallery-conformance:
	$(call run-tests,(and (tag :gallery) (or "^eas-conformance-" "^eas-text-gallery-templates-" "^eas-vega-.*gallery")))

test-gallery-vega:
	$(call run-tests,(and (tag :gallery) "^eas-vega-"))

test-gallery-vega:
	$(call run-tests,(and (tag :gallery) "^eas-vega-"))

# The Vega gallery templates (templates/vega/) against test/vega-examples/ref.
test-gallery-vega:
	$(call run-tests,(and (tag :gallery) "^eas-vega-"))

bench:
	scripts/eas-bench.sh

bench-budget:
	scripts/eas-bench.sh --update

tty-check:
	scripts/eas-tty-check.sh

# Needs the network: package-build and package-lint come from MELPA.
melpa-check:
	scripts/melpa-layout-check

clean:
	rm -f src/*.elc test/eas/*.elc scripts/*.elc
