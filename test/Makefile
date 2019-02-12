OUTDIR=../target/test
GIFCC=../target/gifcc

SRCS=$(wildcard *.c)
ASMS=$(patsubst %.c,$(OUTDIR)/%.s,$(SRCS))
EXES=$(patsubst %.c,$(OUTDIR)/%.s,$(SRCS))
NAMES=$(patsubst %.c,%,$(SRCS))

run: $(foreach name,$(NAMES),run-$(name))
.PHONY: run

build: $(foreach name,$(NAMES),build-$(name))
.PHONY: build

gcc-run: $(foreach name,$(NAMES),gcc-run-$(name))
.PHONY: gcc-run

gcc-build: $(foreach name,$(NAMES),gcc-build-$(name))
.PHONY: gcc-build

define defrules
build-$(1): $(OUTDIR)/$(1).token $(OUTDIR)/$(1).ast $(OUTDIR)/$(1).s $(OUTDIR)/$(1)
.PHONY: build-$(1)

run-$(1): build-$(1)
	$(OUTDIR)/$(1)
.PHONY: run-$(1)

gcc-build-$(1): $(OUTDIR)/$(1).gcc
.PHONY: gcc-build-$(1)

gcc-run-$(1): $(OUTDIR)/$(1).gcc
	$(OUTDIR)/$(1).gcc
.PHONY: gcc-run-$(1)
endef
$(foreach name,$(NAMES),$(eval $(call defrules,$(name))))

$(OUTDIR)/%.token: %.c $(GIFCC) | $(OUTDIR)/
	$(GIFCC) --output token $< > $@.tmp
	mv $@.tmp $@

$(OUTDIR)/%.ast: %.c $(GIFCC) | $(OUTDIR)/
	$(GIFCC) --output ast $< > $@.tmp
	mv $@.tmp $@

$(OUTDIR)/%.s: %.c $(GIFCC) | $(OUTDIR)/
	$(GIFCC) $< > $@.tmp
	mv $@.tmp $@

$(OUTDIR)/%: $(OUTDIR)/%.s
	gcc -o $@ $<

$(OUTDIR)/%.gcc: %.c | $(OUTDIR)/
	gcc -w -o $@ $<

%/:
	mkdir -p $@

.PRECIOUS: %/
