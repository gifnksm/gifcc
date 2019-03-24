OUTDIR=../target/$(STAGE)/test
GIFCC=../target/$(STAGE)/gifcc

HEADERS=$(wildcard *.h)
SRCS=$(wildcard *.c)
ASMS=$(patsubst %.c,$(OUTDIR)/%.s,$(SRCS))
EXES=$(patsubst %.c,$(OUTDIR)/%.s,$(SRCS))
NAMES=$(patsubst %.c,%,$(SRCS))

run: $(foreach name,$(NAMES),run-$(name))
.PHONY: run

build: $(foreach name,$(NAMES),build-$(name))
.PHONY: build

define build-rules
run-$(1): build-$(1)
	$(OUTDIR)/$(1)
.PHONY: run-$(1)

build-$(1): $(OUTDIR)/$(1).s $(OUTDIR)/$(1)
.PHONY: build-$(1)
endef
$(foreach name,$(NAMES),$(eval $(call build-rules,$(name))))

ifeq ($(STAGE),gcc)
$(OUTDIR)/%.s: %.c $(HEADERS) | $(OUTDIR)/
	gcc -S -g3 -w -o $@ $<
else
$(OUTDIR)/%.s: %.c $(HEADERS) $(GIFCC) | $(OUTDIR)/
	$(GIFCC) $< --emit all -o $@
endif

$(OUTDIR)/%: $(OUTDIR)/%.s
	gcc -o $@ $<

%/:
	mkdir -p $@

.PRECIOUS: %/
