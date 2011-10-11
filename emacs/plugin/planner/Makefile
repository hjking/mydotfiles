.PHONY: all autoloads lisp doc clean realclean distclean fullclean install
.PHONY: test dist release upload elpa info-only
.PRECIOUS: %.elc

DEFS = $(shell test -f Makefile.defs && echo Makefile.defs \
	|| echo Makefile.defs.default)

include $(DEFS)

EL  = $(filter-out $(PROJECT)-autoloads.el,$(wildcard *.el))
ELC = $(patsubst %.el,%.elc,$(EL))

all: autoloads lisp $(MANUAL).info

lisp: $(ELC)

$(PROJECT)-build.elc: ./scripts/$(PROJECT)-build.el
	@echo $(PROJECT)-build.el is not byte-compiled

autoloads: $(PROJECT)-autoloads.el

$(PROJECT)-autoloads.el: $(EL)
	@$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/$(PROJECT)-build.el \
		-f $(PROJECT)-generate-autoloads . contrib

%.elc: %.el
	@$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/$(PROJECT)-build.el \
		-f batch-byte-compile $< || :

%.info: %.texi
	makeinfo $<

%.html: %.texi
	makeinfo --html --no-split $<

info-only: $(MANUAL).info

doc: $(MANUAL).info $(MANUAL).html

clean:
	-rm -f *.elc *~

realclean fullclean: clean
	-rm -f $(MANUAL).info $(MANUAL).html $(PROJECT)-autoloads.el

install: autoloads lisp $(MANUAL).info
	install -d $(ELISPDIR)
	install -m 0644 $(PROJECT)-autoloads.el $(EL) $(wildcard *.elc) \
	    $(ELISPDIR)
	[ -d $(INFODIR) ] || install -d $(INFODIR)
	install -m 0644 $(MANUAL).info $(INFODIR)/$(MANUAL)
	$(call install_info,$(MANUAL))

test: $(ELC)
	$(EMACS) -q $(SITEFLAG) -batch -l ./scripts/$(PROJECT)-build.el \
		-f $(PROJECT)-elint-files $(EL)

distclean:
	-rm -f $(MANUAL).info $(MANUAL).html
	-rm -fr ../$(PROJECT)-$(VERSION)

dist: autoloads distclean
	git archive --format=tar --prefix=$(PROJECT)-$(VERSION)/ HEAD | \
	  (cd .. && tar xf -)
	rm -f ../$(PROJECT)-$(VERSION)/.gitignore
	rm -fr ../$(PROJECT)-$(VERSION)/test
	cp $(PROJECT)-autoloads.el ../$(PROJECT)-$(VERSION)/lisp

release: dist
	(cd .. && tar -czf $(PROJECT)-$(VERSION).tar.gz \
	          $(PROJECT)-$(VERSION) && \
	  zip -r $(PROJECT)-$(VERSION).zip $(PROJECT)-$(VERSION) && \
	  gpg --detach $(PROJECT)-$(VERSION).tar.gz && \
	  gpg --detach $(PROJECT)-$(VERSION).zip)

upload: release
	(cd .. && \
	  scp $(PROJECT)-$(VERSION).zip* $(PROJECT)-$(VERSION).tar.gz* \
	    johnsu01@download.gna.org:/upload/planner-el)

elpa: realclean info-only
	rm -fR $(ELPADIR)/$(PROJECT)-$(VERSION)
	rm -f $(ELPADIR)/$(PROJECT)-$(VERSION).tar
	mkdir -p $(ELPADIR)/$(PROJECT)-$(VERSION)
	cp *.el $(ELPADIR)/$(PROJECT)-$(VERSION)
	cp contrib/*.el $(ELPADIR)/$(PROJECT)-$(VERSION)
	echo '(define-package "$(PROJECT)" "$(VERSION)"' > \
	  $(ELPADIR)/$(PROJECT)-$(VERSION)/$(PROJECT)-pkg.el
	echo '  "$(ELPADESC)")' >> \
	  $(ELPADIR)/$(PROJECT)-$(VERSION)/$(PROJECT)-pkg.el
	cp texi/$(MANUAL).info $(ELPADIR)/$(PROJECT)-$(VERSION)
	cp texi/dir-template $(ELPADIR)/$(PROJECT)-$(VERSION)/dir
	install-info --section "Emacs" "Emacs" \
	  --info-dir=$(ELPADIR)/$(PROJECT)-$(VERSION) \
	  $(ELPADIR)/$(PROJECT)-$(VERSION)/$(MANUAL).info
	rm -f $(ELPADIR)/$(PROJECT)-$(VERSION)/dir.old
	(cd $(ELPADIR) && tar cf $(PROJECT)-$(VERSION).tar \
	  $(PROJECT)-$(VERSION))
