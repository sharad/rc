
VERSION := $(shell perl -nlE '/^;; Version: (.*)/ && print $$1;' pydoc-info.el)

DIST := pydoc-info-$(VERSION)


all: info dist

info:
	cd docs && make info

dist:
	rm -rf $(DIST)/
	mkdir $(DIST)
	cp -t $(DIST) pydoc-info.el pydoc-info-pkg.el \
	    docs/python.texi docs/python.info
	( cd $(DIST) && \
	    install-info python.info dir && \
	    gzip python.texi && \
	    gzip python.info )
	tar cf $(DIST).tar $(DIST)

clean:
	-rm *.elc
	-rm -rf $(DIST) $(DIST).tar
	-cd docs && make clean
