.PHONY: build test coverage coverage-html coverage-clean

build:
	dune build

test:
	dune runtest

# Coverage pipeline — generates summary + per-file report
coverage: coverage-clean
	BISECT_FILE=$(CURDIR)/bisect dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report summary --coverage-path=$(CURDIR) --per-file
	@echo "---"
	@echo "For HTML report: make coverage-html"

coverage-html: coverage-clean
	BISECT_FILE=$(CURDIR)/bisect dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path=$(CURDIR)
	@echo "HTML report: _coverage/index.html"

coverage-clean:
	rm -f bisect*.coverage
	rm -rf _coverage
