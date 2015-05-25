install: cabal.sandbox.config
	cabal install --dependencies-only

cabal.sandbox.config:
	cabal sandbox init

	cabal install gtk2hs-buildtools
	PATH=.cabal-sandbox/bin:$$PATH cabal install gtk

	cabal sandbox add-source chart
	cabal sandbox add-source chart-cairo
	cabal sandbox add-source chart-gtk
	cabal sandbox add-source chart-diagrams

unregister:
	-cabal sandbox hc-pkg unregister Chart-tests
	-cabal sandbox hc-pkg unregister Chart-diagrams
	-cabal sandbox hc-pkg unregister Chart-gtk
	-cabal sandbox hc-pkg unregister Chart-cairo
	-cabal sandbox hc-pkg unregister Chart

clean: unregister
	cd chart && cabal clean
	cd chart-cairo && cabal clean
	cd chart-gtk && cabal clean
	cd chart-diagrams && cabal clean
	cd chart-tests && cabal clean

clean-sandbox: clean
	rm -rf cabal.sandbox.config .cabal-sandbox
	rm -f chart-tests/cabal.sandbox.config

sdist:
	cd chart && cabal sdist
	cd chart-cairo && cabal sdist
	cd chart-gtk && cabal sdist
	cd chart-diagrams && cabal sdist

# define environment variable CHART_VERSION
# before making upload
upload:
	cd chart && cabal upload dist/Chart-$(CHART_VERSION).tar.gz
	cd chart-cairo && cabal upload dist/Chart-cairo-$(CHART_VERSION).tar.gz
	cd chart-gtk && cabal upload dist/Chart-gtk-$(CHART_VERSION).tar.gz
	cd chart-diagrams && cabal upload dist/Chart-diagrams-$(CHART_VERSION).tar.gz

chart-tests/cabal.sandbox.config:
	cd chart-tests && cabal sandbox init --sandbox ../.cabal-sandbox

ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
HARNESS=$(ROOT_DIR)/.cabal-sandbox/bin/harness

tests: chart-tests/cabal.sandbox.config
	cd chart-tests && cabal install --dependencies-only
	cd chart-tests && cabal install
	mkdir -p chart-tests/output/fonts
	mkdir -p chart-tests/output/drawing/{cairo,diagrams}
	mkdir -p chart-tests/output/charts/{cairo-png,cairo-svg,cairo-ps,cairo-pdf}
	mkdir -p chart-tests/output/charts/{diagrams-png,diagrams-svg,diagrams-eps}
	cd chart-tests/output/drawing/cairo && $(HARNESS) drawing-cairo
	cd chart-tests/output/drawing/diagrams && $(HARNESS) drawing-diagrams
	cd chart-tests/output/fonts && $(HARNESS) compare-fonts
	cd chart-tests/output/charts/cairo-png && $(HARNESS) charts-cairo --png
	cd chart-tests/output/charts/cairo-ps && $(HARNESS) charts-cairo --ps
	cd chart-tests/output/charts/cairo-svg && $(HARNESS) charts-cairo --svg
	cd chart-tests/output/charts/cairo-pdf && $(HARNESS) charts-cairo --pdf
	cd chart-tests/output/charts/diagrams-png && $(HARNESS) charts-diagrams--cairo
	cd chart-tests/output/charts/diagrams-svg && $(HARNESS) charts-diagrams --svg
	cd chart-tests/output/charts/diagrams-eps && $(HARNESS) charts-diagrams --eps
