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
	cabal sandbox add-source chart-simple

unregister: 
	-cabal sandbox hc-pkg unregister Chart-tests
	-cabal sandbox hc-pkg unregister Chart-simple
	-cabal sandbox hc-pkg unregister Chart-diagrams
	-cabal sandbox hc-pkg unregister Chart-gtk
	-cabal sandbox hc-pkg unregister Chart-cairo
	-cabal sandbox hc-pkg unregister Chart

clean: unregister
	cd chart && cabal clean
	cd chart-cairo && cabal clean
	cd chart-gtk && cabal clean
	cd chart-diagrams && cabal clean
	cd chart-simple && cabal clean
	cd chart-tests && cabal clean

clean-sandbox: clean
	rm -rf cabal.sandbox.config .cabal-sandbox
	rm -f chart-tests/cabal.sandbox.config

sdist:
	cd chart && cabal sdist
	cd chart-cairo && cabal sdist
	cd chart-gtk && cabal sdist
	cd chart-diagrams && cabal sdist
	cd chart-simple && cabal sdist

# define environment variable CHART_VERSION
# before making upload
upload:
	cd chart && cabal upload dist/Chart-$(CHART_VERSION).tar.gz
	cd chart-cairo && cabal upload dist/Chart-cairo-$(CHART_VERSION).tar.gz
	cd chart-gtk && cabal upload dist/Chart-gtk-$(CHART_VERSION).tar.gz
	cd chart-diagrams && cabal upload dist/Chart-diagrams-$(CHART_VERSION).tar.gz
	cd chart-simple && cabal upload dist/Chart-simple-$(CHART_VERSION).tar.gz

chart-tests/cabal.sandbox.config:
	cd chart-tests && cabal sandbox init --sandbox ../.cabal-sandbox

tests: chart-tests/cabal.sandbox.config
	cd chart-tests && cabal install --dependencies-only 
	cd chart-tests && cabal install --flags="cairo gtk diagrams"


