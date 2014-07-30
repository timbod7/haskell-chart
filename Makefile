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

clean:
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

upload:
	cd chart && cabal upload
	cd chart-cairo && cabal upload
	cd chart-gtk && cabal upload
	cd chart-diagrams && cabal upload
	cd chart-simple && cabal upload

chart-tests/cabal.sandbox.config:
	cd chart-tests && cabal sandbox init --sandbox ../.cabal-sandbox

tests: chart-tests/cabal.sandbox.config
	cd chart-tests && cabal install --dependencies-only 
	cd chart-tests && cabal install --flags="cairo gtk diagrams"


