# Use cabal
#CABAL=cabal
#RUNGHC=runghc
#GHCPKG=ghc-pkg

# or use cabal-dev
CABAL=cabal-dev -s$(CURDIR)/cabal-dev
RUNGHC=GHC_PACKAGE_PATH=$(CURDIR)/cabal-dev/packages-7.6.3.conf: runghc
GHCPKG=GHC_PACKAGE_PATH=$(CURDIR)/cabal-dev/packages-7.6.3.conf: ghc-pkg

install: unregister
	cd chart && $(CABAL) install --force-reinstalls
	cd chart-cairo && $(CABAL) install --force-reinstalls 
	cd chart-gtk && $(CABAL) install --force-reinstalls 
	cd chart-diagrams && $(CABAL) install --force-reinstalls 

unregister:
	-$(GHCPKG) unregister Chart-diagrams
	-$(GHCPKG) unregister Chart-gtk
	-$(GHCPKG) unregister Chart-cairo
	-$(GHCPKG) unregister Chart

clean:
	cd chart && $(CABAL) clean
	cd chart-cairo && $(CABAL) clean
	cd chart-gtk && $(CABAL) clean
	cd chart-tests && $(CABAL) clean

sdist:
	cd chart && $(CABAL) sdist
	cd chart-cairo && $(CABAL) sdist
	cd chart-gtk && $(CABAL) sdist

tests:
	cd chart-tests && $(CABAL) install --flags="cairo gtk diagrams"

