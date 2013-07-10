# Use cabal
#CABAL=cabal
#RUNGHC=runghc

# or use cabal-dev
CABAL=cabal-dev -s$(CURDIR)/cabal-dev
RUNGHC=GHC_PACKAGE_PATH=$(CURDIR)/cabal-dev/packages-7.4.2.conf: runghc

install:
	cd chart && $(CABAL) install --force-reinstalls
	cd chart-cairo && $(CABAL) install
	cd chart-gtk && $(CABAL) install

clean:
	cd chart && $(CABAL) clean
	cd chart-cairo && $(CABAL) clean
	cd chart-gtk && $(CABAL) clean
	cd chart-tests && $(CABAL) clean

sdist:
	cd chart && $(CABAL) sdist
	cd chart-cairo && $(CABAL) sdist
	cd chart-gtk && $(CABAL) sdist

tests-basic:
	cd chart-tests/tests && $(RUNGHC) all_tests.hs

test-picking:
	cd chart-tests/tests/gtk && $(RUNGHC) TestPicking.hs

test-cairo-rendering:
	cd chart-tests/tests/drawing && $(RUNGHC) TestApi.hs

test-darcs-usage:
	$(CABAL) install xml
	cd chart-gtk/tests && $(RUNGHC) darcs_usage.hs

