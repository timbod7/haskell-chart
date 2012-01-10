# Use cabal
#CABAL=cabal
#RUNGHC=runghc

# or use cabal-dev
CABAL=cabal-dev -s$(CURDIR)/cabal-dev
RUNGHC=GHC_PACKAGE_PATH=$(CURDIR)/cabal-dev/packages-7.0.3.conf: runghc

install:
	cd chart && $(CABAL) install
	cd chart-gtk && $(CABAL) install

clean:
	cd chart && $(CABAL) clean
	cd chart-gtk && $(CABAL) clean

sdist:
	cd chart && $(CABAL) sdist
	cd chart-gtk && $(CABAL) sdist

tests-basic:
	cd chart/tests && $(RUNGHC) all_tests.hs

test-picking:
	cd chart-gtk/tests && $(RUNGHC) TestPicking.hs

test-darcs-usage:
	$(CABAL) install xml
	cd chart-gtk/tests && $(RUNGHC) darcs_usage.hs

