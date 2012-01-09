# Use cabal
#CABAL=cabal
#RUNGHC=runghc

# or use cabal-dev
CABAL=cabal-dev -s$(CURDIR)/cabal-dev
RUNGHC=GHC_PACKAGE_PATH=$(CURDIR)/cabal-dev/packages-7.0.3.conf: runghc

libs:
	cd chart && $(CABAL) install
	cd chart-gtk && $(CABAL) install

tests-basic:
	cd chart/tests && $(RUNGHC) all_tests.hs

test-picking:
	cd chart-gtk/tests && $(RUNGHC) TestPicking.hs

test-darcs-usage:
	$(CABAL) install xml
	cd chart-gtk/tests && $(RUNGHC) darcs_usage.hs

