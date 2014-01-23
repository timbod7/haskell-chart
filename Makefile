# Use cabal with sandboxes
CABAL=cabal

.cabal-sandbox:
	$(CABAL) sandbox init

install: .cabal-sandbox unregister
	$(CABAL) install chart/ chart-cairo/ chart-gtk/ chart-diagrams/ chart-simple/

unregister:
	-$(CABAL) sandbox hc-pkg unregister Chart-simple
	-$(CABAL) sandbox hc-pkg unregister Chart-diagrams
	-$(CABAL) sandbox hc-pkg unregister Chart-gtk
	-$(CABAL) sandbox hc-pkg unregister Chart-cairo
	-$(CABAL) sandbox hc-pkg unregister Chart

clean: unregister
	cd chart && $(CABAL) clean
	cd chart-cairo && $(CABAL) clean
	cd chart-gtk && $(CABAL) clean
	cd chart-diagrams && $(CABAL) clean
	cd chart-simple && $(CABAL) clean
	cd chart-tests && $(CABAL) clean

sdist:
	cd chart && $(CABAL) sdist
	cd chart-cairo && $(CABAL) sdist
	cd chart-gtk && $(CABAL) sdist
	cd chart-diagrams && $(CABAL) sdist
	cd chart-simple && $(CABAL) sdist

tests:
	$(CABAL) install chart-tests/ --flags="cairo gtk diagrams"

