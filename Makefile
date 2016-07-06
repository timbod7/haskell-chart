# Delegate everything to stack

build:
	stack build

clean:
	stack clean

test:
	stack test
	stack exec chart-harness -- charts-cairo --png
	stack exec chart-harness -- charts-diagrams --svg

sdist:
	stack sdist

upload:
	stack upload chart --no-signature
	stack upload chart-cairo --no-signature
	stack upload chart-gtk --no-signature
	stack upload chart-diagrams --no-signature
