.PHONY: docs

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/demografr_$(version).tar.gz
logo := man/figures/logo.png

build: $(pkg)

web: README.md $(logo)
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_site()'

docs:
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'

README.md: README.Rmd
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::build_readme()'

$(pkg): README.md
	R -e 'devtools::document()'
	unset R_HAS_GGTREE; mkdir -p build; cd build; R CMD build --log ../../demografr

$(logo): logo.R
	R -e 'source("logo.R")'

clean:
	rm -rf build
