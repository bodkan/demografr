.PHONY: docs

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/demografr_$(version).tar.gz
logo := man/figures/logo.png

abc_data := inst/examples/abc_data.rds

build: $(pkg)

test:
	R -e 'devtools::test()'

website: README.md $(logo)
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_site()'

docs:
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'
	R -e 'pkgdown::build_news()'

README.md: README.Rmd $(abc_data)
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::build_readme()'

$(pkg): README.md
	R -e 'devtools::document()'
	mkdir -p build; cd build; R CMD build --log ../../demografr

$(logo): logo.R
	R -e 'source("logo.R")'

$(abc_data):
	R -e 'pkgdown::build_article("vignette-01-basics")'

build: $(pkg)

check: $(pkg)
	cd build; R CMD check --as-cran $(notdir $<)

winrel: README.md
	R -e 'devtools::check_win_release()'

windev: README.md
	R -e 'devtools::check_win_devel()'

winold: README.md
	R -e 'devtools::check_win_oldrelease()'

clean:
	rm -rf build
