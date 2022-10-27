web: docs
	R -e 'devtools::install(upgrade = "never")'
	R -e 'pkgdown::build_site()'

docs:
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'
	R -e 'pkgdown::build_news()'

readme:
	R -e 'devtools::build_readme()'
