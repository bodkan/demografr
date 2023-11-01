.PHONY: docs

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/demografr_$(version).tar.gz
logo := man/figures/logo.png

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

README.md: README.Rmd
	R -e 'devtools::install(upgrade = "never")'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'
	# restore back useless updates to the non-random figures made by pkgdown
	git checkout \
		man/figures/README-diagnostic_Ne-1.png \
		man/figures/README-prior_Ne-1.png \
		man/figures/README-prior_T_gf-1.png

$(pkg): README.md
	R -e 'devtools::document()'
	mkdir -p build; cd build; R CMD build --log ../../demografr

$(logo): logo.R
	R -e 'source("logo.R")'

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

# check the OS type to assign appropriate Docker image tag
ifeq ($(shell uname -s), Darwin)
    PLATFORM ?= arm64
else
    PLATFORM ?= amd64
endif

IMAGE := bodkan/demografr:$(PLATFORM)
CONTAINER := demografr_$(shell date '+%Y-%m-%d_%H-%M-%S')

# if present, extract GitHub access token
TOKEN := $(shell awk -F= '/GITHUB_PAT/{print $$2}' ~/.Renviron)

PORT ?= 9999

.PHONY: help rstudio bash R r

rstudio:
	docker run --rm -ti -p $(PORT):8787 -e RUNROOTLESS=true -e DISABLE_AUTH=true -v $(shell pwd):/project --name $(CONTAINER) $(IMAGE)

bash:
	docker run --rm -ti -v $(shell pwd):/project -w /project --name $(CONTAINER) $(IMAGE) bash

R r:
	docker run --rm -ti -v $(shell pwd):/project -w /project --name $(CONTAINER) $(IMAGE) R

docker-build:
	docker build --build-arg GITHUB_PAT=$(TOKEN) -t $(IMAGE) .

docker-build-clean:
	@echo $(IMAGE)
	docker build --no-cache --build-arg GITHUB_PAT=$(TOKEN) -t $(IMAGE) .

docker-push:
	docker push $(IMAGE)

docker-pull:
	docker pull $(IMAGE)

local-webapp:
	GOOGLE_API_KEY="" chromium --app=http://localhost:$(PORT) &

remote-webapp:
ifndef SERVER
	$(error SERVER variable must be set to start a web app)
endif
	@if [[ "$(SERVER)" != "localhost" ]]; then \
	    PID=$$(lsof -ti:$(PORT)); \
	    if [[ -n "$$PID" ]]; then \
		kill -9 $$PID; \
	    fi; \
	    autossh -M 0 -f -N -L localhost:$(PORT):localhost:$(PORT) $(SERVER) || { \
		echo "SSH connection failed. Exiting."; \
		exit 1; \
	    }; \
	fi; \
	GOOGLE_API_KEY="" chromium --app=http://localhost:$(PORT) &
