##################################################################
# R package infrastructure
#

.PHONY: website docs build check windev winrel winold clean

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/demografr_$(version).tar.gz
logo := man/figures/logo.png

docs:
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'
	R -e 'pkgdown::build_news()'

website: $(logo) README.md
	rename 's/Rmd$$/Rmd_/' vignettes/vignette-{07,08,09,10}-*.Rmd
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'
	R -e 'pkgdown::build_news()'
	R -e 'pkgdown::build_site()'
	rename 's/Rmd_$$/Rmd/' vignettes/vignette-{07,08,09,10}-*.Rmd_

test:
	R -e 'devtools::test()'


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
	if [ -f vignettes/*.Rmd_ ]; then rename 's/Rmd_$/Rmd/' vignettes/vignette-{07,08,09,10}-*.Rmd_; fi


$(pkg): README.md
	@if [ "$$IN_CONTAINER" = "TRUE" ]; then \
		echo "Attempting to build inside a container"; \
		exit 1; \
	fi
	R -e 'devtools::document()'
	mkdir -p build; cd build; R CMD build --log ../../demografr

README.md: README.Rmd $(logo)
	R -e 'devtools::install(upgrade = "never")'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'
	# restore back useless updates to the non-random figures made by pkgdown
	#git checkout \
	# 	man/figures/README-diagnostic_Ne-1.png \
	# 	man/figures/README-prior_Ne-1.png \
	# 	man/figures/README-prior_T_gf-1.png

$(logo): logo.R
	R -e 'source("logo.R")'

##################################################################
# Docker shortcuts
#

# check the OS type to assign appropriate Docker image tag
ifeq ($(shell uname -s), Darwin)
    PLATFORM ?= arm64
else
    PLATFORM ?= amd64
endif

IMAGE := bodkan/$(shell basename $(shell pwd)):$(PLATFORM)
CONTAINER := $(shell basename $(shell pwd))_$(shell date '+%Y-%m-%d_%H-%M-%S')

# if present, extract GitHub access token
TOKEN := $(shell awk -F= '/GITHUB_PAT/{print $$2}' ~/.Renviron)

.PHONY: rstudio bash R r docker-build docker-push docker-pull local-webapp remote-webapp

bash:
	docker run --rm -ti -v $(shell pwd):/project -w /project --name $(CONTAINER) $(IMAGE) bash

R:
	docker run --rm -ti -v $(shell pwd):/project -w /project --name $(CONTAINER) $(IMAGE) R

attach:
	container_id=`docker ps | awk -v name="$$(basename "$$PWD")" '$$2 ~ name {print $$1}'`; \
	docker exec -it $$container_id /bin/bash

rstudio:
ifndef PORT
	$(error PORT variable must be set explicitly)
endif
	docker run --rm -ti -p $(PORT):8787 -e RUNROOTLESS=true -e DISABLE_AUTH=true -v $(shell pwd):/project --name $(CONTAINER) $(IMAGE)

docker-build:
	docker build --build-arg GITHUB_PAT=$(TOKEN) -t $(IMAGE) .

docker-clean-build:
	@echo $(IMAGE)
	docker build --no-cache --build-arg GITHUB_PAT=$(TOKEN) -t $(IMAGE) .

docker-push:
	docker push $(IMAGE)

docker-pull:
	docker pull $(IMAGE)

local-webapp:
ifndef PORT
	$(error PORT variable must be set explicitly)
endif
	GOOGLE_API_KEY="" chromium --app=http://localhost:$(PORT) &

remote-webapp:
ifndef SERVER
	$(error SERVER variable must be set to start a web app)
endif
ifndef PORT
	$(error PORT variable must be set explicitly)
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

port-forward:
ifndef SERVER
	$(error SERVER variable must be set to start a web app)
endif
ifndef PORT
	$(error PORT variable must be set explicitly)
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
	fi
