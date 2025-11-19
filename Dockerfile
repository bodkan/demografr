FROM rocker/rstudio:4.5.2

LABEL maintainer="Martin Petr <mp@bodkan.net>"

ENV SLIM_VERSION="v5.1" \
    RENV_VERSION="v1.1.5" \
    BIOCONDUCTOR_VERSION="3.22" \
    PYTHON_ENV="Python-3.13_msprime-1.3.4_tskit-0.6.4_pyslim-1.1.0_tspop-0.0.2"

############################################################
# setup the base system
############################################################

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -yqq --no-install-recommends \
        build-essential \
        cmake \
        curl \
        gdal-bin \
        git \
        htop \
        iputils-ping \
        less \
        libblas-dev \
        libbz2-dev \
        libcurl4-openssl-dev \
        libffi-dev \
        libfftw3-dev \
        libfontconfig1-dev \
        libfribidi-dev \
        libgdal-dev \
        libgit2-dev \
        libglpk-dev \
        libglu1 \
        libgsl-dev \
        liblapack-dev \
        libharfbuzz-dev \
        libmagick++-dev \
        libnlopt-cxx-dev \
        libpopt-dev \
        libreadline-dev \
        librsvg2-dev \
        libsnappy1v5 \
        libssl-dev \
        libsqlite3-dev \
        libtiff5-dev \
        libudunits2-dev \
        libunwind-dev \
        libwebp-dev \
        libxml2-dev \
        libxt6 \
        libzmq5 \
        libzstd-dev \
        man-db \
        parallel \
        rename \
        tmux \
        tree \
        unminimize \
        vim \
        wget \
        zlib1g-dev

# fix 'Errors were encountered while processing: fontconfig' during unminimize
RUN fc-cache -f; yes | unminimize

# the container is intended as a dedicated environment to be run in a rootless setting
ENV HOME="/root"

############################################################
# compile and install third-party software dependencies
############################################################

# all compiled binaries and scripts used by the project will be in this location
ENV BIN="${HOME}/bin/"
RUN mkdir -p ${BIN}

# compile SLiM
RUN cd /tmp; wget https://github.com/MesserLab/SLiM/archive/refs/tags/${SLIM_VERSION}.tar.gz -O slim.tar.gz; \
    tar xf slim.tar.gz; cd SLiM-*; mkdir build; cd build; cmake ..; make slim eidos

# install all compiled software into $PATH
RUN cd /tmp; cp SLiM-*/build/slim SLiM-*/build/eidos $BIN

############################################################
# install R packages required by the project
############################################################

# location for the whole project (scripts, notebooks, and data) inside the container
ENV PROJECT=/project
WORKDIR $PROJECT

# save R packages under home in the container to avoid cluttering the project directory
# (because the project itself will be mounted from the host system)
ENV RENV_PATHS_LIBRARY_ROOT="${HOME}/renv"
# fix an obscure error during R package installations:
# https://github.com/rstudio/renv/issues/239#issuecomment-553595005
ENV R_INSTALL_STAGED=FALSE

ENV RENV_BOOTSTRAP_TARBALL="/tmp/${RENV_VERSION}.tar.gz"
RUN wget https://github.com/rstudio/renv/archive/refs/tags/${RENV_VERSION}.tar.gz -O $RENV_BOOTSTRAP_TARBALL

# every R package in the container is locked in a dedicated renv environment, but
# this one package is used system-wide to manage RStudio Server
RUN R -e 'install.packages("rstudioapi")'

# do this when first setting up the container to create an renv.lock file:
#   R CMD INSTALL $RENV_BOOTSTRAP_TARBALL
#   export GITHUB_PAT='<PLACE GITHUB TOKEN HERE IF NEEDED>'
#   renv::init(bare = TRUE, bioconductor = Sys.getenv("BIOCONDUCTOR_VERSION"))
#   install.packages("yaml") # to be able to discover dependencies in Rmd files
#   renv::snapshot(dev = TRUE)

## install required R packages to their locked-in versions
#COPY DESCRIPTION .
#COPY renv.lock .Rprofile ./
#COPY renv/ renv/
#ARG GITHUB_PAT="''"
#RUN R -e 'renv::restore()'
#
## setup Python environment for slendr
#RUN R -e 'slendr::setup_env(agree = TRUE, pip = TRUE)'
## rather than installing a separate Python interpreter, use the slendr one
#ENV PATH="${BIN}:${HOME}/.local/share/r-miniconda/envs/${PYTHON_ENV}/bin:${PATH}"
#
## make sure all software is available in R
#RUN echo "PATH=$PATH" >> ${HOME}/.Renviron
#
#############################################################
## final configuration steps
#############################################################
#
## clone shell configuration files into the container
#RUN cd ${HOME}; git clone https://github.com/bodkan/dotfiles .dotfiles/; rm -f .bashrc .profile; \
#    cd .dotfiles; ./install.sh
#
## make sure the project is ready when RStudio Server session starts
## https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/session_startup_scripts.html
## https://community.rstudio.com/t/how-to-set-the-default-startup-project-in-rocker-tidyverse/63092/2
#RUN echo "\nsetHook('rstudio.sessionInit', \(new) if (new) rstudioapi::openProject('${PROJECT}'))" >> ${HOME}/.Rprofile
#
## remove compilation sources and other redundant files
#RUN rm -r /tmp/* /home/rstudio
