FROM rocker/rstudio:4.5.1

LABEL maintainer="Martin Petr <mp@bodkan.net>"

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -yqq --no-install-recommends \
        build-essential \
        cmake \
        curl \
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
# Fix for 'Errors were encountered while processing: fontconfig' during unminimize
RUN fc-cache -f; yes | unminimize

# location for compiled binaries, renv R libraries, and Python modules
ENV HOME="/root"

# all compiled binaries and scripts used by processing and analysis pipelines
# will be copied to $HOME/bin which will be added to $PATH
ENV BIN="${HOME}/bin/"
RUN mkdir -p ${BIN}

# compile SLiM
RUN cd /tmp; wget https://github.com/MesserLab/SLiM/archive/refs/tags/v5.1.tar.gz -O slim.tar.gz; \
    tar xf slim.tar.gz; cd SLiM-*; mkdir build; cd build; cmake ..; make slim eidos

# install all compiled software into $PATH
RUN cd /tmp; cp SLiM-*/build/slim SLiM-*/build/eidos $BIN

# location for the whole project (scripts, notebooks, and data) inside the container
ENV PROJECT=/project

# setup all required R and Python packages
WORKDIR $PROJECT

# save R packages to home inside the container to avoid cluttering the project directory
ENV RENV_PATHS_LIBRARY_ROOT="${HOME}/renv"
ENV R_INSTALL_STAGED=FALSE

# every R package in the container is locked in a dedicated renv environment, but
# this one package is used system-wide to manage RStudio Server
RUN R -e 'install.packages("rstudioapi")'

# do this when first setting up the container to create an renv.lock file:
#   export RENV_BOOTSTRAP_TARBALL="/tmp/v1.1.5.tar.gz"
#   wget https://github.com/rstudio/renv/archive/refs/tags/v1.1.5.tar.gz -O $RENV_BOOTSTRAP_TARBALL
#   R CMD INSTALL $RENV_BOOTSTRAP_TARBALL
#   export GITHUB_PAT=''
#   renv::init(bare = TRUE, bioconductor = "3.22")
#   options(timeout=600); install.packages("remotes"); remotes::install_deps(dependencies = TRUE)
#   renv::snapshot(dev = TRUE)

# install required R packages to their locked-in versions
COPY DESCRIPTION .
COPY renv.lock .Rprofile ./
COPY renv/ renv/
ARG GITHUB_PAT="''"
RUN R -e 'renv::restore()'

# setup Python environment for slendr
RUN R -e 'slendr::setup_env(agree = TRUE, pip = TRUE)'
# rather than installing a separate Python interpreter, use the slendr one
ENV PATH="${BIN}:${HOME}/.local/share/r-miniconda/envs/Python-3.12_msprime-1.3.4_tskit-0.6.4_pyslim-1.0.4_tspop-0.0.2/bin:${PATH}"
RUN echo "PATH=$PATH" >> ${HOME}/.Renviron

# clone configuration dotfiles into the container
RUN cd ${HOME}; git clone https://github.com/bodkan/dotfiles .dotfiles/; rm -f .bashrc .profile; \
    cd .dotfiles; ./install.sh

# make sure the project is ready when RStudio Server session starts
# https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/session_startup_scripts.html
# https://community.rstudio.com/t/how-to-set-the-default-startup-project-in-rocker-tidyverse/63092/2
RUN echo "\nsetHook('rstudio.sessionInit', \(new) if (new) rstudioapi::openProject('${PROJECT}'))" \
    >> ${HOME}/.Rprofile

# remove compilation sources and other redundant files
RUN rm -r /tmp/* /home/rstudio
