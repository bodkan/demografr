FROM rocker/rstudio:4.4.2

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
RUN wget https://github.com/MesserLab/SLiM/archive/refs/tags/v4.3.tar.gz -O slim.tar.gz; \
    tar xf slim.tar.gz; cd SLiM-*; mkdir build; cd build; cmake ..; make slim eidos

# install all compiled software into $PATH
RUN cp SLiM-*/build/slim SLiM-*/build/eidos $BIN

# location for the whole project (scripts, notebooks, and data) inside the container
ENV PROJECT=/project

# setup all required R and Python packages
WORKDIR $PROJECT

# put personal dotfiles into the container
RUN git clone https://github.com/bodkan/dotfiles ~/.dotfiles; rm ~/.bashrc ~/.profile; \
    cd ~/.dotfiles; ./install.sh

# save R packages to home inside the container to avoid cluttering the project directory
ENV RENV_PATHS_LIBRARY_ROOT="${HOME}/renv"
ENV R_INSTALL_STAGED=FALSE

# do this when first setting up the container to create an renv.lock file:
#   - export RENV_BOOTSTRAP_TARBALL="/tmp/v1.1.0.tar.gz"
#   - wget https://github.com/rstudio/renv/archive/refs/tags/v1.1.0.tar.gz -O $RENV_BOOTSTRAP_TARBALL
#   - R CMD INSTALL $RENV_BOOTSTRAP_TARBALL
#   - export GITHUB_PAT=''
#   - renv::init(bare = TRUE, bioconductor = "3.29")
#   - options(timeout=600); install.packages("remotes"); remotes::install_deps(dependencies = TRUE)
#   - renv::snapshot(dev = TRUE)
