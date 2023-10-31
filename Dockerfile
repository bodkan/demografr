FROM rocker/rstudio:4.3.1

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
        vim \
        wget \
        zlib1g-dev; \
    yes | unminimize

# location for compiled binaries, renv R libraries, and Python modules
ENV HOME="/root"

# all compiled binaries and scripts used by processing and analysis pipelines
# will be copied to $HOME/bin which will be added to $PATH
ENV BIN="${HOME}/bin/"
RUN mkdir -p ${BIN}

# location for the whole project (scripts, notebooks, and data) inside the container
ENV PROJECT=/project

# setup all required R and Python packages
WORKDIR $PROJECT

# install R dependencies
ENV R_INSTALL_STAGED=FALSE
ENV RENV_PATHS_LIBRARY_ROOT="${HOME}/renv"
ENV RENV_CONFIG_INSTALL_TRANSACTIONAL=FALSE
ENV RENV_BOOTSTRAP_TARBALL="/tmp/v1.0.3.tar.gz"
RUN wget https://github.com/rstudio/renv/archive/refs/tags/v1.0.3.tar.gz -O $RENV_BOOTSTRAP_TARBALL

# run this when first setting up the container to create an renv.lock file:
#   - R CMD INSTALL $RENV_BOOTSTRAP_TARBALL
#   - renv::init(bare = TRUE, bioconductor = "3.17")
#   - install.packages("remotes"); remotes::install_deps()

# for 'production' builds, restore all R dependencies at their locked-in versions
#COPY DESCRIPTION .
#COPY renv.lock .Rprofile ./
#COPY renv/ renv/
#ARG GITHUB_PAT="''"
#RUN R -e 'renv::restore()'
#
# rather than installing a separate Python interpreter, use the Python environment
# that's installed and used by slendr, in order to avoid version compatibility issues
#RUN R -e 'slendr::setup_env(agree = TRUE, pip = TRUE)'
#ENV PATH="${BIN}:${HOME}/.local/share/r-miniconda/envs/Python-3.11_msprime-1.2.0_tskit-0.5.6_pyslim-1.0.4/bin:${PATH}"

# put personal dotfiles into the container
#ENV IN_DOCKER=true
#RUN git clone https://github.com/bodkan/dotfiles ~/.dotfiles; rm ~/.bashrc ~/.profile; \
#    cd ~/.dotfiles; ./install.sh

# set the default directory of RStudio Server to the project directory
#RUN echo "session-default-working-dir=${PROJECT}" >> /etc/rstudio/rsession.conf

# clean up compilation sources and other redundant files
#RUN rm -r /home/rstudio
