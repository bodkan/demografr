options(browserNLdisabled = TRUE)

options(install.packages.compile.from.source = "never")

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos=r)
})

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
if (Sys.getenv("IN_CONTAINER") == TRUE) {
  Sys.setenv(RENV_CONFIG_PPM_ENABLED = "TRUE")
  source("renv/activate.R")
} else {
  Sys.setenv(RENV_CONFIG_PPM_ENABLED = "FALSE")
}

# avoid annoyance of having to manually open .Rproj file
# https://rstudio.github.io/rstudioapi/reference/projects.html
# https://community.rstudio.com/t/how-to-set-the-default-startup-project-in-rocker-tidyverse/63092/2
setHook("rstudio.sessionInit", function(newSession) {
  if (newSession && is.null(rstudioapi::getActiveProject()))
    rstudioapi::openProject(".")
}, action = "append")
