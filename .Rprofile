options(browserNLdisabled = TRUE)

options(install.packages.compile.from.source = "never")

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos=r)
})

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
if (file.exists("/.dockerenv")) {
  Sys.setenv(RENV_CONFIG_PPM_ENABLED = "TRUE")
  source("renv/activate.R")
} else {
  Sys.setenv(RENV_CONFIG_PPM_ENABLED = "FALSE")
}
