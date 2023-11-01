source("renv/activate.R")

options(browserNLdisabled = TRUE)

# avoid annoyance of having to manually open .Rproj file
# https://rstudio.github.io/rstudioapi/reference/projects.html
# https://community.rstudio.com/t/how-to-set-the-default-startup-project-in-rocker-tidyverse/63092/2
setHook("rstudio.sessionInit", function(newSession) {
  if (newSession && is.null(rstudioapi::getActiveProject()))
    rstudioapi::openProject(".")
}, action = "append")
