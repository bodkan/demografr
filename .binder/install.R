# 2025-03-21 13:11

install.packages(c("devtools", "sf", "stars", "rnaturalearth"))

install.packages("slendr", repos = "https://cloud.r-project.org")

slendr::setup_env(pip = TRUE, agree = TRUE)

devtools::install_github("bodkan/demografr")
