# 2022-02-16 13:32

install.packages(c("devtools", "sf", "stars", "rnaturalearth"))

devtools::install_github("bodkan/slendr")

slendr::setup_env(pip = TRUE, agree = TRUE)

devtools::install_github("bodkan/demografr")
