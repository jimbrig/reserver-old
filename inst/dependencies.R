# No Remotes ----
# Attachments ----
to_install <- c("config", "dplyr", "lubridate", "randomNames", "shiny", "tibble")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }
