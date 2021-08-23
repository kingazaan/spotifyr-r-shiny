my_packages <- c("shiny","shinythemes","spotifyr", "tidyverse", "DT", "reshape2", "plotly", "ggridges", "httr", "remotes")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))