
project_loc <- getwd()
pkg_loc <- paste0(project_loc, "/Packages")
install_loc <- paste0(pkg_loc, "/Install")

dir.create(pkg_loc, showWarnings = FALSE)
dir.create(install_loc, showWarnings = FALSE)

assign(".lib.loc", install_loc, envir = environment(.libPaths))

install.packages(paste0(pkg_loc, "/helpers_0.1.0.tar.gz"), lib = install_loc, type = "source", repos = NULL)
install.packages("shiny", lib = install_loc, destdir = pkg_loc)
install.packages("R6", lib = install_loc, destdir = pkg_loc)
install.packages("shinydashboard", lib = install_loc, destdir = pkg_loc)
install.packages("digest", lib = install_loc, destdir = pkg_loc)
install.packages("plotly", lib = install_loc, destdir = pkg_loc)
install.packages("data.table", lib = install_loc, destdir = pkg_loc)
install.packages("dplyr", lib = install_loc, destdir = pkg_loc)
install.packages("shinyalert", lib = install_loc, destdir = pkg_loc)
install.packages("rlang", lib = install_loc, destdir = pkg_loc)
install.packages("ggplot2", lib = install_loc, destdir = pkg_loc)
install.packages("Rcpp", lib = install_loc, destdir = pkg_loc)
install.packages("crayon", lib = install_loc, destdir = pkg_loc)
install.packages("assertthat", lib = install_loc, destdir = pkg_loc)
install.packages("magrittr", lib = install_loc, destdir = pkg_loc)
install.packages("mime", lib = install_loc, destdir = pkg_loc)
install.packages("glue", lib = install_loc, destdir = pkg_loc)
install.packages("purrr", lib = install_loc, destdir = pkg_loc)
install.packages("httr", lib = install_loc, destdir = pkg_loc)
install.packages("yaml", lib = install_loc, destdir = pkg_loc)
install.packages("backports", lib = install_loc, destdir = pkg_loc)
install.packages("cli", lib = install_loc, destdir = pkg_loc)



#install.packages("ggplot2movies", lib = install_loc, destdir = pkg_loc)
#install.packages("leaflet", lib = install_loc, destdir = pkg_loc)

