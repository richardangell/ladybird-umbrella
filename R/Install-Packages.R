
project_loc <- getwd()

pkg_loc <- paste0(project_loc, "/R_Packages")

install_loc <- paste0(project_loc, "/R_Packages/Install")

assign(".lib.loc", install_loc, envir = environment(.libPaths))

install.packages("shiny", lib = install_loc, destdir = pkg_loc)
install.packages("shinydashboard", lib = install_loc, destdir = pkg_loc)
install.packages("plotly", lib = install_loc, destdir = pkg_loc)
install.packages("ggplot2movies", lib = install_loc, destdir = pkg_loc)
install.packages("ggplot2", lib = install_loc, destdir = pkg_loc)
install.packages("data.table", lib = install_loc, destdir = pkg_loc)
install.packages("leaflet", lib = install_loc, destdir = pkg_loc)
