
library(data.table)
library(dplyr)
library(rlang)
library(plotly)
source("./R/summarise_variables.R")
source("./R/summarise_column.R")
source("./R/dplyr_summarise.R")
source("./R/bin_ordered.R")
source("./R/plot_bar_line_graphs.R")

#------------------------------------------------------------------------------#
# plot_bar_line_graph ----
#------------------------------------------------------------------------------#

data <- data.frame(a = c(runif(1000), rep(NA, 10)),
                   b = rnorm(1010),
                   c = rpois(1010, 3),
                   d = rnorm(1010),
                   e = runif(1010),
                   f = factor(sample(1010)),
                   g = as.character(sample(5, size = 1010, replace = T)))



xx <- summarise_column(df = data,
                       col = "a",
                       observed = "b",
                       predictions1 = "e", 
                       predictions2 = "d", 
                       weights = "c")



plot_bar_line_graph(df = xx$summary,
                    col = "a",
                    weights = "c")

plot_bar_line_graph(df = xx$summary,
                    col = "a",
                    weights = "c",
                    observed = "b")

plot_bar_line_graph(df = xx$summary,
                    col = "a",
                    weights = "c",
                    predictions1 = "d",
                    observed = "b")

plot_bar_line_graph(df = xx$summary,
                    col = "a",
                    weights = "c",
                    predictions1 = "d",
                    predictions2 = "e",
                    observed = "b")

#------------------------------------------------------------------------------#
# summarise_column ----
#------------------------------------------------------------------------------#


data <- data.frame(a = c(runif(1000), rep(NA, 10)),
                   b = rnorm(1010),
                   c = rpois(1010, 3),
                   d = rnorm(1010),
                   e = runif(1010),
                   f = factor(sample(1010)),
                   g = as.character(sample(5, size = 1010, replace = T)))


summarise_column(df = data,
                 col = "g",
                 observed = NULL,
                 predictions1 = NULL, 
                 predictions2 = NULL, 
                 weights = NULL)

summarise_column(df = data,
                 col = "g",
                 observed = NULL,
                 predictions1 = NULL, 
                 predictions2 = NULL, 
                 weights = "a")

summarise_column(df = data,
                 col = "f",
                 observed = "b",
                 predictions1 = NULL, 
                 predictions2 = NULL, 
                 weights = "a")

summarise_column(df = data,
                 col = "e",
                 observed = NULL,
                 predictions1 = NULL, 
                 predictions2 = NULL, 
                 weights = "c")

xx <- summarise_column(df = data,
                       col = "a",
                       observed = "b",
                       predictions1 = "e", 
                       predictions2 = "d", 
                       weights = "c")

#------------------------------------------------------------------------------#
# dplyr_summarise ----
#------------------------------------------------------------------------------#


df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5), 
  b = sample(5)
)

dplyr_summarise(df = df, 
                col = "g1", 
                observed = "a", 
                predictions1 = NULL, 
                predictions2 = NULL, 
                weights = "b")


#------------------------------------------------------------------------------#
# summarise_variables ----
#------------------------------------------------------------------------------#

data <- data.frame(a = c(runif(1000), rep(NA, 10)),
                   b = rnorm(1010),
                   c = rpois(1010, 3),
                   d = rnorm(1010),
                   e = runif(1010),
                   f = factor(sample(1010)),
                   g = as.character(sample(5, size = 1010, replace = T)))


zz <- summarise_variables(df = data, 
                          cols = c("d", "e", "f", "g"), 
                          observed = "b", 
                          predictions1 = NULL, 
                          predictions2 = NULL, 
                          weight = "c")

plot_bar_line_graph(df = zz$d$summary,
                    col = "d",
                    weights = "c",
                    observed = "b")


