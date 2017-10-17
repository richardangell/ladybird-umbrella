library(dplyr)

data <- data.frame(a = runif(100),
                   b = rnorm(100),
                   c = rpois(100, 3),
                   d = rnorm(100),
                   e = runif(100),
                   f = factor(sample(100)),
                   g = as.character(sample(5, size = 100, replace = T)))


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





summarise_column <- function(df,
                             col,
                             observed, 
                             predictions1, 
                             predictions2, 
                             weights) {
  
  col_class <- class(df[[col]])
  
  nominal_classes <- c("character", "factor")
  
  ordered_classes <- c("integer", "numeric")
  
  if (any(col_class %in% nominal_classes)) {
    
    col_type <- "nominal"
    
    summarised_col <- dplyr_summarise(df = df,
                                      col = col,
                                      observed = observed, 
                                      predictions1 = predictions1, 
                                      predictions2 = predictions2, 
                                      weights = weights)
    
  } else if (any(col_class %in% ordered_classes)) {
    
    col_type <- "ordinal"

    df[["binned_ordered"]] <- bin_ordered(df[[col]])
    
    summarised_col <- dplyr_summarise(df = df,
                                      col = "binned_ordered",
                                      observed = observed, 
                                      predictions1 = predictions1, 
                                      predictions2 = predictions2, 
                                      weights = weights)
    
    # remove the bucketed column - don't want to add to the data.frame
    #   input by the user - but as a future enhancement...
    # need to store bucket info and have a way to create buckets from this
    df[["binned_ordered"]] <- NULL
    
  } else {
    
    stop(gettextf("unexpected class (%s) for %s", 
                  sQuote(col_class),
                  sQuote(col)))
    
  }
  
  column_summary <- list()
  
  column_summary$col_class <- col_class 
  
  column_summary$col_type <- col_type
  
  column_summary$summary <- summarised_col
  
  return(column_summary)
  
}
