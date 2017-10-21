
summarise_variables(df = df, observed = "c")

summarise_variables <- function(df, 
                                cols = NULL, 
                                observed = NULL, 
                                predictions1 = NULL, 
                                predictions2 = NULL, 
                                weight = NULL) {
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
  if (!is.data.frame(df)) {
    
    stop("df should be a data.frame")
    
  } else {
    
    if (!nrow(df) > 0) {
      
      stop("df has 0 rows")
      
    }
    
  }
  
  if (all(sapply(c(observed, 
                   predictions1, 
                   predictions2, 
                   weight), 
                 is.null))) {
    
    stop("no summary variables (observed, predictions1, predictions2, weight) have been specified")
    
  } else {
    
    summary_variables <- c(observed, 
                           predictions1, 
                           predictions2, 
                           weight)
    
  }

  if (!is.null(observed)) {
    
    if (!observed %in% colnames(df)) {
      
      stop(gettextf("observed (%s) is not in df", 
                    sQuote(observed)))
      
    }
    
  }
  
  if (!is.null(predictions1)) {
    
    if (!predictions1 %in% colnames(df)) {
      
      stop(gettextf("predictions1 (%s) is not in df", 
                    sQuote(predictions1)))
      
    }
    
  }
  
  if (!is.null(predictions2)) {
    
    if (!predictions2 %in% colnames(df)) {
      
      stop(gettextf("predictions2 (%s) is not in df", 
                    sQuote(predictions2)))
      
    }
    
  }
  
  if (!is.null(weight)) {
    
    if (!weight %in% colnames(df)) {
      
      stop(gettextf("weight (%s) is not in df", 
                    sQuote(weight)))
      
    }
    
    if (!any(df[[weight]] > 0)) {
      
      stop("no weights > 0")
      
    }
    
  } else {
    
    df[["weights_column_temp_1234"]] <- rep(1, nrow(df))
    
    weight <- "weights_column_temp_1234"
    
  }
  
  if (!is.null(cols)) {
    
    cols <- setdiff(colnames(df), summary_variables)
    
  } else {
    
    if (any(!cols %in% colnames(df))) {
      
      stop(gettextf("the following variables specified in cols (%s) are missing from df", 
                    paste(sQuote(setdiff(cols, 
                                         colnames(df))),
                          collapse = ",")))
      
    }
    
    if (any(summary_variables %in% cols)) {
      
      warning(gettextf("the following summary variables (%s) are in cols so have been removed", 
                       paste(sQuote(intersect(summary_variables, 
                                              cols)),
                             collapse = ",")))
      
      cols <- setdiff(cols, summary_variables)
      
    }
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Summarise variables ----
  #----------------------------------------------------------------------------#
  
  variable_summary <- list()
  
  variable_summary$df_metadata <- list(nrow = nrow(df),
                                       ncol = ncol(df),
                                       colnames = colnames(df),
                                       observed = observed, 
                                       predictions1 = predictions1, 
                                       predictions2 = predictions2, 
                                       weight = weight_colname) 
  
  # convert to data.table
  # could not do data.table conversion  but instead use dplyr...
  # data.table is a smaller dependency for the project
  # dplyr now has nicer ability to use it's functions programatically 
  setDT(df)
  
  for (col in cols) {
    
    summarised_variables[[col]] <- summarise_variable(df = df,
                                                      col = col,
                                                      observed = observed, 
                                                      predictions1 = predictions1, 
                                                      predictions2 = predictions2, 
                                                      weights = weight)  
    
  }
  
  # coerce df back to data.frame class
  setDF(df)
  
  # remove weights column from df if one was added
  # is this required?
  # will adding the weights column to the data.frame within the function
  #   create a copy of df? if so this should be avoided
  if (weight == "weights_column_temp_1234") {
    
    df$weights_column_temp_1234 <- NULL
    
  }
  
  return(summarised_variables)
  
  
  
}

