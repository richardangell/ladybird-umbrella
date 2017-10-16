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
    
    
  } else if (any(col_class %in% ordered_classes)) {
    
    col_type <- "ordinal"
    
    
    
  } else {
    
    stop(gettextf("unexpected class (%s) for %s", 
                  sQuote(col_class),
                  sQuote(col)))
    
  }
  
  
  
  
  
  column_summary <- list()
  
  column_summary$col_class <- col_class 
  
}