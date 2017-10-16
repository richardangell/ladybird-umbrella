library(dplyr)
library(rlang)

df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5), 
  b = sample(5)
)

my_summarise <- function(df, group_var) {
  df %>%
    group_by(!!group_var) %>%
    summarise(a = mean(a),
              b = sum(b))
}



my_summarise(df, parse_quosure("g1"))



