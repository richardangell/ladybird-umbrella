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


my_summarise2 <- function(df, group_var, summarise_expr) {
  df %>%
    group_by(!!group_var) %>%
    summarise(!!summarise_expr)
}

my_summarise2(df, parse_quosure("g1"), parse_quosure("a = mean(a), b = sum(b)"))

my_summarise2 <- function(df, expr) {
  expr <- enquo(expr)
  
  summarise(df, 
            mean = mean(!!expr),
            sum = sum(!!expr),
            n = n()
  )
}

my_summarise2(df, a)
my_summarise2(df, a * b)




df %>% 
  select(c("g1", "g2", "a")) %>%
  summarise_each(funs(mean))

df

df %>% 
  group_by(g1) %>%
  summarise_at(vars(c("a", "b"), 
                    "a"), 
               list(sum = sum, 
                    count = length)) 


df %>% 
  group_by(g1) %>%
  summarise_at(c("a", "b"), length)

%>%
  summarise_at(c("a"), mean)


obs: sum / M
pred1: sum / M
pred2: sum / M
weight: sum / M
  

summary_fcn


