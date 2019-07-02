library(data.table)
library(shiny)
library(shinydashboard)
library(shinyalert)
library(dplyr)
library(rlang)
library(plotly)
library(helpers)
source('./R/app.R')

adult <- read.table(
  'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
  sep = ',', 
  fill = F, 
  strip.white = T
)

colnames(adult) <- c(
  'age', 
  'workclass', 
  'fnlwgt', 
  'educatoin', 
  'educatoin_num', 
  'marital_status', 
  'occupation', 
  'relationship', 
  'race', 
  'sex', 
  'capital_gain', 
  'capital_loss', 
  'hours_per_week', 
  'native_country', 
  'income'
)

adult[['weights']] <- 1
adult[['response']] <- ifelse(adult[['income']] == '<=50K', 1, 0)

ladybird_umbrella()

