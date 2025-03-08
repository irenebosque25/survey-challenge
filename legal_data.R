
library(haven)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)


trans <-  read_csv("data/trans.csv")

trans <- trans |> 
  row_to_names(row_number = 1)

trans <- trans |> 
  clean_names() |> 
  select(2, 8) |> 
  drop_na(na_2) |> 
  rename("country" = "na_2")

rainbow <- read_csv("data/rainbow.csv", )

rainbow <- rainbow |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  select(2, 3) |> 
  drop_na(na_2) |> 
  rename("country" = "na_2",
         "rain_ind" = "na_3") |> 
  mutate(rain_ind = as.numeric(rain_ind) / 100)
