# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>%
  mutate_all(~na_if(., 0),
             ~na_if(., 98),
             ~na_if(., 99)) %>%
  filter(!is.na(HRS1)) %>%
  select(which(colSums(is.na(gss_tbl))/nrow(gss_tbl) < 0.75))