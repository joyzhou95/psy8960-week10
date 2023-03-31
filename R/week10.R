# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(67)
# Data Import and Cleaning
gss_tbl_original <- read_sav("../data/GSS2016.sav") %>%
  mutate_all(~na_if(., 0),
             ~na_if(., 98),
             ~na_if(., 99)) %>%
  filter(!is.na(HRS1))

gss_tbl <- select(gss_tbl_original,
                  which(colSums(is.na(gss_tbl_original))/nrow(gss_tbl_original) < 0.75)) %>%
  rename(workhours = HRS1)

# Visualization
gss_tbl %>%
  ggplot(aes(workhours)) +
  geom_histogram() +
  labs(x = "Number of Hourse Worked Last Week", y = "Frequency")

# Analysis
# OLS regression model
shuffle_row <- sample(nrow(gss_tbl))
shuffled_gss <- gss_tbl[shuffle_row,]
split <- round(nrow(shuffled_gss)*0.75)
train <- shuffled_gss[1:split,]
test <- shuffled_gss[(split + 1):nrow(shuffled_gss),]

