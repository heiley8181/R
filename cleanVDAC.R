# exp2 vdac
setwd("C:/Users/haeri/Desktop/R/HMC/Exp 2/VDAC")
library(tidyverse)
library(readr)
merged_file <- read_csv("C:/Users/haeri/Desktop/R/HMC/Exp 2/VDAC/merged_file.csv")
View(merged_file)


data <- read_csv("merged_file.csv",
                 col_types = cols(
                   rt = col_double(),
                   tgtPos = col_character(),
                   rwdType= col_character(),
                   tgtPos = col_character(),
                   correct = col_logical()
                 )) %>%
  filter(trialType %in% c("VDACtest")) %>%
  rename(
    subj_idx = subID
  ) %>%
  select(subj_idx, rt, correct, rwdType, trialNum)

data <- 
  mutate(data,
         rt = rt/1000)

data$subj_idx <-
  as.numeric(factor(data$subj_idx, levels = unique(data$subj_idx))) - 1

data <- data %>%
  group_by(subj_idx) %>%
  mutate(trialNum = row_number())

write.csv(data, "cleanVDAC.csv", row.names = FALSE)
