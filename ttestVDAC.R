setwd("C:/Users/haeri/Desktop/R/HMC/Exp 2/VDAC")
library(Rmisc)
library(tidyverse)
library(BayesFactor)
library(readr)
cleanVDAC <- read_csv("cleanVDAC.csv")

# rt by blocks + reward
block1 <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(1:50) %>%
  na.omit()

block2 <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(51:100) %>%
  na.omit()

block3 <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(101:150) %>%
  na.omit()

block4 <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(151:200) %>%
  na.omit()

#assign new column: block
block1 <- block1 %>%
  mutate(block = 1)
block2 <- block2 %>%
  mutate(block = 2)
block3 <- block3 %>%
  mutate(block = 3)
block4 <-block4 %>%
  mutate(block = 4)

rtrwdVDAC <- rbind(block1, block2, block3, block4)

# create high & low reward type data
high <- filter(rtrwdVDAC, rwdType == 1)
low <- filter(rtrwdVDAC, rwdType == 2)

longData <- bind_rows(high, low)

longData2 <- group_by(longData, subj_idx, rwdType) %>%
  summarise(rt=mean(rt))
a <- filter(longData2, rwdType == 1)
b <- filter(longData2, rwdType == 2)

longData <- group_by(longData, subj_idx, rwdType, block) %>%
  summarise(rt=mean(rt))

test <- longData

test$subj_idx <- as.factor(test$subj_idx)
test$rwdType <- as.factor(test$rwdType)
test$block <- as.factor(test$block)

bf <- anovaBF(
  rt ~ rwdType*block + subj_idx,
  data = data.frame(test),
  whichRandom = "subj_idx")

bf

#interaction bayesfactor
bf[4]/bf[3]

#anova
aov <- aov(rt ~ rwdType * block + Error(subj_idx / (rwdType * block)),
           data = test)
summary(aov)

#pairwise t-test
t.test(data = longData2,
       rt ~ rwdType,
       paired = TRUE)

t.test(a$rt, b$rt,
       paired = TRUE)

#accuracy by trial blocks
block1a <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(1:50) %>%
  na.omit()

block2a <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(51:100) %>%
  na.omit()

block3a <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(101:150) %>%
  na.omit()

block4a <- cleanVDAC %>%
  group_by(subj_idx) %>%
  slice(151:200) %>%
  na.omit()

# create new column = block

block1a <- block1a %>%
  mutate(block = 1)
block2a <- block2a %>%
  mutate(block = 2)
block3a <- block3a %>%
  mutate(block = 3)
block4a <-block4a %>%
  mutate(block = 4)

acrwdVDAC <- rbind(block1a, block2a, block3a, block4a)

# create high & low reward type data
higha <- filter(acrwdVDAC, rwdType == 1)
lowa <- filter(acrwdVDAC, rwdType == 2)

longData_a <- bind_rows(higha, lowa)

longData_a <- group_by(longData_a, subj_idx, rwdType, block) %>%
  summarise(correct=mean(correct))

longData_a2 <- group_by(longData_a, subj_idx, rwdType) %>%
  summarise(correct=mean(correct))

test2 <- longData_a

test2$subj_idx <- as.factor(test$subj_idx)
test2$rwdType <- as.factor(test$rwdType)
test2$block <- as.factor(test$block)

bf2 <- anovaBF(
  correct ~ rwdType*block + subj_idx,
  data = data.frame(test2),
  whichRandom = "subj_idx")

bf2

#interaction bayesfactor
bf2[4]/bf2[3]

#anova
aov2 <- aov(correct ~ rwdType * block + Error(subj_idx / (rwdType * block)),
            data = test2)
summary(aov2)

#pairwise t-test
t.test(data = longData_a2,
       correct ~ rwdType,
       paired = TRUE)