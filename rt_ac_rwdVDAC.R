setwd("C:/Users/haeri/Desktop/R/HMC/Exp 2/VDAC")
library(Rmisc)
library(tidyverse)
library(Cairo)
library(patchwork)
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

longData <- group_by(longData, subj_idx, rwdType, block) %>%
  summarise(rt=median(rt))

# calculate sd, mean, ci
rtrwdVDACsum <- summarySEwithin(longData, measurevar = "rt", withinvars=c("rwdType", "block"), idvar="subj_idx", na.rm=TRUE, conf.interval=.95)

#mutate block column to integer

rtrwdVDACsum$block <- as.integer(rtrwdVDACsum$block)

#create RT by blocks graph
rtblock <- ggplot(rtrwdVDACsum, aes(x=block, y=rt, color = rwdType)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=rt-se, ymax=rt+se)) +
  geom_point(shape=21, size=3, fill="white") +
  labs(x = "Blocks",
       y = "Response Time (s)",
       color = "Reward Type") +
 scale_y_continuous(breaks = seq(0.5, 0.7, 0.05), limits = c(0.5, 0.7)) +
 theme_classic() +
 scale_color_manual(labels = c('High', 'Low'),
                     values = c("red", "forestgreen")) +
 theme(axis.title = element_text(size = 13, color = "black"),
       axis.title.x = element_text(margin = margin(t = 5, r = 00, b = 0, l = 0)),
       axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
       axis.text = element_text(size = 12),
       axis.line = element_line(colour = "black"),
       legend.title = element_text(size = 12, color = "black"),
       legend.position = c(0.75,0.8),
       legend.background = element_rect(fill='transparent'), 
       text = element_text(family = "serif"))

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

# calculate sd, mean, ci
acrwdVDACsum <- summarySEwithin(longData_a, measurevar = "correct", withinvars= c("rwdType", "block"), idvar="subj_idx", na.rm=TRUE, conf.interval=.95)

#mutate block column to integer

acrwdVDACsum$block <- as.integer(acrwdVDACsum$block)

#create Accuracy by blocks graph
acblock <- ggplot(acrwdVDACsum, aes(x=block, y=correct, color = rwdType)) +
  geom_line(linewidth=0.7) +
  geom_errorbar(width=.1, aes(ymin=correct-se, ymax=correct+se)) +
  geom_point(shape=21, size=3, fill="white") +
  labs(x = "Blocks",
       y = "Accuracy",
       color = "Reward Type") +
  scale_y_continuous(breaks = seq(0.8, 1, 0.05), limits = c(0.8, 1)) +
  theme_classic() +
  scale_color_manual(labels = c('High', 'Low'),
                     values = c("red", "forestgreen")) + #black & gray66
  theme(axis.title = element_text(size = 13, color = "black"),
        axis.title.x = element_text(margin = margin(t = 5, r = 00, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.position = c(0.75,0.8),
        legend.background = element_rect(fill='transparent'), 
        text = element_text(family = "serif"))

#merge the plots
p <- rtblock + acblock
p

ggsave(
  "Exp2 rt ac_color.png",
  p,
  width = 6.8,
  height = 4.4,
  type = "cairo"
)
