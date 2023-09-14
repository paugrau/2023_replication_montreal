# load packages
library(lmtest)
library(ggpubr)
library(tidyverse)
library(sandwich)

# load cleaned data
load("replication/df.RData")

# create subdata
lose <- df %>% 
  filter(wave2 == "TRUE" & wave != 3 & group != 3)

regain <- df %>% 
  filter(wave3 == "TRUE" & wave != 1 & group != 3)

net <- df %>% 
  filter(wave3 == "TRUE" & wave != 2 & group != 3)

## differential attrition between treated and untreated
at <- df %>% 
  filter(wave != 3 & wave!= 2 & group != 3)

at$remain[at$wave2 == TRUE] <- 1
at$remain[at$wave2 == FALSE] <- 0

remain_treated <- at$remain[at$treatment == 1]
remain_untreated <- at$remain[at$treatment == 0]

t_test_result1 <- t.test(remain_treated, remain_untreated)
t_test_result1

## gender differences between dropouts and second wavers
gender_remain <- at$female[at$remain == 1]
gender_dropout <- at$female[at$remain == 0]

t_test_result2 <- t.test(gender_remain, gender_dropout)
t_test_result2

## educational differences between dropouts and second wavers
educ_remain <- at$education[at$remain == 1]
educ_dropout <- at$education[at$remain == 0]

t_test_result3 <- t.test(educ_remain, educ_dropout)
t_test_result3

## regional differences between dropouts and second wavers
region_remain <- at$kreisfrei[at$remain == 1]
region_dropout <- at$kreisfrei[at$remain == 0]

t_test_result4 <- t.test(region_remain, region_dropout)
t_test_result4

## DiD split by high and low education
lose$time <- lose$wave -1

highed <- lose %>% 
  filter(education >2)
lowed <- lose %>% 
  filter(education <3)

highedext <- lm(ext_eff ~ time + treatment + time*treatment, data = highed)
summary(highedext)
higheddem <- lm(demsat ~ time + treatment + time*treatment, data = highed)
summary(higheddem)

lowedext <- lm(ext_eff ~ time + treatment + time*treatment, data = lowed)
summary(lowedext)
loweddem <- lm(demsat ~ time + treatment + time*treatment, data = lowed)
summary(loweddem)



