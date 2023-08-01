## Run this script before the "Attrition" script

library(ebal)
library(lmtest)
library(ggpubr)
library(tidyverse)
library(sandwich)

load("df.RData")

lose <- df %>% 
  filter(wave2 == "TRUE" & wave != 3 & group != 3)

regain <- df %>% 
  filter(wave3 == "TRUE" & wave != 1 & group != 3)

net <- df %>% 
  filter(wave3 == "TRUE" & wave != 2 & group != 3)

n_distinct(lose$id)
n_distinct(regain$id)
n_distinct(net$id)


### Entropy balanced DiD for losing eligibility
lose <- lose %>% 
  drop_na(female) %>% 
  drop_na(education) %>% 
  drop_na(kreisfrei) %>% 
  drop_na(treatment) %>% 
  drop_na(wave)

covs1 <- lose %>% 
  select(female, education, kreisfrei)
covs1 <- covs1[,-1]

test1 <- ebalance(lose$treatment, covs1)
test1

control1 <- lose %>% 
  filter(treatment == 0)

treatment1 <- lose %>% 
  filter(treatment == 1)

control1$entropyweight <- test1$w

treatment1$entropyweight <- 1

lose <- bind_rows(control1, treatment1)
n_distinct(lose$id)

table(lose$entropyweight)


lose$time <- lose$wave -1


loseext <- lm(ext_eff ~ time + treatment + time*treatment, data = lose, weight = entropyweight)
summary(loseext)
ext1 <- coeftest(loseext, vcov. = vcovHC(loseext, type = "HC1"))
ext1c <- ext1[4,1]
ext1u <- ext1[4,1] + ext1[4,2]*1.96
ext1l <- ext1[4,1] - ext1[4,2]*1.96

loseint <- lm(int_eff ~ time + treatment + time*treatment, data = lose, weight = entropyweight)
summary(loseint)
int1 <- coeftest(loseint, vcov. = vcovHC(loseint, type = "HC1"))
int1c <- int1[4,1]
int1u <- int1[4,1] + int1[4,2]*1.96
int1l <- int1[4,1] - int1[4,2]*1.96

losedem <- lm(demsat ~ time + treatment + time*treatment, data = lose, weight = entropyweight)
summary(losedem)
dem1 <- coeftest(losedem, vcov. = vcovHC(losedem, type = "HC1"))
dem1c <- dem1[4,1]
dem1u <- dem1[4,1] + dem1[4,2]*1.96
dem1l <- dem1[4,1] - dem1[4,2]*1.96

losepol <- lm(polint ~ time + treatment + time*treatment, data = lose, weight = entropyweight)
summary(losepol)
pol1 <- coeftest(losepol, vcov. = vcovHC(losepol, type = "HC1"))
pol1c <- pol1[4,1]
pol1u <- pol1[4,1] + pol1[4,2]*1.96
pol1l <- pol1[4,1] - pol1[4,2]*1.96

coef1 <- c(ext1c,int1c,dem1c,pol1c)
coef1 <- as.data.frame(coef1)
upper1 <- c(ext1u,int1u,dem1u,pol1u)
upper1 <- as.data.frame(upper1)
lower1 <- c(ext1l,int1l,dem1l,pol1l)
lower1 <- as.data.frame(lower1)
Outcome <- c("Ext Eff", "Int Eff", 
             "Dem Sat", "Pol Int")
Outcome <- as.data.frame(Outcome)

df1 <- bind_cols(coef1,upper1,lower1,Outcome)

eb1 <- ggplot(df1, aes(x = coef1, y = Outcome)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_point(size = 5) +
  geom_errorbar(aes(xmin = lower1, xmax = upper1), 
                size = 2, width = 0.4) +
  theme_light(base_size = 40) +
  xlab("") +
  ylab("") + 
  scale_x_continuous(breaks=c(-0.2, 0, 0.2)) +
  ggtitle("Losing eligibility") +
  theme(plot.title = element_text(size=30))
eb1


### Entropy balanced DiD for regaining eligibility
regain <- regain %>% 
  drop_na(female) %>% 
  drop_na(education) %>% 
  drop_na(kreisfrei) %>% 
  drop_na(treatment) %>% 
  drop_na(wave)

covs2 <- regain %>% 
  select(female, education, kreisfrei)
covs2 <- covs2[,-1]

test2 <- ebalance(regain$treatment, covs2)
test2

control2 <- regain %>% 
  filter(treatment == 0)

treatment2 <- regain %>% 
  filter(treatment == 1)

control2$entropyweight <- test2$w

treatment2$entropyweight <- 1

regain <- bind_rows(control2, treatment2)
n_distinct(regain$id)

table(regain$entropyweight)


regain$time <- regain$wave -2


regainext <- lm(ext_eff ~ time + treatment + time*treatment, data = regain, weight = entropyweight)
summary(regainext)
ext2 <- coeftest(regainext, vcov. = vcovHC(regainext, type = "HC1"))
ext2c <- ext2[4,1]
ext2u <- ext2[4,1] + ext2[4,2]*1.96
ext2l <- ext2[4,1] - ext2[4,2]*1.96

regainint <- lm(int_eff ~ time + treatment + time*treatment, data = regain, weight = entropyweight)
summary(regainint)
int2 <- coeftest(regainint, vcov. = vcovHC(regainint, type = "HC1"))
int2c <- int2[4,1]
int2u <- int2[4,1] + int2[4,2]*1.96
int2l <- int2[4,1] - int2[4,2]*1.96

regaindem <- lm(demsat ~ time + treatment + time*treatment, data = regain, weight = entropyweight)
summary(regaindem)
dem2 <- coeftest(regaindem, vcov. = vcovHC(regaindem, type = "HC1"))
dem2c <- dem2[4,1]
dem2u <- dem2[4,1] + dem2[4,2]*1.96
dem2l <- dem2[4,1] - dem2[4,2]*1.96

regainpol <- lm(polint ~ time + treatment + time*treatment, data = regain, weight = entropyweight)
summary(regainpol)
pol2 <- coeftest(regainpol, vcov. = vcovHC(regainpol, type = "HC1"))
pol2c <- pol2[4,1]
pol2u <- pol2[4,1] + pol2[4,2]*1.96
pol2l <- pol2[4,1] - pol2[4,2]*1.96

coef2 <- c(ext2c,int2c,dem2c,pol2c)
coef2 <- as.data.frame(coef2)
upper2 <- c(ext2u,int2u,dem2u,pol2u)
upper2 <- as.data.frame(upper2)
lower2 <- c(ext2l,int2l,dem2l,pol2l)
lower2 <- as.data.frame(lower2)

df2 <- bind_cols(coef2,upper2,lower2,Outcome)

eb2 <- ggplot(df2, aes(x = coef2, y = Outcome)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_point(size = 5) +
  geom_errorbar(aes(xmin = lower2, xmax = upper2), 
                size = 2, width = 0.4) +
  theme_light(base_size = 40) +
  xlab("DiD Coefficient") +
  ylab("") +
  scale_x_continuous(breaks=c(-0.2, 0, 0.2)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=30)
  ) +
  ggtitle("Regaining eligibility")
eb2

### Entropy balanced DiD for net effect of temp. disenfr.
net <- net %>% 
  drop_na(female) %>% 
  drop_na(education) %>% 
  drop_na(kreisfrei) %>% 
  drop_na(treatment) %>% 
  drop_na(wave)

covs3 <- net %>% 
  select(female, education, kreisfrei)
covs3 <- covs3[,-1]

test3 <- ebalance(net$treatment, covs3)
test3

control3 <- net %>% 
  filter(treatment == 0)

treatment3 <- net %>% 
  filter(treatment == 1)

control3$entropyweight <- test3$w

treatment3$entropyweight <- 1

net <- bind_rows(control3, treatment3)
n_distinct(net$id)

table(net$entropyweight)


net$time <- net$wave -1
net$time[net$time == 2] <- 1

netext <- lm(ext_eff ~ time + treatment + time*treatment, data = net, weight = entropyweight)
summary(netext)
ext3 <- coeftest(netext, vcov. = vcovHC(netext, type = "HC1"))
ext3c <- ext3[4,1]
ext3u <- ext3[4,1] + ext3[4,2]*1.96
ext3l <- ext3[4,1] - ext3[4,2]*1.96

netint <- lm(int_eff ~ time + treatment + time*treatment, data = net, weight = entropyweight)
summary(netint)
int3 <- coeftest(netint, vcov. = vcovHC(netint, type = "HC1"))
int3c <- int3[4,1]
int3u <- int3[4,1] + int3[4,2]*1.96
int3l <- int3[4,1] - int3[4,2]*1.96

netdem <- lm(demsat ~ time + treatment + time*treatment, data = net, weight = entropyweight)
summary(netdem)
dem3 <- coeftest(netdem, vcov. = vcovHC(netdem, type = "HC1"))
dem3c <- dem3[4,1]
dem3u <- dem3[4,1] + dem3[4,2]*1.96
dem3l <- dem3[4,1] - dem3[4,2]*1.96

netpol <- lm(polint ~ time + treatment + time*treatment, data = net, weight = entropyweight)
summary(netpol)
pol3 <- coeftest(netpol, vcov. = vcovHC(netpol, type = "HC1"))
pol3c <- pol3[4,1]
pol3u <- pol3[4,1] + pol3[4,2]*1.96
pol3l <- pol3[4,1] - pol3[4,2]*1.96

coef3 <- c(ext3c,int3c,dem3c,pol3c)
coef3 <- as.data.frame(coef3)
upper3 <- c(ext3u,int3u,dem3u,pol3u)
upper3 <- as.data.frame(upper3)
lower3 <- c(ext3l,int3l,dem3l,pol3l)
lower3 <- as.data.frame(lower3)

df3 <- bind_cols(coef3,upper3,lower3,Outcome)

eb3 <- ggplot(df3, aes(x = coef3, y = Outcome)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
  geom_point(size = 5) +
  geom_errorbar(aes(xmin = lower3, xmax = upper3), 
                size = 2, width = 0.4) +
  theme_light(base_size = 40) +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks=c(-0.2, 0, 0.2)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=30)
  ) +
  ggtitle("Net effect")
eb3

ggarrange(eb1,eb2,eb3, nrow = 1,align = "hv")

