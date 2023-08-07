
# 0. Import packages
library(rdrobust)
library(patchwork)

# 0. Import data

realtest::source2('code/data.R') # this does the data prep for us. 

# 1. Clean data

df <- df %>%
  mutate(days_=as.numeric(days))

# 2. Descriptives

df%>%ggplot(aes(x=days_, y=ext_eff, col=factor(treatment)))+geom_jitter()+geom_smooth(col="black")+geom_vline(xintercept=0)

# RD:

## Variables of interest
rdrobust::rdplot(x=df$days_, y=df$polint)
rdrobust::rdplot(x=df$days_, y=df$ext_eff)
rdrobust::rdplot(x=df$days_, y=df$int_eff)
rdrobust::rdplot(x=df$days_, y=df$demsat)

df%>%filter(days_<100&days>-100)%>%ggplot(aes(x=days_, fill=factor(treatment)))+geom_histogram(bins=500)

# OPTIMAL BANDWIDTH


# RECOMMENDATIONS (Valentim, Ruipérez & Dinas 2021)

## Plot the distribution conditional on the distance to the threshold.

polint    <-   rdrobust::rdplot(x=df$days_, y=df$polint)$rdplot+labs(title = "polint")
ext_eff   <-   rdrobust::rdplot(x=df$days_, y=df$ext_eff)$rdplot+labs(title = "ext_eff")
int_eff   <-   rdrobust::rdplot(x=df$days_, y=df$int_eff)$rdplot+labs(title = "int_eff")
demsat    <-   rdrobust::rdplot(x=df$days_, y=df$demsat)$rdplot+labs(title = "demsat")

patchwork::wrap_plots(
  polint,
  ext_eff,
  int_eff,
  demsat)

rm(polint, ext_eff, int_eff, demsat)

## Focus on non-parametric models.

## Show the results using different bandwidths.

### INT_EFF
bw <- seq(3,130,1)
est <- rep(NA,length(bw))
se <- rep(NA,length(bw))
rdd.list <- list()
for(i in 1:length(bw)){
  rdd.list[[i]] <- rdd::RDestimate(int_eff ~ days_,data= df,bw=bw[i])#,cluster= all$stateyear)  
  est[i] <- rdd.list[[i]]$est[1]
  se[i] <- rdd.list[[i]]$se[1]
}

df1 <- data.frame(
  bw=bw,
  est=est,
  ci.hi=est+1.96*se,
  ci.lo=est-1.96*se)

int_eff=ggplot(df1, aes(x = bw, y = est)) +  
  geom_line(aes(x = bw, y = est),color="blue", size=1) + 
  geom_line(aes(x = bw, y = ci.hi ), linetype="dashed") +
  geom_line(aes(x = bw, y = ci.lo ), linetype="dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  xlab("Bandwidth") + theme(axis.title.x =element_text(color = "black", size = 6)) +
  ylab("LATE")   + theme(axis.title.y =element_text(color = "black", size = 6)) +
  ggtitle("Internal efficacy") +
  theme(plot.title = element_text(face="bold", size=6))
int_eff

### EXT_EFF
bw <- seq(3,130,1)
est <- rep(NA,length(bw))
se <- rep(NA,length(bw))
rdd.list <- list()
for(i in 1:length(bw)){
  rdd.list[[i]] <- rdd::RDestimate(ext_eff ~ days_,data= df,bw=bw[i])#,cluster= all$stateyear)  
  est[i] <- rdd.list[[i]]$est[1]
  se[i] <- rdd.list[[i]]$se[1]
}

df1 <- data.frame(
  bw=bw,
  est=est,
  ci.hi=est+1.96*se,
  ci.lo=est-1.96*se)

ext_eff=ggplot(df1, aes(x = bw, y = est)) +  
  geom_line(aes(x = bw, y = est),color="blue", size=1) + 
  geom_line(aes(x = bw, y = ci.hi ), linetype="dashed") +
  geom_line(aes(x = bw, y = ci.lo ), linetype="dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  xlab("Bandwidth") + theme(axis.title.x =element_text(color = "black", size = 6)) +
  ylab("LATE")   + theme(axis.title.y =element_text(color = "black", size = 6)) +
  ggtitle("External efficacy") +
  theme(plot.title = element_text(face="bold", size=6))
ext_eff

### POLINT
bw <- seq(3,130,1)
est <- rep(NA,length(bw))
se <- rep(NA,length(bw))
rdd.list <- list()
for(i in 1:length(bw)){
  rdd.list[[i]] <- rdd::RDestimate(polint ~ days_,data= df,bw=bw[i])#,cluster= all$stateyear)  
  est[i] <- rdd.list[[i]]$est[1]
  se[i] <- rdd.list[[i]]$se[1]
}

df1 <- data.frame(
  bw=bw,
  est=est,
  ci.hi=est+1.96*se,
  ci.lo=est-1.96*se)

polint=ggplot(df1, aes(x = bw, y = est)) +  
  geom_line(aes(x = bw, y = est),color="blue", size=1) + 
  geom_line(aes(x = bw, y = ci.hi ), linetype="dashed") +
  geom_line(aes(x = bw, y = ci.lo ), linetype="dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  xlab("Bandwidth") + theme(axis.title.x =element_text(color = "black", size = 6)) +
  ylab("LATE")   + theme(axis.title.y =element_text(color = "black", size = 6)) +
  ggtitle("Political interest") +
  theme(plot.title = element_text(face="bold", size=6))
polint

### DEMSAT

bw <- seq(3,130,1)
est <- rep(NA,length(bw))
se <- rep(NA,length(bw))
rdd.list <- list()
for(i in 1:length(bw)){
  rdd.list[[i]] <- rdd::RDestimate(demsat ~ days_,data= df,bw=bw[i])#,cluster= all$stateyear)  
  est[i] <- rdd.list[[i]]$est[1]
  se[i] <- rdd.list[[i]]$se[1]
}

df1 <- data.frame(
  bw=bw,
  est=est,
  ci.hi=est+1.96*se,
  ci.lo=est-1.96*se)

demsat=ggplot(df1, aes(x = bw, y = est)) +  
  geom_line(aes(x = bw, y = est),color="blue", size=1) + 
  geom_line(aes(x = bw, y = ci.hi ), linetype="dashed") +
  geom_line(aes(x = bw, y = ci.lo ), linetype="dashed") +
  geom_hline(yintercept = 0, color = "gray") +
  xlab("Bandwidth") + theme(axis.title.x =element_text(color = "black", size = 6)) +
  ylab("LATE")   + theme(axis.title.y =element_text(color = "black", size = 6)) +
  ggtitle("Democratic Satisfaction") +
  theme(plot.title = element_text(face="bold", size=6))
demsat

patchwork::wrap_plots(int_eff, ext_eff, polint, demsat)


##


df_nona <- df%>%
  ungroup()%>%
  drop_na(days_, ext_eff, in_school, education, abitur, female, at_home, migration)%>%
  select(days_, ext_eff, in_school, education, abitur, female, at_home, migration)
rm(ext_eff)
attach(df_nona)

summary(rdrobust::rdrobust(y=ext_eff, x=days_, vce="nn", bwselect="msetwo", covs=cbind(in_school, education, abitur, female, at_home, migration)))
summary(rdrobust::rdrobust(y=int_eff, x=days_, vce="nn", bwselect="msetwo", covs=cbind(in_school, education, abitur, female, at_home, migration)))
summary(rdrobust::rdrobust(y=polint, x=days_, vce="nn", bwselect="msetwo", covs=cbind(in_school, education, abitur, female, at_home, migration)))


## Run manipulation tests.

## Replicate the analyses using placebo outcomes.

female    <-   rdrobust::rdplot(x=df$days_, y=df$female)$rdplot+labs(title = "Female")
in_school <-   rdrobust::rdplot(x=df$days_, y=df$in_school)$rdplot+labs(title = "in_school")
education <-   rdrobust::rdplot(x=df$days_, y=df$education)$rdplot+labs(title = "education")
abitur    <-   rdrobust::rdplot(x=df$days_, y=df$abitur)$rdplot+labs(title = "abitur")
kreisfrei <-   rdrobust::rdplot(x=df$days_, y=df$kreisfrei)$rdplot+labs(title = "kreisfrei")
turnout   <-   rdrobust::rdplot(x=df$days_, y=df$turnout)$rdplot+labs(title = "turnout")

patchwork::wrap_plots(
  female,
  in_school,
  education,
  abitur,
  kreisfrei,
  turnout
)

rm(  female,
     in_school,
     education,
     abitur,
     kreisfrei,
     turnout)

## Replicate the analyses using placebo thresholds.
