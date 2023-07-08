
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
## Variables of interest
rdrobust::rdplot(x=df$days_, y=df$polint)
rdrobust::rdplot(x=df$days_, y=df$ext_eff)
rdrobust::rdplot(x=df$days_, y=df$int_eff)
rdrobust::rdplot(x=df$days_, y=df$demsat)

# RD:
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

df%>%filter(days_<100&days>-100)%>%ggplot(aes(x=days_, fill=factor(treatment)))+geom_histogram(bins=500)

# OPTIMAL BANDWIDTH

df_nona <- df%>%
  ungroup()%>%
  drop_na(days_, ext_eff, in_school, education, abitur, female, at_home, migration)%>%
  select(days_, ext_eff, in_school, education, abitur, female, at_home, migration)
rm(ext_eff)
attach(df_nona)

summary(rdrobust::rdrobust(y=ext_eff, x=days_, vce="nn", bwselect="msetwo", covs=cbind(in_school, education, abitur, female, at_home, migration)))
summary(rdrobust::rdrobust(y=int_eff, x=days_, vce="nn", bwselect="msetwo", covs=cbind(in_school, education, abitur, female, at_home, migration)))
summary(rdrobust::rdrobust(y=polint, x=days_, vce="nn", bwselect="msetwo", covs=cbind(in_school, education, abitur, female, at_home, migration)))
