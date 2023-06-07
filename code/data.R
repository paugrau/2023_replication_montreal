################################################################################
## DATA
## Authors: MLS, AL
## Created: 15 October 2020
## Last update: 28 October 2020
## Purpose: Paper Temp. Disenfranchisement
################################################################################

## Load (, if necessary install packages,) and set working directory ------------

## clear working memory
rm(list = ls())

## install packages if not installed
p_needed <-
  c(
    "haven",
    "dplyr",
    "data.table",
    "ggplot2",
    "tidyverse",
    "MatchIt",
    "dotwhisker",
    "broom",
    "rddtools",
    "cobalt",
    "stargazer",
    "rdrobust",
    "ggpubr",
    "texreg",
    "extrafont",
    "viridis",
    "kableExtra"
  )

# Compare needed and installed packages
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

# If a package is not installed yet, install it
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

# Load all the packages in the environment
sapply(p_needed, require, character.only = TRUE)

# # Load and if necessary install packages in development
# # THis is for balance_plot (not used at the moment)
# if(!('DAPSm' %in% packages)) remotes::install_github('gpapadog/DAPSm')
# library(DAPSm)

# Load fonts
loadfonts()

# Set WD to current file path (in our case the Dropbox ;) )
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd('..')

# remove objects, which are no longer needed
rm(p_needed, p_to_install, packages)

## Main Data -------------------------------------------------------------------

data <- read_dta("data/shpanel_20220130_v12.dta")

## create variables for participation in given election, to be repeated in all waves

df <-
  data %>% select(
    uid,
    welle,
    gruppe,
    geschlecht,
    alter,
    schulform,
    schulabschluss1,
    schulabschluss2,
    wahlteilnahme,
    polint1,
    demzufriedenheit,
    wahrnehmungpol3,
    wahrnehmungpol4,
    migrationshintergrund,
    gebdatum,
    ltwbeteiligung,
    btwbeteiligung,
    kreisfrei,
    wohnen
  ) %>% mutate(
    id = uid,
    group = gruppe,
    wave = welle,
    # posttreatment = wave > 1,
    birthdate = as.Date(gebdatum, "%Y-%m-%d"),
    days =  birthdate - as.Date("1999-09-24"),
    int_eff = as.numeric((wahrnehmungpol3 - 6) * -1),
    ext_eff = as.numeric(wahrnehmungpol4),
    polint = as.numeric((polint1 - 6) * -1),
    demsat = as.numeric((demzufriedenheit - 6) * -1),
    turnout = as.integer((wahlteilnahme - 2) * -1),
    turnout_ltw = case_when(wave == 1 ~ turnout,
                            TRUE ~ NA_integer_),
    turnout_btw = case_when(wave == 2 ~ turnout,
                           TRUE ~ NA_integer_),
    turnout_kw = case_when(wave == 3 ~ turnout,
                            TRUE ~ NA_integer_),
    recall_turnout_ltw = ((ltwbeteiligung - 2) * -1),
    recall_turnout_btw = ((btwbeteiligung - 2) * -1),
    female = geschlecht - 1,
    age = alter
  )

# Code which waves a participant participated in
ids_wave1 <- df %>% filter(wave == 1) %>% select(id)
ids_wave2 <- df %>% filter(wave == 2) %>% select(id)
ids_wave3 <- df %>% filter(wave == 3) %>% select(id)

df <- df %>% mutate(
  wave1 = id %in% ids_wave1$id,
  wave2 = id %in% ids_wave2$id,
  wave3 = id %in% ids_wave3$id
)
# The above variables have constant values for all ids, i.e., it allows to know for
# wave1 observation, whether the corresponding respondent has also participated
# in later waves

df$migration <-
  ifelse(df$migrationshintergrund == 1, 1, 0) # recode migration to binary

df$treatment <-
  ifelse(df$group == 2, 1, 0) # generate treatment variable
df$treatment_reversed <-
  ifelse(df$treatment == 1, 0, 1) # invert treatment variable

df$education[df$schulform == 1 |
               df$schulabschluss1 == 3 |
               df$schulabschluss2 == 4] <- 3 # summarise education var into one measure
df$education[df$schulabschluss1 == 2 | df$schulabschluss2 == 3] <- 2
df$education[df$schulabschluss1 == 1 | df$schulabschluss2 == 2] <- 1
df$education[df$schulabschluss1 == 2 | df$schulabschluss2 == 3] <- 2
df$education[df$schulabschluss2 == 1] <- 0

# 0 ohne Abschluss
# 1 Erster Abschluss
# 2 Mittlerer Abschluss
# 3 Abitur

df$abitur <-  df$education == 3

# Whether respondent still goes to school
df$in_school <- df$schulform <= 4

# Whether respondent still lives at home with parents
df$at_home <- df$wohnen < 4

# Fill up demographic/turnout vars across waves & keep needed vars only
df <- df %>%
  group_by(id) %>%
    arrange(id, wave) %>%
  fill(female, migration, schulform, turnout_ltw, turnout_btw) %>%
  fill(female,
       migration,
       schulform,
       turnout_btw,
       turnout_kw,
       .direction = "up") %>%
  select(
      id,
      group,
      wave,
      wave1,
      wave2,
      wave3,
      # posttreatment,
      birthdate,
      days,
      treatment,
      treatment_reversed,
      female,
      age,
      in_school,
      education,
      abitur,
      at_home,
      migration,
      kreisfrei,
      turnout,
      turnout_ltw,
      turnout_btw,
      turnout_kw,
      int_eff,
      ext_eff,
      polint,
      demsat
  )

save(df, file = "data/df.RData")

## Subsamples ------------------------------------------------------------------

g123w123 <- df

# Combinations of groups and waves

g12w12 <- df %>% filter(group != 3, wave != 3, wave2 == 1)
g23w12 <- df %>% filter(group != 1, wave != 3, wave2 == 1)
g13w12 <- df %>% filter(group != 2, wave != 3, wave2 == 1)

g12w13 <- df %>% filter(group != 3, wave != 2, wave3 == 1)
g23w13 <- df %>% filter(group != 1, wave != 2, wave3 == 1)

g12w23 <- df %>% filter(group != 3, wave != 1, wave2 == 1, wave3 == 1)
g23w23 <- df %>% filter(group != 1, wave != 1, wave2 == 1, wave3 == 1)

## Matching --------------------------------------------------------------------

set.seed(65557777)

for (subsample in c('g12w12', 'g12w13', 'g23w12', 'g23w13',
                    'g12w23', 'g23w23')) {

  # Matching

  tmp <- get(subsample)

  posttreatmentwave <- as.integer(str_sub(subsample, -1, -1))

  matching <- tmp %>%
    filter(wave == posttreatmentwave) %>%
    select(id, wave, treatment, treatment_reversed,
           female, education, kreisfrei) %>%
    drop_na() %>%
    as.data.frame()

  m_out <-
    matchit(treatment_reversed ~ female + education + kreisfrei,
            data = matching)

  m_data <- match.data(m_out) %>% as_tibble() %>% select(id)

  tmp_m <- tmp %>% inner_join(., m_data, by = 'id')

  assign(x = paste0(subsample, '_m'), value = tmp_m)
  rm(tmp, tmp_m, wave)

  # Love Plot for Balance

  varnames <-
    tibble(
      old = c('female', 'education', 'kreisfrei'),
      new = str_to_title(old),
    ) %>% as.data.frame()

  tmp <- love.plot(bal.tab(m_out), abs = T, line = T, stars = 'std',
                   colors = 'black', shapes = c(15, 19),
                   drop.distance = T, var.names = varnames) +
    labs(caption = '* Standardized Mean Differences') +
    theme_bw(base_size = 14)

  assign(x = paste0('f_', subsample, '_balance'), value = tmp)

  # pdf
  ggsave(filename = paste0('figures/f_', subsample, '_m_balance.pdf'),
         plot = get(paste0('f_', subsample, '_balance')))

  # png
  ggsave(filename = paste0('figures/f_', subsample, '_m_balance.png'),
         plot = get(paste0('f_', subsample, '_balance')))

  rm(tmp)
}
rm(subsample)




## Birthdate windows -----------------------------------------------------------

for (subsample in c('g12w12', 'g12w13', 'g23w12', 'g23w13',
                    'g12w23', 'g23w23')) {
  for (range in c(50, 100, 150, 200, 250, 300)) {
    tmp <- filter(get(subsample), abs(days) <= range)
    assign(paste0(subsample, '_', range), tmp)
  }
  rm(tmp)
}
rm(subsample)