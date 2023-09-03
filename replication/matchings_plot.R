library(ggplot2)
library(ggdist)

source('code/functions.R')
source2('code/data.R', 63,288) # this does the data prep for us. 
rm(list=setdiff(ls(), c("g12w12", "g12w23", "g12w13")))

## Fixed Presets ---------------------------------------------------------------

dvs <- c('ext_eff', 'demsat', 'int_eff', 'polint')
dv_names <- c('External Efficacy', 'Satisfaction with Democracy',
              'Internal Efficacy', 'Political Interest')
names(dv_names) <- dvs

subgroups <- c('all', 'v', 'nv')

## Difference-in-Differeneces --------------------------------------------------

# Create datasets with Matching
library(WeightIt)
covs <- formula(treatment ~ female + education + kreisfrei)

## CEM
g12w12_cem_ <- matchit(covs, data = g12w12%>%na.omit(), method = "cem")
g12w23_cem_ <- matchit(covs, data = g12w23%>%na.omit(), method = "cem")
g12w13_cem_ <- matchit(covs, data = g12w13%>%na.omit(), method = "cem")

g12w12_cem <- match.data(g12w12_cem_)%>%mutate(weights=as.numeric(weights))
g12w23_cem <- match.data(g12w23_cem_)%>%mutate(weights=as.numeric(weights))
g12w13_cem <- match.data(g12w13_cem_)%>%mutate(weights=as.numeric(weights))

## EB
g12w12_eb <- na.omit(g12w12)
g12w12_eb$weights <- weightit(covs, data = g12w12_eb, method="ebal")$weights

g12w23_eb <- na.omit(g12w23)
g12w23_eb$weights <- weightit(covs, data = g12w23_eb, method="ebal")$weights

g12w13_eb <- na.omit(g12w13)
g12w13_eb$weights <- weightit(covs, data = g12w13_eb, method="ebal")$weights

# Model estimation

datasets <- c('g12w12', 'g12w23', 'g12w13',
              'g12w12_eb', 'g12w23_eb', 'g12w13_eb',
              'g12w12_cem', 'g12w23_cem', 'g12w13_cem'
              ) # Main results: Losing, gaining, and net effect

for (dataset in datasets) {
  
  df <- as.data.frame(get(dataset))
  
  models <- list()
  for(dv in dvs) {
    for(subgroup in subgroups) {
      
      eq <- as.formula(paste(dv, 'treatment * posttreatment', sep = '~'))
      
      print(eq)
      print(paste(dataset, ',', subgroup))
      
      if(subgroup == 'all') {
        tmp <- df
      } else if(subgroup == 'v') {
        tmp <- filter(df, turnout_ltw == 1)
      } else if (subgroup == 'nv') {
        tmp <- filter(df, turnout_ltw == 0)
      } else{
        cat('Error!')
        stop()
      }
      
      if (str_extract(dataset, '(?<=w)\\d\\d') == '12') {
        tmp$posttreatment <- tmp$wave == 2
      } else if(str_extract(dataset, '(?<=w)\\d\\d') == '13') {
        tmp$posttreatment <- tmp$wave == 3
      } else if(str_extract(dataset, '(?<=w)\\d\\d') == '23') {
        tmp$posttreatment <- tmp$wave == 3
      } else stop()
      
      if (str_detect(dataset, '_eb')) {
        m <- lm(formula = eq, data = tmp, weights = weights)
      } else {
        m <- lm(formula = eq, data = tmp)
      }
      
      assign(x = paste('m', dv, subgroup, sep = '_'), value = m)
    }
  }
  
  # Prepare results for plotting
  
  model_df <- data_frame()
  for(dv in dvs) {
    for(subgroup in subgroups) {
      model <- get(paste('m', dv, subgroup, sep = '_'))
      
      tmp <- tidy(model, conf.int = T, conf.level = .9) %>%
        filter(term == 'treatment:posttreatmentTRUE') %>%
        mutate(coefficient = term,
               dv = dv,
               term = factor(dv,
                             levels = dvs,
                             labels = dv_names),
               subgroup = subgroup,
               model = factor(subgroup,
                              levels = c("all", "v", "nv"),
                              labels = c("All", "Voters", "Non-Voters")),
               significant = p.value <= .05) %>%
        rename(ci90l = conf.low,
               ci90h = conf.high)
      
      model_df <- bind_rows(model_df, tmp)
    }
  }
  rm(tmp)
  
  assign(paste0('model_df_', dataset), model_df)
}

models_df <- bind_rows(mutate(model_df_g12w12, matching="base", effect = 'Losing eligiblity'),
                       mutate(model_df_g12w23, matching="base", effect = 'Regaining eligibility'),
                       mutate(model_df_g12w13, matching="base", effect = 'Net effect of temporary\n disenfranchisement'),
                       mutate(model_df_g12w12_cem, matching="cem", effect = 'Losing eligiblity'),
                       mutate(model_df_g12w23_cem, matching="cem", effect = 'Regaining eligibility'),
                       mutate(model_df_g12w13_cem, matching="cem", effect = 'Net effect of temporary\n disenfranchisement'),
                       mutate(model_df_g12w12_eb, matching="eb", effect = 'Losing eligiblity'),
                       mutate(model_df_g12w23_eb, matching="eb", effect = 'Regaining eligibility'),
                       mutate(model_df_g12w13_eb, matching="eb", effect = 'Net effect of temporary\n disenfranchisement')
                       ) %>%
  mutate(effect = factor(effect, levels = c('Losing eligiblity',
                                            'Regaining eligibility',
                                            'Net effect of temporary\n disenfranchisement'),
                         ordered = T),
         term= factor(term, levels=))

# Create coefficient plot with all three models
sbgrp <- 'all'

new_coef <-
  models_df %>% filter(subgroup == sbgrp) %>%
  dwplot(
    .,
    vline = geom_vline(
      xintercept = 0,
      colour = "grey60",
      linetype = 2
    ),
    dot_args = list(size = 4),
    line_args = list(size = 1)) +
  geom_errorbarh(aes(y = term, xmin = ci90l, xmax = ci90h, col=matching),
                 position = position_dodge(width = 0.2),
                 height = 0, size = 1.5) +
  theme_bw(base_size = 16) + xlab("Coefficient Estimate") + ylab("") +
  #scale_colour_grey(start = .1,
  #                  end = .1,
  #                  guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = c(-.2, 0, .2),
                     labels = c('\u00AD0.2', '0.0', '0.2')) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        panel.grid.minor= element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = 'bold')) +
  facet_wrap(~effect, nrow = 1)

# Filter the data based on 'sbgrp'
new_coef <- models_df %>%
  filter(subgroup == sbgrp) %>%
  mutate(term = factor(term, levels = c("Political Interest", "Internal Efficacy", "Satisfaction with Democracy", "External Efficacy")))%>%
  ggplot(aes(x = estimate, y=term, col=matching, fill=matching)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbarh(position = position_dodge(width = 0.2), aes(y = term, xmin = ci90l, xmax = ci90h, col=matching), height = 0, size = 1) +
  theme_bw(base_size = 16) +
  xlab("Coefficient Estimate") +
  ylab("") +
  #scale_colour_grey(start = 0.1, end = 0.1, guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks = c(-0.2, 0, 0.2), labels = c('\u00AD0.2', '0.0', '0.2')) +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold')
  ) +
  facet_wrap(~effect, nrow = 1)

# Print the plot
print(new_coef)


