# Plot a timeline of birthdates for election eligibility

library(stringr)
library(lubridate)
library(ggplot2)
library(extrafont)


df <- read.csv('data/gruppen.csv', stringsAsFactors = F)

df$begin <- as.Date(df$begin)
df$end <- as.Date(df$end)

df$label <- str_replace(df$label, 'Gruppe', 'Group')

brks <- sort(c(df$end, as.Date(c('1998-01-01', '1999-01-01', '2000-01-01',
                               '2001-01-01', '2002-01-01'))))

lbls <- str_replace(brks, '-01-01', '')

lbls[which(nchar(lbls) == 4)] <- paste('\n', lbls[which(nchar(lbls) == 4)])

lbls <- str_replace_all(lbls, '-', '\u00AD')

g_timeline <-
  ggplot(df, aes(x = end, y = y)) +
  # geom_rect(aes(xmin = begin, xmax = end, ymin = 0, ymax = 1,
  #               fill = as.factor(gruppe)), alpha = .5,
            # show.legend = F) +
  geom_rect(aes(xmin = as.Date('1998-11-17'), xmax = as.Date('2002-03-06'),
                ymin = 0, ymax = .5),
            fill = 'darkgrey', color = 'black', alpha = .5) +
  geom_text(aes(x = as.Date('1998-11-17') + days(30), y = .45,
                label = 'Target population'), hjust = 0, check_overlap = T) +
  # geom_rect(aes(xmin = as.Date('1999-01-17'), xmax = as.Date('2000-05-17'),
  #               ymin = 0, ymax = .75), color = 'cornsilk3', alpha = .3) +
  # geom_text(aes(x = as.Date('1999-01-17'), y = .7, label = 'Option 2'), hjust = 0,
  #           check_overlap = T) +
  # geom_rect(aes(xmin = as.Date('2000-09-07'), xmax = as.Date('2002-01-07'),
  #               ymin = 0, ymax = .75), color = 'cornsilk3', alpha = .3) +
  # geom_text(aes(x = as.Date('2000-09-07'), y = .7, label = 'Option 2'), hjust = 0,
  #           check_overlap = T) +
  geom_vline(aes(xintercept = as.numeric(end)), linetype = 'dashed') +
  geom_label(aes(label = label, y = y - .1), hjust = 1) +
  scale_x_date(limits = c(min(df$begin), as.Date('2002-05-31')),
               breaks = brks, labels = lbls) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = c(0, 0)) +
  xlab('') + ylab('') +
  theme_bw() + theme(panel.grid.minor.x = element_blank())

pdf('figures/g_timeline.pdf', width = 6, height = 3)
g_timeline
dev.off()