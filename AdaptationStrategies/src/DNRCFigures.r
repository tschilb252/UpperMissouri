#################################################
#' @title Upper Missouri Basin Study - DNRC
#' Figures
#' @author Dan Broman
#' @description Upper Missouri Basin Study
#' Figures from DNRC
#' Last Modified June 21 2018
#################################################
library(tidyverse)
library(data.table)
library(RWDataPlyr)
library(RColorBrewer)
setwd('C:/Projects/git/UpperMissouri/AdaptationStrategies/')
source('src/fncnLib.r')

#################################################
#' User Inputs
# Data Directories
dirInp = 'data/'
dirOup = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

#################################################
# Holter Hydropower
HdroClimHolt = fread(paste0(dirInp, 'HolterHydroClim.csv'))
HdroClimHolt = HdroClimHolt %>%
  mutate(Date = as.Date(Date))
HdroCapHolt = 7100 # Holter hydropower capacity in MwH

ggplot(data = HdroClimHolt) +
  geom_ribbon(aes(x = Date, ymin = Value10, ymax = Value90),
    fill = '#A3ABD3', color = '#25499F', alpha = 0.4) +
  geom_line(aes(x = Date, y = Value50), color = '#152C5F') +
  scale_x_date(date_labels = '%b %d') +
  scale_y_continuous(labels = comma) +
  xlab('') +
  ylab('Streamflow (cfs)') +
  geom_hline(yintercept = HdroCapHolt, color = '#B70B01')  +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.ticks=element_blank(),
    legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()
  )
ggsave(paste0(dirOup, 'HolterHydro.png'), height = 8, width = 10)
