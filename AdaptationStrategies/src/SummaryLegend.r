#################################################
#' @title Upper Missouri Basin Study - Summary
#'        Legend
#' @author Dan Broman
#' @description Plots and saves summary figure
#' color bar legend
#' Last Modified June 13 2018
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
dirInp = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/Data/'
dirOup = 'T:/PlanningOperations/Staff/DBROMAN/UMBIA/AdaptationStrategies/Figures/'

#################################################

tmpData = data.table(Measure = 'MeasureA',
  Scenario = 'Historical',
  ValueColScle = c(-100, 100),
  ValueTxt = c('', ''),
  StrategyLab = c('Baseline', 'StrategyA'))

tmpPlotHorz = ggplot(data = tmpData, aes(x = Measure, y = Scenario,
  fill = ValueColScle, label = ValueTxt)) +
  geom_tile(colour = 'white', size = 1) +
  geom_text(size = 4, colour = 'white') +
  facet_wrap(~StrategyLab, ncol = 1, strip.position="left", labeller = label_wrap_gen(width=20)) +
  scale_fill_gradientn(colors = colPal, limits = c(-pctHigh, pctHigh),
    breaks = c(-100, -50, 0, 50, 100), labels = paste0(c(-100, -50, 0, 50, 100), '%')) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0), position="top") +
  scale_y_discrete(expand=c(0,0), position="right") +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.title=element_blank(),
    legend.justification='center',
    legend.text=element_text(hjust = 0, vjust = 0.5, size = 10),
    legend.key.width=unit(3, "cm"),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  ) +
    coord_equal()

tmpPlotVert = ggplot(data = tmpData, aes(x = Measure, y = Scenario,
  fill = ValueColScle, label = ValueTxt)) +
  geom_tile(colour = 'white', size = 1) +
  geom_text(size = 4, colour = 'white') +
  facet_wrap(~StrategyLab, ncol = 1, strip.position="left", labeller = label_wrap_gen(width=20)) +
  scale_fill_gradientn(colors = colPal, limits = c(-pctHigh, pctHigh),
    breaks = c(-100, -50, 0, 50, 100), labels = paste0(c(-100, -50, 0, 50, 100), '%')) +
  xlab('') +
  ylab('') +
  scale_x_discrete(expand=c(0,0), position="top") +
  scale_y_discrete(expand=c(0,0), position="right") +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="right",
    legend.title=element_blank(),
    legend.justification='center',
    legend.text=element_text(hjust = 0, vjust = 0.5, size = 10),
    legend.key.height=unit(3, "cm"),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  ) +
    coord_equal()

tmpLegendVert = g_legend(tmpPlotVert)
ggsave(plot = tmpLegendVert, paste0(dirOup, 'SummaryLegendVertical.png'), height = 6, width = 1)

tmpLegendHorz = g_legend(tmpPlotHorz)
ggsave(plot = tmpLegendHorz, paste0(dirOup, 'SummaryLegendHorizontal.png'), height = 1, width = 6)
