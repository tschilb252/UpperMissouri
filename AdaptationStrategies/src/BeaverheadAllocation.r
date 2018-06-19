#################################################
#' @title Upper Missouri Basin Study - Beaverhead
#'        Basin Figures
#' @author Dan Broman
#' @description Figures for the Upper Missouri
#' Basin Study, Beaverhead River Basin
#' Clark Canyon Water Users Allocation Fraction
#' Last Modified June 12 2018
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

# LookUp Table Locations
ScenTbl = fread('lib/ScenarioTable.csv')
StgyTbl = fread('lib/StrategyTableBeaverhead.csv')
MeasTbl = fread('lib/MeasureTableBeaverhead.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')

dateStrt = as.Date('1986-10-01')
dateEnd = as.Date('1995-09-30')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# Clark Canyon users allocation fraction (shortage)
fileTmp = fileList[3]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas = bind_rows(datMeas, datTmpDT)
}

datMeas = datMeas %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeasAgg = datMeas %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    filter(!is.na(Value)) %>%
    left_join(StgyTbl)

datMeasAgg$StrategyLab = factor(datMeasAgg$StrategyLab,
      levels = unique(StgyTbl$StrategyLab))

datMeasPlot = datMeasAgg %>%
  filter(Period %in% c('2050s', 'Historical'), Date >= dateStrt, Date <= dateEnd) %>%
  mutate(Year = year(Date)) %>%
  filter(Scenario == 'Historical')

datMeasPlot1 = datMeasPlot %>% filter(Measure == 'CCWSC Allocation')

Plot1 = ggplot(data = datMeasPlot1) +
  # geom_ribbon(aes(x = Year, ymin = 0.75, ymax = 0.95), fill = '#FDEE91', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0.5, ymax = 0.75), fill = '#FCCA8B', alpha = 0.3) +
  # geom_ribbon(aes(x = Year, ymin = 0, ymax = 0.5), fill = '#DC8C6B', alpha = 0.3) +
  geom_hline(yintercept = c(0.77, 0.5, 0.25), alpha = 0.6, linetype = 3, size = 0.2) +
  geom_line(aes(x = Year, y = Value, colour = StrategyLab, linetype = StrategyLab), alpha = 0.8) +
  geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab), size = 2, alpha = 0.8) +
  facet_wrap(~Measure, ncol = 1) +
  scale_colour_manual(values = c('black', '#25499F', '#23A491', '#7A5BA9')) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = paste0(c(0, 25, 50, 75, 100), '%')) +
  scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
  xlab('') +
  ylab('') +
  theme(
    axis.line.x=element_line(size=0.5, colour = 'gray60'),
    axis.line.y=element_line(size=0.5, colour = 'gray60'),
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="NA",
    legend.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    # panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    strip.background = element_blank(),
    strip.text.x=element_text(size = 10),
    strip.text.y=element_text(size = 10)
  )

datMeasPlot2 = datMeasPlot %>% filter(Measure == 'East Bench Allocation')
Plot2 = ggplot(data = datMeasPlot2) +
    # geom_ribbon(aes(x = Year, ymin = 0.75, ymax = 0.95), fill = '#FDEE91', alpha = 0.3) +
    # geom_ribbon(aes(x = Year, ymin = 0.5, ymax = 0.75), fill = '#FCCA8B', alpha = 0.3) +
    # geom_ribbon(aes(x = Year, ymin = 0, ymax = 0.5), fill = '#DC8C6B', alpha = 0.3) +
    geom_hline(yintercept = c(0.77, 0.5, 0.25), alpha = 0.6, linetype = 3, size = 0.2) +
    geom_line(aes(x = Year, y = Value, colour = StrategyLab, linetype = StrategyLab), alpha = 0.8) +
    geom_point(aes(x = Year, y = Value, colour = StrategyLab, shape = StrategyLab), size = 2, alpha = 0.8) +
    facet_wrap(~Measure, ncol = 1) +
    scale_colour_manual(values = c('black', '#25499F', '#23A491', '#7A5BA9')) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c('Bank', paste0(c(25, 50, 75, 100), '%'))) +
    scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
    xlab('') +
    ylab('') +
    theme(
      axis.line.x=element_line(size=0.5, colour = 'gray60'),
      axis.line.y=element_line(size=0.5, colour = 'gray60'),
      axis.line=element_blank(),
      axis.text.x=element_text(angle = 90, hjust = 0, vjust = 0.5, size = 10),
      axis.text.y=element_text(hjust = 0, vjust = 0.5, size = 10),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="bottom",
      legend.title=element_blank(),
      legend.text=element_text(size = 10),
      panel.background=element_blank(),
      panel.border=element_blank(),
      # panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank(),
      strip.background = element_blank(),
      strip.text.x=element_text(size = 10),
      strip.text.y=element_text(size = 10)
    )

Plot3 = grid.arrange(Plot1, Plot2, ncol = 1)
ggsave(plot = Plot3, paste0(dirOup, 'BeaverheadAllocation.png'), height = 8, width = 10)
