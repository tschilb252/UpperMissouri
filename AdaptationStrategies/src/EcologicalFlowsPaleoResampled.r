#################################################
#' @title Upper Missouri Basin Study - Ecological Flows
#' Figures
#' @author Dan Broman
#' @description Bar figures for the Upper Missouri
#' Basin Study, Ecological Flows (Ecological Flows) Strategy
#' Last Modified July 11 2018
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
StgyTbl = fread('lib/StrategyTableEcologicalFlowsPaleoResampled.csv')
MeasTbl = fread('lib/MeasureTableEcologicalFlowsPaleoResampled.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW', 'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# cFRFlushingRelease
fileTmp = fileList[1]
datMeas = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas = bind_rows(datMeas, datTmpDT)
}

datMeasAgg = datMeas %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario)) %>%
    mutate(Wyear = wyear(Date), Month = month(Date)) %>%
    mutate(ValueFlag = ifelse(Value > 0, 1, 0)) %>%
    dplyr::rename(Slot = RiverWareSlot) %>%
    group_by(Wyear, Trace, ScenarioSet, Strategy, Scenario, Slot, Period) %>%
    dplyr::summarise(Value = sum(ValueFlag, na.rm = T))

datMeasAgg2 = datMeasAgg %>%
  mutate(ValueFlag = ifelse(Value > 0, 1, 0)) %>%
  group_by(Trace, ScenarioSet, Strategy, Scenario, Slot, Period) %>%
  dplyr::summarise(Value = sum(ValueFlag, na.rm = T))

datMeasAgg3 = datMeasAgg2 %>%
  group_by(Slot, Period, Scenario) %>%
  dplyr::summarise(ValueMin = min(Value), ValueMax = max(Value), ValueMedian = median(Value))

datMeasComp = datMeasAgg %>%
  mutate(CFValueFlag = ifelse(Value > 0, 1, 0)) %>%
  ungroup() %>%
  dplyr::select(Wyear, Trace, ScenarioSet, Scenario, Strategy, Period, CFValueFlag)

# TiberFlushingRelease
fileTmp = fileList[2]
datMeas2 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas2 = bind_rows(datMeas2, datTmpDT)
}

datMeas2Agg = datMeas2 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario)) %>%
    mutate(Wyear = wyear(Date), Month = month(Date)) %>%
    mutate(ValueFlag = ifelse(Value > 0, 1, 0)) %>%
    dplyr::rename(Slot = RiverWareSlot) %>%
    group_by(Wyear, Trace, ScenarioSet, Strategy, Scenario, Slot, Period) %>%
    dplyr::summarise(Value = sum(ValueFlag, na.rm = T))

datMeas2Agg2 = datMeas2Agg %>%
  mutate(ValueFlag = ifelse(Value > 0, 1, 0)) %>%
  group_by(Trace, ScenarioSet, Strategy, Scenario, Slot, Period) %>%
  dplyr::summarise(Value = sum(ValueFlag, na.rm = T))

datMeas2Agg3 = datMeas2Agg2 %>%
  group_by(Slot, Period, Scenario) %>%
  dplyr::summarise(ValueMin = min(Value), ValueMax = max(Value), ValueMedian = median(Value))

datMeas2Comp = datMeas2Agg %>%
  mutate(TiberValueFlag = ifelse(Value > 0, 1, 0)) %>%
  ungroup() %>%
  dplyr::select(Wyear, Trace, ScenarioSet, Strategy, Scenario, Period, TiberValueFlag)

datMeas3 = left_join(datMeasComp, datMeas2Comp)

datMeas3Agg = datMeas3 %>%
    mutate(ValueFlag = ifelse(CFValueFlag > 0 & TiberValueFlag > 0, 1, 0)) %>%
    group_by(Trace, ScenarioSet, Strategy, Scenario, Period) %>%
    dplyr::summarise(Value = sum(ValueFlag, na.rm = T))

datMeas3Agg2 = datMeas3Agg %>%
  group_by(Period, Scenario) %>%
  dplyr::summarise(ValueMin = min(Value), ValueMax = max(Value), ValueMedian = median(Value))
datMeas3Agg2$Measure = 'Combined Releases'

datMeasPlot = bind_rows(datMeasAgg3, datMeas2Agg3)

datMeasPlot = datMeasPlot %>% left_join(MeasTbl)

datMeasPlot = datMeasPlot %>% bind_rows(datMeas3Agg2)

xAxsTbl = data.table(Scenario = rev(c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW')), ScenBreaks = 1:6)
datMeasPlot = datMeasPlot %>% left_join(xAxsTbl)

datMeasPlot$Scenario = factor(datMeasPlot$Scenario,
    levels = rev(c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW')))

datMeasPlot$Measure = factor(datMeasPlot$Measure,
  levels = c('Canyon Ferry Releases', 'Tiber Releases', 'Combined Releases'))

# datMeasPlot$Measure = factor(datMeasPlot$Measure,
#   levels = c('Combined Releases', 'Tiber Releases', 'Canyon Ferry Releases'))
# Pull out Historical Baseline to plot as line

datMeasPlotHist = data.table(Measure = c('Canyon Ferry Releases', 'Tiber Releases', 'Combined Releases'), Scenario = 'Historical', Value = c(6,10,1))
datMeasPlotHist$Measure = factor(datMeasPlotHist$Measure,
  levels = c('Canyon Ferry Releases', 'Tiber Releases', 'Combined Releases'))

datMeasPlotFl = datMeasPlot %>% filter(Period %in% c('Historical', '2050s'))

ggplot(data = datMeasPlotFl) +
  geom_linerange(aes(x = Scenario,
    ymin = ValueMin,
    ymax = ValueMax,
    colour = Measure),
    position = position_dodge(width = 1),
    alpha = 0.8, size = 10) +
    geom_point(data = datMeasPlotHist, aes(x = Scenario, y = Value), shape = '|', size = 8) +
    scale_colour_manual(values = c('#24449B', '#119B8B', 'black'), guide = F) +
  	theme_bw() +
    facet_wrap(~Measure) +
  	theme(legend.position = 'bottom',
  		legend.title = element_blank(),
  		axis.text = element_text(size = 12),
  		strip.background = element_rect(fill = F),
  		strip.text = element_text(size = 15)) +
  	xlab('') +
  	ylab('No. Releases') +
    coord_flip()

ggsave(paste0(dirOup, 'EcologicalFlowReleasesBar.png'), height = 3, width = 8)
