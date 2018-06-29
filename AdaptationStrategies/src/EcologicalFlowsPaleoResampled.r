#################################################
#' @title Upper Missouri Basin Study - Ecological Flows
#' Figures
#' @author Dan Broman
#' @description Bar figures for the Upper Missouri
#' Basin Study, Ecological Flows (Ecological Flows) Strategy
#' Last Modified June 29 2018
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

# CFRAnnFlushingRelease
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

datMeasAgg3$Scenario = factor(datMeasAgg3$Scenario,
    levels = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW'))

# Plots Canyon Ferry Years with Releases
datMeasPlotFl = datMeasAgg3 %>% filter(Period %in% c('Historical', '2050s'))

ggplot() +
  geom_linerange(data = datMeasPlotFl, aes(x = Scenario,
    ymin = ValueMin,
    ymax = ValueMax),
    position = position_dodge(width = 1),
    alpha = 0.8, size = 10) +
  	theme_bw() +
  	theme(legend.position = 'bottom',
  		legend.title = element_blank(),
  		axis.text = element_text(size = 12),
  		strip.background = element_rect(fill = F),
  		strip.text = element_text(size = 15)) +
  	xlab('') +
  	ylab('')
