#################################################
#' @title Upper Missouri Basin Study - Increase
#' Irrigation Efficiency Figures
#' @author Dan Broman
#' @description Summary figures for the Upper Missouri
#' Basin Study, Increased Irrigation Efficiency Strategy
#' Last Modified June 14 2018
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
StgyTbl = fread('lib/StrategyTableIncIrrigEffic.csv')
MeasTbl = fread('lib/MeasureTableIncIrrigEffic.csv')

ScenList = c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
  'FBMID', 'FBLDP', 'FBMIP', 'FBLPP')
#################################################
#'  Read in Data

fileList = unique(MeasTbl$File)
stgyList = unique(StgyTbl$Strategy)
ctFiles = nrow(StgyTbl)

# Depletion shortages
fileTmp = fileList[1]
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
  # summarise data to reduce the size
  datTmpDTAgg = datTmpDT %>%
    mutate(Wyear = wyear(Date)) %>%
    dplyr::rename(Slot = RiverWareSlot) %>%
    group_by(Wyear, Trace, ScenarioSet, Strategy, Slot) %>%
    dplyr::summarise(Value = sum(Value))
  datMeas = bind_rows(datMeas, datTmpDTAgg)
}

datMeasAgg = datMeas %>%
  left_join(MeasTbl) %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeasAgg2 = datMeasAgg %>%
  group_by(Measure, Scenario, Period, Strategy, Wyear) %>%
  summarise(Value = sum(Value)) %>%       # sum up shortages by above groups
  ungroup()

datMeasAvg = datMeasAgg %>%
  group_by(Measure, Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeasAvgHist = datMeasAvg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeasAvgFut = datMeasAvg %>%
  left_join(datMeasAvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeasAvgFut = datMeasAvgFut %>% mutate(ValueColScle = ValueChange * -1)

# Clark Canyon allocation fraction (shortages)
fileTmp = fileList[2]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas2 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas2 = bind_rows(datMeas2, datTmpDT)
}

datMeas2Agg = datMeas2 %>%
  mutate(Wyear = wyear(Date)) %>%
  dplyr::rename(Slot = RiverWareSlot) %>%
  dplyr::filter(!is.na(Value))

datMeas2Agg = datMeas2Agg %>%
  left_join(MeasTbl) %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas2Avg = datMeas2Agg %>%
  group_by(Measure, Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas2AvgHist = datMeas2Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeas2AvgFut = datMeas2Avg %>%
  left_join(datMeas2AvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas2AvgFut = datMeas2AvgFut %>% mutate(ValueColScle = ValueChange)

# In-stream flows
fileTmp = fileList[3]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas3 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas3 = bind_rows(datMeas3, datTmpDT)
}

# 7-day low flows
# datMeas3Agg = datMeas3 %>%
#   mutate(Wyear = wyear(Date), DOWY = dowy(Date)) %>%
#   dplyr::rename(Slot = RiverWareSlot) %>%
#   group_by(Wyear, Trace, ScenarioSet, Strategy, Slot) %>%
#   mutate(Value = rollapply(data = Value, width = 7, FUN = mean,
#     align = 'right', fill = NA, na.rm = T)) %>%
#   dplyr::summarise(Value = min(Value, na.rm = T))
#
# datMeas3Agg = datMeas3Agg %>%
#   left_join(MeasTbl) %>%
#   left_join(ScenTbl) %>%
#   filter(Scenario %in% ScenList) %>%
#   mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))
#
# datMeas3Avg = datMeas3Agg %>%
#   group_by(Measure, Scenario, Period, Strategy) %>%
#   summarise(Value = mean(Value)) %>%
#   ungroup()

# August mean flow change
datMeas3Agg = datMeas3 %>%
  mutate(Wyear = wyear(Date), Month = month(Date)) %>%
  dplyr::rename(Slot = RiverWareSlot) %>%
  filter(Month == 8) %>%
  group_by(Wyear, Trace, ScenarioSet, Strategy, Slot) %>%
  dplyr::summarise(Value = mean(Value, na.rm = T))

datMeas3Agg = datMeas3Agg %>%
  left_join(MeasTbl) %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas3Avg = datMeas3Agg %>%
  group_by(Measure, Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas3AvgHist = datMeas3Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeas3AvgFut = datMeas3Avg %>%
  left_join(datMeas3AvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas3AvgFut = datMeas3AvgFut %>% mutate(ValueColScle = ValueChange)

# Hydropower production
fileTmp = fileList[4]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas4 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas4 = bind_rows(datMeas4, datTmpDT)
}

datMeas4 = datMeas4 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario))

datMeas4Agg = datMeas4 %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    group_by(Measure, Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(Value)) %>%
    ungroup()

datMeas4Avg = datMeas4Agg %>%
  group_by(Measure, Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas4AvgHist = datMeas4Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeas4AvgFut = datMeas4Avg %>%
  left_join(datMeas4AvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas4AvgFut = datMeas4AvgFut %>% mutate(ValueColScle = ValueChange)

# Hydropower production (Toston)
fileTmp = fileList[5]
slotListTmp = dplyr::filter(MeasTbl, File == fileTmp)$Slot
datMeas5 = data.table()
for(iterFile in 1:ctFiles){
  filePath = paste0(dirInp, StgyTbl$Directory[iterFile], '/', fileTmp)
  ScenarioSet = StgyTbl$ScenarioSet[iterFile]
  Strategy =  StgyTbl$Strategy[iterFile]
  datTmp = read.rdf(filePath)
  datTmpDT = Rdf2dt(datTmp, slotListTmp)
  datTmpDT$ScenarioSet = ScenarioSet
  datTmpDT$Strategy = Strategy
  datMeas5 = bind_rows(datMeas5, datTmpDT)
}

datMeas5 = datMeas5 %>%
  left_join(ScenTbl) %>%
  filter(Scenario %in% ScenList) %>%
  mutate(Scenario = ifelse(nchar(Scenario) == 5, substr(Scenario, 3,5), Scenario)) %>%
  mutate(Value = TostonEnergy(Value))

datMeas5Agg = datMeas5 %>%
    mutate(WYear = wyear(Date)) %>%         # add water year column
    dplyr::rename(Slot = RiverWareSlot) %>%
    left_join(MeasTbl) %>%
    group_by(Measure, Scenario, Period, Strategy, WYear) %>%   # group by scenario, period, strategy, and water year
    summarise(Value = sum(Value)) %>%
    ungroup()

datMeas5Avg = datMeas5Agg %>%
  group_by(Measure, Scenario, Period, Strategy) %>%
  summarise(Value = mean(Value)) %>%
  ungroup()

datMeas5AvgHist = datMeas5Avg %>%
  filter(Scenario == 'Historical', Strategy == 'Baseline') %>%
  rename(ValueHist = Value) %>%
  dplyr::select(-Scenario, -Period, -Strategy)

datMeas5AvgFut = datMeas5Avg %>%
  left_join(datMeas5AvgHist) %>%
  mutate(ValueChange = (Value - ValueHist) / ValueHist * 100)

datMeas5AvgFut = datMeas5AvgFut %>% mutate(ValueColScle = ValueChange)

# Combine measures and plot
datMeasPlot = bind_rows(datMeasAvgFut, datMeas2AvgFut, datMeas3AvgFut, datMeas4AvgFut, datMeas5AvgFut)
datMeasPlot$Scenario = factor(datMeasPlot$Scenario,
  levels = rev(c('Historical', 'HD', 'HW', 'CT', 'WD', 'WW',
    'MID', 'LDP', 'MIP', 'LPP')))
datMeasPlot = datMeasPlot %>% left_join(StgyTbl)
datMeasPlot$StrategyLab = factor(datMeasPlot$StrategyLab,
  levels = unique(StgyTbl$StrategyLab))

datMeasPlot$Measure = factor(datMeasPlot$Measure ,
  levels = unique(MeasTbl$Measure))

# Plot defs
pctLow = 5
pctHigh = 100
colPal = c('#DA4325', '#ECA14E', '#F4F3EB', '#5CC3AF', '#0A6265')

datMeasPlot = datMeasPlot %>%
  mutate(ValueTxt = ifelse(abs(ValueColScle ) > pctHigh, '•', '')) %>%
  mutate(ValueColScle = ifelse(abs(ValueColScle) < pctLow, 0,
  ifelse(ValueColScle > pctHigh, pctHigh,
    ifelse(ValueColScle < -1 * pctHigh, -1 * pctHigh, ValueColScle))))

datMeasPlotFl = datMeasPlot %>%
  filter(Period %in% c('2050s', 'Historical') | is.na(Period))

ggplot(data = datMeasPlotFl, aes(x = Measure, y = Scenario,
  fill = ValueColScle, label = ValueTxt)) +
  geom_tile(colour = 'white', size = 1) +
  geom_text(size = 4, colour = 'white') +
  facet_wrap(~StrategyLab, ncol = 1, strip.position="left", labeller = label_wrap_gen(width=20)) +
  scale_fill_gradientn(colors = colPal, limits = c(-pctHigh, pctHigh)) +
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
    axis.title.y=element_blank(),legend.position="none",
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

ggsave(paste0(dirOup, 'IncIrrigEfficGrid.png'), height = 10, width = 8)
write.csv(datMeasPlot, paste0(dirOup, 'IncIrrigEfficGrid.csv'),
  row.names = F, quote = F)
